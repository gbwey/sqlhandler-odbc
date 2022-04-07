{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : HSql.ODBC.DBPG
Description : Postgres
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

Implementation of GConn for postgres.
-}
module HSql.ODBC.DBPG where

import Control.Arrow ((&&&))
import Data.Text (Text)
import qualified Data.Text as T
import Database.Postgres
import HSql.Core.Sql
import HSql.ODBC.GConn
import qualified Language.Haskell.TH.Syntax as TH (lift, runIO)
import Primus.Error
import Text.Shakespeare.Text (st)
import Prelude hiding (FilePath)

-- | writeable instance for a postgres database
type instance WriteableDB (DBPG Writeable) = 'True

instance GConn (DBPG a) where
  loadConnTH _ k = do
    c <- TH.runIO $ loadConn @(DBPG a) k
    TH.lift c

  getAllTablesCountSql _ = Just getPGTableCountsSql

  getAllTablesSql db =
    mkSql
      "getAllTablesSql PostGres"
      [st|
    select schemaname || '.' || tablename
    from pg_catalog.pg_tables
    where schemaname #{pgSchemaDBSql db}
    order by schemaname, tablename
  |]
  getAllViewsSql db =
    mkSql
      "getAllViewsSql PostGres"
      [st|
    select schemaname || '.' || viewname
    from pg_catalog.pg_views
    where schemaname #{pgSchemaDBSql db}
    order by schemaname, viewname
  |]
  existsTableSql db t =
    let cl = maybe mempty (\s -> [st|AND n.nspname = '#{s}'|]) (getEffectiveSchema db t)
     in mkSql
          "existsTableSql PostGres"
          [st|
SELECT case when EXISTS (
    SELECT 1
    FROM   pg_catalog.pg_class c
    JOIN   pg_catalog.pg_namespace n ON n.oid = c.relnamespace
    WHERE  1=1
    #{cl} -- AND n.nspname = 'public'
    AND    c.relname = '#{tName t}'
    AND    c.relkind = 'r'    -- only tables(?)
) then '#{found}'
else '#{notfound}' end
|]
  dropTableIfExistsSql db table = mkSql "dropTableIfExistsSql PostGres" [st|drop table if exists #{getEffectiveTable db table}|]
  dropViewIfExistsSql db table = mkSql "dropViewIfExistsSql PostGres" [st|drop view if exists #{getEffectiveTable db table}|]

  getColumnMetaSql db t = (pgType, getPGColumnMetaSql db t)

  translateColumnMeta _ (cd, ColumnMeta{..}) =
    case cd of
      CString -> [st|varchar(#{cLength})|]
      CFixedString -> [st|char(#{cLength})|]
      CInt -> "bigint" -- "integer"
      CDateTime -> "timestamp"
      CDate -> "date"
      CFloat -> "numeric"
      CBool -> "boolean"
      CBinary -> normalError "CBinary postgres not defined yet"
      CBLOB -> "bytea"
      CCLOB -> "text"
      COther o -> normalError $ "translateColumnMeta: dont know how to convert this columnmeta to postgres " ++ show o
  limitSql _ = maybe mempty (\n -> [st|limit #{n}|])

-- | convert from postgres datatype to 'ColDataType'
pgType :: ColumnMeta -> ColDataType
pgType (T.toLower . cType &&& cScale -> (ss, mscale))
  | ss == "numeric" && mscale == Just 0 = CInt
  | ss `elem` ["varchar", "character varying"] = CString
  | ss `elem` ["char", "bpchar", "character"] = CFixedString
  | ss `elem` ["smallserial", "serial", "bigserial", "smallint", "int", "integer", "bigint", "int4", "int8"] = CInt
  | ss `elem` ["real", "float", "float8", "double", "numeric"] = CFloat
  | ss `elem` ["bit", "bool", "boolean"] = CBool
  | ss `elem` ["timestamp", "timestamp without time zone", "timestamp with time zone"] = CDateTime
  | ss == "date" = CDate
  | ss `elem` [] = CBinary
  | ss == "bytea" = CBLOB
  | ss == "text" = CCLOB
  | otherwise = COther ss

-- | get the database schema from the connection
pgSchemaDBSql :: DBPG a -> Text
pgSchemaDBSql = pgSchemaSql' . getSchema

-- | get the effective database schema from the table and connection
pgSchemaTableSql :: DBPG a -> Table (DBPG a) -> Text
pgSchemaTableSql db = pgSchemaSql' . getEffectiveSchema db

-- | get schema
pgSchemaSql' :: Maybe Text -> Text
pgSchemaSql' =
  \case
    Nothing -> " NOT IN ('pg_catalog', 'information_schema')"
    Just sch -> [st| = '#{sch}'|]

-- https://dba.stackexchange.com/questions/90555/postgresql-select-primary-key-as-serial-or-bigserial/90567#90567

-- | sql for getting metadata from a postgres table
getPGColumnMetaSql :: DBPG a -> Table (DBPG a) -> Sql (DBPG a) '[] '[Sel ColumnMeta]
getPGColumnMetaSql db t =
  mkSql
    "getPGColumnMetaSql"
    [st|
SELECT
      tab_columns.column_name
    , udt_name
    , case when is_nullable='YES' then 1 else 0 end as isnullable
    , case when character_maximum_length is null then 2000 else character_maximum_length end as char_maxlen
    , numeric_precision
    , numeric_scale
    , 0 as iscomputed
    , case when W.data_type in ('smallserial','serial','bigserial') then 1 else 0 end as is_identity
    , case when tab_constraints.constraint_type='PRIMARY KEY' then 1 else 0 end
FROM information_schema.columns AS tab_columns
inner join (

SELECT a.attrelid::regclass::text, a.attname
     , CASE WHEN a.atttypid = ANY ('{int,int8,int2}'::regtype[])
          AND EXISTS (
             SELECT 1 FROM pg_attrdef ad
             WHERE  ad.adrelid = a.attrelid
             AND    ad.adnum   = a.attnum
             AND    ad.adsrc = 'nextval('''
                || (pg_get_serial_sequence (a.attrelid::regclass::text
                                          , a.attname))::regclass
                || '''::regclass)'
             )
        THEN CASE a.atttypid
                WHEN 'int'::regtype  THEN 'serial'
                WHEN 'int8'::regtype THEN 'bigserial'
                WHEN 'int2'::regtype THEN 'smallserial'
             END
        ELSE format_type(a.atttypid, a.atttypmod)
        END AS data_type
FROM   pg_attribute  a
WHERE  a.attrelid = '#{tName t}'::regclass
AND    a.attnum > 0
AND    NOT a.attisdropped
) W on W.attname=tab_columns.column_name


LEFT OUTER JOIN
information_schema.constraint_column_usage AS col_constraints
ON tab_columns.table_name = col_constraints.table_name AND
tab_columns.column_name = col_constraints.column_name
LEFT OUTER JOIN
information_schema.table_constraints AS tab_constraints
ON tab_constraints.constraint_name = col_constraints.constraint_name
--LEFT OUTER JOIN
--information_schema.check_constraints AS col_check_constraints
--ON col_check_constraints.constraint_name = tab_constraints.constraint_name
WHERE tab_columns.table_name = '#{tName t}'
and   tab_columns.table_schema #{pgSchemaTableSql db t}
and (tab_constraints.table_name is null or tab_constraints.table_name = tab_columns.table_name)
ORDER BY ordinal_position
|]

-- https://stackoverflow.com/questions/769683/show-tables-in-postgresql
-- change out 'public' to the schema you want

-- | get all tables and associated row counts
getPGTableCountsSql :: Sql (DBPG a) '[] '[Sel (GetAllTablesCount (DBPG a))]
getPGTableCountsSql =
  mkSql
    "getPGTableCountsSql"
    [st|
select table_schema || '.' || table_name as table
       ,(xpath('/row/cnt/text()', xml_count))[1]::text::int as row_count
       ,null
       ,null
from (
  select table_name, table_schema,
         query_to_xml(format('select count(*) as cnt from %I.%I', table_schema, table_name), false, true, '') as xml_count
  from information_schema.tables
  where table_schema = 'public'
) t
order by table_schema, table_name
 |]
