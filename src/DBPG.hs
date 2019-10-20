-- using getPGTableCountsSql instead for counts of tables cos no roundtripping needed and is fast!
-- multiple resultsets work great with postgres
{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TypeApplications #-}
{- |
Module      : DBPG
Description : Postgres
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
Maintainer  : gbwey9@gmail.com

Implementation of GConn for postgres.
-}
module DBPG where
import Prelude hiding (FilePath)
import Text.Shakespeare.Text
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (fromText)
import GConn
import Sql
import Control.Arrow
import GHC.Generics (Generic)
import Control.Lens.TH
import Language.Haskell.TH.Syntax
import Dhall hiding (maybe,string,map)
import Logging
import qualified Language.Haskell.TH.Syntax as TH

data DBPG a = DBPG {
                     _pgdriverdsn :: !Text
                   , _pgserver :: !Text
                   , _pgschema :: Maybe Text
                   , _pguid :: !Text
                   , _pgpwd :: !Secret
                   , _pgdb :: !Text
                   , _pgport :: !(Maybe Natural)
                   } deriving (TH.Lift, Show, Generic)

makeLenses ''DBPG

instance FromDhall (DBPG a) where
  autoWith i = genericAutoZ i { fieldModifier = T.drop 3 }

type instance WriteableDB (DBPG Writeable) = 'True

instance ToText (DBPG a) where
  toText x = fromText $ maybe "" (<> ".") (_pgschema x) <> _pgdb x

instance GConn (DBPG a) where
  loadConnTH _ k = do
    c <- runIO $ loadConn @(DBPG a) k
    TH.lift c

  connText DBPG {..} = [st|#{_pgdriverdsn};Server=#{_pgserver};Port=#{maybe "5432" show _pgport};Database=#{_pgdb};Uid=#{_pguid};Pwd=#{unSecret _pgpwd};|]
  connCSharpText = undefined
  showDb DBPG {..} = [st|postgres ip=#{_pgserver} db=#{_pgdb}|]
  getSchema = _pgschema -- not sure how to specify the schema for postgres odbc
  getDb = Just . _pgdb
  getDelims _ = Just ('\"','\"')
  getAllTablesCountSql _ = Just getPGTableCountsSql

  getAllTablesSql db = mkSql "getAllTablesSql PostGres" [st|
    select schemaname || '.' || tablename
    from pg_catalog.pg_tables
    where schemaname #{pgSchemaDBSql db}
    order by schemaname, tablename
  |]
  getAllViewsSql db = mkSql "getAllViewsSql PostGres" [st|
    select schemaname || '.' || viewname
    from pg_catalog.pg_views
    where schemaname #{pgSchemaDBSql db}
    order by schemaname, viewname
  |]
  existsTableSql db t =
    let cl = maybe mempty (\s -> [st|AND n.nspname = '#{s}'|]) (getEffectiveSchema db t)
    in mkSql "existsTableSql PostGres" [st|
SELECT case when EXISTS (
    SELECT 1
    FROM   pg_catalog.pg_class c
    JOIN   pg_catalog.pg_namespace n ON n.oid = c.relnamespace
    WHERE  1=1
    #{cl} -- AND n.nspname = 'public'
    AND    c.relname = '#{_tName t}'
    AND    c.relkind = 'r'    -- only tables(?)
) then '#{found}'
else '#{notfound}' end
|]
  dropTableIfExistsSql db table = mkSql "dropTableIfExistsSql PostGres" [st|drop table if exists #{getEffectiveTable db table}|]
  dropViewIfExistsSql db table = mkSql "dropViewIfExistsSql PostGres" [st|drop view if exists #{getEffectiveTable db table}|]

  getColumnMetaSql db t = (pgType, getPGColumnMetaSql db t)

  translateColumnMeta _ (cd, ColumnMeta {..}) =
     case cd of
       CString -> [st|varchar(#{cLength})|]
       CFixedString -> [st|char(#{cLength})|]
       CInt -> "bigint" -- "integer"
       CDateTime -> "timestamp"
       CDate -> "date"
       CFloat -> "numeric"
       CBool -> "boolean"
       CBinary -> error "CBinary postgres not defined yet"
       CBLOB -> "bytea"
       CCLOB -> "text"
       COther o -> error $ "translateColumnMeta: dont know how to convert this columnmeta to postgres " ++ show o
  limitSql _ = maybe mempty (\n -> [st|limit #{n}|])
  getDbDefault _ = ''DBPG

pgType :: ColumnMeta -> ColDataType
pgType (T.toLower. cType &&& cScale -> (ss,mscale))
  | ss == "numeric" && mscale == Just 0 = CInt
  | ss `elem` ["varchar", "character varying"] = CString
  | ss `elem` ["char", "bpchar", "character"] = CFixedString
  | ss `elem` ["smallserial", "serial", "bigserial", "smallint", "int", "integer", "bigint", "int4", "int8"] = CInt
  | ss `elem` ["real", "float", "float8", "double", "numeric"] = CFloat
  | ss `elem` ["bit", "bool","boolean"] = CBool
  | ss `elem` ["timestamp", "timestamp without time zone", "timestamp with time zone"] = CDateTime
  | ss == "date" = CDate
  | ss `elem` [] = CBinary
  | ss == "bytea" = CBLOB
  | ss == "text" = CCLOB
  | otherwise = COther ss

pgSchemaDBSql :: DBPG a -> Text
pgSchemaDBSql = pgSchemaSql' . getSchema

pgSchemaTableSql :: DBPG a -> Table (DBPG a) -> Text
pgSchemaTableSql db = pgSchemaSql' . getEffectiveSchema db

pgSchemaSql' :: Maybe Text -> Text
pgSchemaSql' =
  \case
    Nothing -> " NOT IN ('pg_catalog', 'information_schema')"
    Just sch -> [st| = '#{sch}'|]

-- https://dba.stackexchange.com/questions/90555/postgresql-select-primary-key-as-serial-or-bigserial/90567#90567
getPGColumnMetaSql :: DBPG a -> Table (DBPG a) -> Sql (DBPG a) '[] '[Sel ColumnMeta]
getPGColumnMetaSql db t = mkSql "getPGColumnMetaSql" [st|
SELECT
      tab_columns.column_name
    , udt_name
    , case when is_nullable='YES' then 1 else 0 end as isnullable
    , case when character_maximum_length is null then 2000 else character_maximum_length end as char_maxlen
    , numeric_precision
    , numeric_scale
    , 0 as iscomputed
    , case when ZZZ.data_type in ('smallserial','serial','bigserial') then 1 else 0 end as is_identity
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
WHERE  a.attrelid = '#{_tName t}'::regclass
AND    a.attnum > 0
AND    NOT a.attisdropped
) ZZZ on ZZZ.attname=tab_columns.column_name


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
WHERE tab_columns.table_name = '#{_tName t}'
and   tab_columns.table_schema #{pgSchemaTableSql db t}
and (tab_constraints.table_name is null or tab_constraints.table_name = tab_columns.table_name)
ORDER BY ordinal_position
|]


--https://stackoverflow.com/questions/769683/show-tables-in-postgresql
-- change out 'public' to the schema you want
-- this takes 20 seconds to run but the next time is really fast

getPGTableCountsSql :: Sql (DBPG a) '[] '[Sel (GetAllTablesCount (DBPG a))]
getPGTableCountsSql = mkSql "getPGTableCountsSql" [st|
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

