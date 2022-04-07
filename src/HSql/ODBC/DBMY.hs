{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : HSql.ODBC.DBMY
Description : MySql
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

Implementation of GConn for mysql.
-}
module HSql.ODBC.DBMY where

import Data.Text (Text)
import qualified Data.Text as T
import Database.MySql
import HSql.Core.Sql
import HSql.ODBC.GConn
import qualified Language.Haskell.TH.Syntax as TH (lift, runIO)
import Primus.Error
import Text.Shakespeare.Text (st)
import Prelude hiding (FilePath)

-- | writeable instance for a mysql database
type instance WriteableDB (DBMY Writeable) = 'True

instance GConn (DBMY a) where
  loadConnTH _ k = do
    c <- TH.runIO $ loadConn @(DBMY a) k
    TH.lift c

  getAllTablesSql _ =
    mkSql
      "getAllTablesSql MySql"
      [st|
    select concat(table_schema, '.', table_name)
    from information_schema.tables
    where table_type = 'BASE TABLE'
    and table_schema = database()
    order by table_schema, table_name
  |]
  getAllViewsSql _ =
    mkSql
      "getAllViewsSql MySql"
      [st|
    select concat(table_schema, '.', table_name)
    from information_schema.tables
    where table_type = 'VIEW'
    and table_schema = database()
    order by table_schema, table_name
  |]
  existsTableSql db t =
    let sch = case getEffectiveSchema db t of
          Nothing -> mempty
          Just s -> [st|AND table_schema = '#{s}'|]
     in mkSql
          "existsTableSql MySql"
          [st|
SELECT case when count(*) > 0 then '#{found}' else '#{notfound}' end
  FROM information_schema.tables
  WHERE 1=1
    #{sch}
    AND table_name = '#{tName t}'
|]
  dropTableIfExistsSql db table = mkSql "dropTableIfExistsSql MySql" [st|drop table if exists #{getEffectiveTable db table}|]
  dropViewIfExistsSql db table = mkSql "dropViewIfExistsSql MySql" [st|drop view if exists #{getEffectiveTable db table}|]

  getColumnMetaSql db t = (myType, getMYColumnMetaSql db t)

  translateColumnMeta _ (cd, ColumnMeta{..}) =
    case cd of
      CString -> [st|varchar(#{cLength})|]
      CFixedString -> [st|char(#{cLength})|]
      CInt -> "bigint"
      CDateTime -> "datetime"
      CDate -> "date"
      CFloat -> "double"
      CBool -> "tinyint"
      CBinary -> "varbinary(max)"
      CBLOB -> "blob"
      CCLOB -> "blob"
      COther o -> normalError $ "translateColumnMeta: dont know how to convert this columnmeta to mysql " ++ show o
  limitSql _ = maybe mempty (\n -> [st|limit #{n}|])

-- | convert from mysql datatype to 'ColDataType'
myType :: ColumnMeta -> ColDataType
myType (T.toLower . cType -> ss)
  | ss == "varchar" = CString
  | ss == "char" = CFixedString
  | ss `elem` ["smallint", "mediumint", "int", "integer", "bigint"] = CInt
  | ss `elem` ["float", "double"] = CFloat
  | ss == "tinyint" = CBool
  --   ss `elem` ["bit", "bool","boolean"] = CBool
  | ss `elem` ["timestamp", "datetime"] = CDateTime
  | ss == "date" = CDate
  | ss `elem` ["varbinary", "binary"] = CBinary
  | ss `elem` ["tinyblob", "blob", "mediumblob", "longblob"] = CBLOB
  | ss == "text" = CCLOB
  | otherwise = COther ss

-- | get the database schema
mySchemaDBSql :: DBMY a -> Text
mySchemaDBSql = mySchemaSql' . getSchema

-- | get the table schema
mySchemaTableSql :: DBMY a -> Table (DBMY a) -> Text
mySchemaTableSql db =
  mySchemaSql' . getEffectiveSchema db

-- | get the schema
mySchemaSql' :: Maybe Text -> Text
mySchemaSql' =
  \case
    Nothing -> " NOT IN ('performance_schema', 'information_schema')"
    Just sch -> [st| = '#{sch}'|]

-- | sql for get metadata on a table
getMYColumnMetaSql :: DBMY db -> Table (DBMY db) -> Sql (DBMY db) '[] '[Sel ColumnMeta]
getMYColumnMetaSql db t =
  mkSql
    "getMYColumnMetaSql"
    [st|
SELECT
      column_name
    , data_type
    , case when is_nullable='YES' then 1 else 0 end as isnullable
    , case when character_maximum_length is null then 0 else character_maximum_length end as char_maxlen
    , numeric_precision
    , numeric_scale
    , 0 as iscomputed
    , case when extra is not null and extra like '%auto_increment%' then 1 else 0 end
    , case when column_key is not null and column_key = 'PRI' then 1 else 0 end -- need to get column number of key
FROM information_schema.columns
WHERE table_name = '#{tName t}'
and   table_schema #{mySchemaTableSql db t}
ORDER BY ordinal_position
|]
