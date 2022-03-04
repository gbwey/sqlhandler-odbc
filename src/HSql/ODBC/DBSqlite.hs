{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
Module      : HSql.ODBC.DBSqlite
Description : Sqlite
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

Implementation of GConn for sqlite.
-}
module HSql.ODBC.DBSqlite where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Sqlite
import HSql.Core.Decoder
import HSql.Core.Encoder
import HSql.Core.Sql
import HSql.Core.SqlParserMS
import HSql.Core.VinylUtils
import HSql.ODBC.GConn
import qualified Language.Haskell.TH.Syntax as TH (lift, runIO)
import Text.Shakespeare.Text (st)
import Utils.Error
import Prelude hiding (FilePath)

-- | writeable instance for sqlite database
type instance WriteableDB (DBSqlite Writeable) = 'True

instance GConn (DBSqlite a) where
  loadConnTH _ k = do
    c <- TH.runIO $ loadConn @(DBSqlite a) k
    TH.lift c

  ignoreDisconnectError _ = True
  getAllTablesSql _ =
    mkSql
      "getAllTablesSql sqlite"
      [st|
     SELECT '.' || name FROM sqlite_master WHERE type='table'
     order by name
  |]
  getAllViewsSql _ =
    mkSql
      "getAllViewsSql sqlite"
      [st|
     SELECT '.' ||name FROM sqlite_master WHERE type='view'
     order by name
  |]
  existsTableSql _ table =
    mkSql
      "existsTableSql sqlite"
      [st|
select case
         when exists(select 7 from sqlite_master where type='table' and name='#{table}')
           then '#{found}'
         else '#{notfound}'
       end
|]
  dropTableIfExistsSql _ table = mkSql "dropTableIfExistsSql sqlite" [st|drop table if exists #{table}|]
  dropViewIfExistsSql _ table = mkSql "dropViewIfExistsSql sqlite" [st|drop view if exists #{table}|]

  getColumnMetaSql _ t = (slType, getSLColumnMetaSql t)

  translateColumnMeta _ z@(cd, _) =
    case cd of
      CString -> "varchar"
      CFixedString -> "char"
      CInt -> "int"
      CDateTime -> "datetime"
      CDate -> "date"
      CFloat -> "float"
      CBool -> "boolean"
      CBLOB -> "blob"
      CCLOB -> "clob"
      CBinary -> "varchar"
      COther o -> normalError $ "translateColumnMeta: dont know how to convert this columnmeta to mssql " ++ show o ++ " z=" ++ show z
  limitSql _ = maybe mempty (\n -> [st|limit #{n}|])

-- | column meta data
data LiteMeta = LiteMeta
  { tpos :: !Int
  , tnm :: !Text
  , ttp :: !Text
  , tnotnull :: !Bool
  , tdflt :: !(Maybe Text)
  , tispk :: !Bool
  }
  deriving stock (Show, Eq)

instance DefDec (Dec LiteMeta) where
  defDec = defD6 LiteMeta

-- | decoder for 'ColumnMeta'
decColumnMetaSqlite :: Dec ColumnMeta
decColumnMetaSqlite = fmap f defDec
 where
  f LiteMeta{..} =
    ColumnMeta
      tnm
      ttp
      tnotnull
      1000
      Nothing
      Nothing
      False
      False
      (if tispk then 1 else 0)

-- | extract metadata for a sqlite table
getSLColumnMetaSql :: Table (DBSqlite a) -> Sql (DBSqlite a) '[] '[Sel ColumnMeta]
getSLColumnMetaSql t = Sql "getSLColumnMetaSql" defEnc (E1 (SelP decColumnMetaSqlite)) [st|pragma table_info (#{escapeField t (showTName (tName t))})|]

-- https://www.techonthenet.com/sqlite/datatypes.php
-- all dates are numeric types!!!
-- strings have no size!!! they can be big as u want

-- | convert from sqlite datatype to 'ColDataType'
slType :: ColumnMeta -> ColDataType
slType (T.toLower . T.takeWhile isAlphaNum . cType -> ss)
  | ss == "varchar" = CString
  | ss == "char" = CFixedString
  | ss `elem` ["int", "integer", "bigint", "numeric"] = CInt
  | ss `elem` ["float", "double"] = CFloat
  | ss == "boolean" = CBool
  | ss `elem` ["datetime", "datetime2"] = CDateTime
  | ss == "date" = CDate
  | ss `elem` ["varbinary", "binary"] = CBinary
  | ss == "blob" = CBLOB
  | ss == "clob" = CCLOB
  | otherwise = COther ss
