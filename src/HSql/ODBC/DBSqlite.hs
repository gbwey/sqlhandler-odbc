{- |
Module      : HSql.ODBC.DBSqlite
Description : Sqlite
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
Maintainer  : gbwey9@gmail.com

Implementation of GConn for sqlite.
-}
{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module HSql.ODBC.DBSqlite where
import Prelude hiding (FilePath)
import Text.Shakespeare.Text (st)
import Data.Text (Text)
import qualified Data.Text as T
import HSql.ODBC.GConn
import Data.Char (isAlphaNum)
import HSql.Core.Sql
import HSql.Core.Decoder
import HSql.Core.Encoder
import HSql.Core.VinylUtils
import qualified Language.Haskell.TH.Syntax as TH (lift,runIO)
import Database.Sqlite

type instance WriteableDB (DBSqlite Writeable) = 'True

instance GConn (DBSqlite a) where
  loadConnTH _ k = do
    c <- TH.runIO $ loadConn @(DBSqlite a) k
    TH.lift c

  ignoreDisconnectError _ = True
  getAllTablesSql _ = mkSql "getAllTablesSql sqlite" [st|
     SELECT '.' || name FROM sqlite_master WHERE type='table'
     order by name
  |]
  getAllViewsSql _ = mkSql "getAllViewsSql sqlite" [st|
     SELECT '.' ||name FROM sqlite_master WHERE type='view'
     order by name
  |]
  existsTableSql _ table = mkSql "existsTableSql sqlite" [st|
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
       COther o -> error $ "translateColumnMeta: dont know how to convert this columnmeta to mssql " ++ show o ++ " z=" ++ show z
  limitSql _ = maybe mempty (\n -> [st|limit #{n}|])

data LiteMeta = LiteMeta { tpos :: !Int
                         , tnm :: !Text
                         , ttp :: !Text
                         , tnotnull :: !Bool
                         , tdflt :: !(Maybe Text)
                         , tispk :: !Bool
                         } deriving (Show,Eq)

instance DefDec (Dec LiteMeta) where
  defDec = defD6 LiteMeta

decColumnMetaSqlite :: Dec ColumnMeta
decColumnMetaSqlite = fmap f defDec
  where f LiteMeta {..} =
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

getSLColumnMetaSql :: Table (DBSqlite a) -> Sql (DBSqlite a) '[] '[Sel ColumnMeta]
getSLColumnMetaSql t = Sql "getSLColumnMetaSql" defEnc (E1 (SelP decColumnMetaSqlite)) [st|pragma table_info (#{escapeField t (_tName t)})|]

-- https://www.techonthenet.com/sqlite/datatypes.php
-- all dates are numeric types!!!
-- strings have no size!!! they can be big as u want
slType :: ColumnMeta -> ColDataType
slType (T.toLower . T.takeWhile isAlphaNum . cType -> ss)
  | ss == "varchar" = CString
  | ss == "char" = CFixedString
  | ss `elem` ["int", "integer", "bigint","numeric"] = CInt
  | ss `elem` ["float", "double"] = CFloat
  | ss == "boolean" = CBool
  | ss `elem` ["datetime", "datetime2"] = CDateTime
  | ss == "date" = CDate
  | ss `elem` ["varbinary", "binary"] = CBinary
  | ss == "blob" = CBLOB
  | ss == "clob" = CCLOB
  | otherwise = COther ss

