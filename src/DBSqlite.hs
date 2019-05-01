{- |
Module      : DBSqlite
Description : Sqlite
Copyright   : (c) Grant Weyburne, 2016
License     : GPL-3
Maintainer  : gbwey9@gmail.com

Implementation of GConn for sqlite.
-}
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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}
module DBSqlite where
import Prelude hiding (FilePath)
import Text.Shakespeare.Text
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (fromText)
import GConn
import Data.Char
import System.FilePath
import Sql
import GHC.Generics (Generic)
import Control.Lens.TH
import Util
import qualified Data.Configurator as C
import Language.Haskell.TH hiding (Dec)

data DBSqlite a = DBSqlite { _driverdsn :: !Text
                           , _liteschema :: !(Maybe Text)
                           , _dbfn :: !FilePath
                           } deriving (Show, Eq, Generic)

makeLenses ''DBSqlite

instance ToText (DBSqlite a) where
  toText x = fromText $ maybe "" (<> ".") (_liteschema x) <> T.pack (_dbfn x)

type instance WriteableDB (DBSqlite Writeable) = 'True

instance GConn (DBSqlite a) where
  loadConnTH _ k = do
    c <- runIO loadFromConfig
    driver <- runIO $ req c (k <> ".driver")
    mschema <- runIO $ C.lookup c (k <> ".schema")
    fn <- runIO $ req c (k <> ".fn")
    let schemasplice = maybe [| Nothing |] (\x -> [| Just $(stringE x) |]) mschema
    [| DBSqlite $(stringE driver) $(schemasplice) $(stringE fn) |]

  ignoreDisconnectError _ = True
  connText DBSqlite {..} = [st|#{_driverdsn};Database=#{_dbfn};|] -- ;TraceFile=d:\haskell\s.log;|]
  connCSharpText DBSqlite {..} = undefined
  showDb DBSqlite {..} = [st|sqlite db=#{_dbfn}|]
  getSchema = const Nothing
  getDb = Just . T.pack . _dbfn
  getDelims _ = Just ('"','"')
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

  translateColumnMeta _ (cd, ColumnMeta {..}) =
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
       COther o -> error $ "translateColumnMeta: dont know how to convert this columnmeta to mssql " ++ show o
  limitSql _ = maybe mempty (\n -> [st|limit #{n}|])
  getDbDefault _ = ''DBSqlite

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
getSLColumnMetaSql t = Sql "getSLColumnMetaSql" defEnc (E1 (SelP 1 decColumnMetaSqlite)) [st|pragma table_info (#{escapeField t (_tName t)})|]

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

