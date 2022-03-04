{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : HSql.ODBC.DBMSSQL
Description : MSSQL Server
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

Implementation of GConn for ms sql server.
-}
module HSql.ODBC.DBMSSQL (
  module HSql.ODBC.DBMSSQL,
  module Database.MSSql,
) where

import Control.Arrow ((&&&))
import Control.DeepSeq (NFData)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Vinyl
import Database.MSSql
import qualified GHC.Generics as G (Generic)
import GHC.Stack
import HSql.Core
import HSql.Core.VinylUtils
import HSql.ODBC.GConn
import qualified Language.Haskell.TH.Syntax as TH (lift, runIO)
import Text.Shakespeare.Text (ToText (..), st)
import Utils.Error
import Prelude hiding (FilePath)

-- | writeable database
type instance WriteableDB (DBMS Writeable) = 'True

-- | c# connection string
connCSharpText :: DBMS a -> String
connCSharpText DBMS{..} = T.unpack [st|Server=#{msserver};Database=#{msdb};#{connAuthMSSQLCSharp msauthn};Connection Timeout=0;MultipleActiveResultSets=true;|] -- Packet Size=32767

instance GConn (DBMS a) where
  loadConnTH _ k = do
    c <- TH.runIO $ loadConn @(DBMS a) k
    TH.lift c

  ignoreDisconnectError _ = True

  getAllTablesCountSql _ =
    Just $
      mkSql
        "getAllTablesCountSql"
        [st|
SELECT
 SCHEMA_NAME(schema_id) + '.[' + t.name + ']' as tname
,SUM(p.rows) AS tot
,t.create_date
,t.modify_date
FROM sys.tables AS t
JOIN sys.partitions AS p
ON t.object_id = p.object_id
AND p.index_id IN ( 0, 1 )
GROUP BY SCHEMA_NAME(schema_id), t.name ,t.create_date, t.modify_date
order by SCHEMA_NAME(schema_id), t.name
|]

  getAllTablesSql _ =
    mkSql
      "getAllTablesSql MSSQL"
      [st|
     select SCHEMA_NAME(schema_id) + '.' + name
     from sys.tables
     order by SCHEMA_NAME(schema_id), name
  |]
  getAllViewsSql _ =
    mkSql
      "getAllViewsSql MSSQL"
      [st|
     select SCHEMA_NAME(schema_id) + '.' + name
     from sys.views
     order by SCHEMA_NAME(schema_id), name
  |]
  existsTableSql db table =
    mkSql
      "existsTableSql MSSQL"
      [st|
IF OBJECT_ID('#{getEffectiveTable db table}', 'U') IS NOT NULL
BEGIN
  select '#{found}'
END
ELSE select '#{notfound}'
|]
  dropTableIfExistsSql db table =
    mkSql
      "dropTableIfExistsSql MSSQL"
      [st|
IF OBJECT_ID('#{getEffectiveTable db table}', 'U') IS NOT NULL
BEGIN
  drop table #{table}
END
|]
  dropViewIfExistsSql db table =
    mkSql
      "dropViewIfExistsSql MSSQL"
      [st|
IF OBJECT_ID('#{getEffectiveTable db table}', 'V') IS NOT NULL
BEGIN
  drop view #{table}
END
|]

  getColumnMetaSql db t = (mssqlType, getMSColumnMetaSql db t)

  translateColumnMeta _ (cd, ColumnMeta{..}) =
    case cd of
      CString -> [st|varchar(#{cLength})|]
      CFixedString -> [st|char(#{cLength})|]
      CInt -> "bigint"
      CDateTime -> "datetime2"
      CDate -> "date"
      CFloat -> "float(53)"
      CBool -> "tinyint"
      CBLOB -> "varbinary(max)"
      CCLOB -> "varchar(max)"
      CBinary -> [st|varbinary(#{cLength})|]
      COther o -> normalError $ "translateColumnMeta: dont know how to convert this columnmeta to mssql " ++ show o
  limitSql _ = maybe mempty (\n -> [st|top #{n}|])

-- | typed select query for the meta data of a sql table
getMSColumnMetaSql :: DBMS db -> Table (DBMS db) -> Sql (DBMS db) '[] '[Sel ColumnMeta]
getMSColumnMetaSql db t =
  let cl = maybe mempty (<> ".") (getEffectiveSchema db t)
   in mkSql
        "getMSColumnMetaSql"
        [st|
SELECT
    c.name 'Column Name'
  , t.Name 'Data type'
  , c.is_nullable
  , c.max_length 'Max Length'
  , c.precision
  , c.scale
  , case when c.is_computed = 1 then 1 else 0 end
  , case when c.is_identity = 1 then 1 else 0 end
  , case when ic.index_column_id is null then 0 else ic.index_column_id end
FROM
    sys.columns c
INNER JOIN
    sys.types t ON c.user_type_id = t.user_type_id
LEFT OUTER JOIN
    sys.index_columns ic ON ic.object_id = c.object_id AND ic.column_id = c.column_id
LEFT OUTER JOIN
    sys.indexes i ON ic.object_id = i.object_id AND ic.index_id = i.index_id
WHERE
  c.object_id = OBJECT_ID('#{cl}#{tName t}')
order by c.column_id
|]

-- | convert mssql specific column type to generic ColDataType
mssqlType :: ColumnMeta -> ColDataType
mssqlType (cLength &&& T.toLower . cType -> (len, ss))
  | ss == "varchar" = if len == -1 then CCLOB else CString
  | ss == "char" = CFixedString
  | ss `elem` ["int", "integer", "bigint"] = CInt
  | ss `elem` ["decimal", "float", "double"] = CFloat
  | ss `elem` ["tinyint", "bit"] = CBool
  | ss `elem` ["datetime", "datetime2"] = CDateTime
  | ss == "date" = CDate
  | ss == "varbinary" = if len == -1 then CBLOB else CBinary
  | ss == "binary" = CBinary
  | ss `elem` ["blob", "image"] = CBLOB
  | ss `elem` ["clob", "text"] = CCLOB
  | otherwise = COther ss

-- | support trusted and user/pwd authentication
connAuthMSSQLCSharp :: MSAuthn -> String
connAuthMSSQLCSharp Trusted = "Trusted_Connection=True"
connAuthMSSQLCSharp (UserPwd uid (Secret pwd)) = T.unpack [st|User Id=#{uid};Password=#{pwd}|]

-- | change to a trusted connection
mkTrusted :: HasCallStack => DBMS a -> DBMS a
mkTrusted z@DBMS{..} =
  z
    { msauthn = case msauthn of
        Trusted -> normalError "mkTrusted: already trusted!!!"
        UserPwd{} -> Trusted
    }

-- | run sql wrapped with arithabort and using default encoders and decoders
mssql ::
  forall b a db.
  (DefDec (Rec SingleIn b), DefEnc (Rec Enc a)) =>
  Text ->
  Text ->
  Sql (DBMS db) a b
mssql x w = mkSql x ("set arithabort on;\n" <> w)

-- | run sql wrapped with arithabort passing
mssql' ::
  forall b a db.
  Text ->
  Rec Enc a ->
  Rec SingleIn b ->
  Text ->
  Sql (DBMS db) a b
mssql' x y z w = Sql x y z ("set arithabort on;\n" <> w)

-- | vinyl labelled record for a primary key
type PKeysMSSQL db = F '["tab" ::: Table (DBMS db), "pkname" ::: Text, "cname" ::: Text, "key_ordinal" ::: Int, "fillfactor" ::: Int, "type_desc" ::: Text]

-- | extract primary keys for all tables in the database
getPrimaryKeysMS :: GConn (DBMS db) => Sql (DBMS db) '[] '[Sel (PKeysMSSQL db)]
getPrimaryKeysMS =
  mkSql
    "getPrimaryKeysMS"
    [st|
select
   schema_name(ta.schema_id) + '.' + ta.name as tab
  ,ind.name as pkname
  ,col.name  cname
  ,indcol.key_ordinal
  ,ind.fill_factor
  ,ind.type_desc
 from sys.tables ta
  inner join sys.indexes ind
   on ind.object_id = ta.object_id
  inner join sys.index_columns indcol
   on indcol.object_id = ta.object_id
    and indcol.index_id = ind.index_id
  inner join sys.columns col
   on col.object_id = ta.object_id
    and col.column_id = indcol.column_id
 where ind.is_primary_key = 1
 order by tab, pkname,indcol.key_ordinal
|]

-- | add a primary key
addPrimaryKeyMS :: GConnWrite (DBMS db) => Table (DBMS db) -> Text -> Clustered -> Text -> Sql (DBMS db) '[] '[U0]
addPrimaryKeyMS tab pkname clustered cnames =
  mkSql "addPrimaryKeyMS" [st|ALTER TABLE #{tab} ADD CONSTRAINT [#{pkname}] PRIMARY KEY #{show clustered} ( #{cnames} )|]

-- | create an index
createIndexMS :: GConnWrite (DBMS db) => Table (DBMS db) -> Text -> Text -> Sql (DBMS db) '[] '[U0]
createIndexMS tab ixname cnames = mkSql "createIndexMS" [st|create index [ix_#{ixname}] on #{tab} ( #{cnames} )|]

-- | vinyl labelled record for a foreign key
type FKeysMSSQL db = F '["tab" ::: Table (DBMS db), "colname" ::: Text, "reftab" ::: Table (DBMS db), "refcolname" ::: Text, "fkeyname" ::: Text]

-- | get all foreign keys for a database
getForeignKeysMS :: GConn (DBMS db) => Sql (DBMS db) '[] '[Sel (FKeysMSSQL db)]
getForeignKeysMS =
  mkSql
    "getForeignKeysMS"
    [st|
SELECT
  OBJECT_SCHEMA_NAME(f.parent_object_id) + '.' + OBJECT_NAME(f.parent_object_id) AS TableName,
  COL_NAME(fc.parent_object_id,fc.parent_column_id) AS ColumnName,
  OBJECT_SCHEMA_NAME(f.referenced_object_id) + '.' + OBJECT_NAME (f.referenced_object_id) AS ReferenceTableName,
  COL_NAME(fc.referenced_object_id,fc.referenced_column_id) AS ReferenceColumnName,
  f.name AS ForeignKey
FROM
  sys.foreign_keys AS f
  INNER JOIN sys.foreign_key_columns AS fc ON f.OBJECT_ID = fc.constraint_object_id
  INNER JOIN sys.objects AS o ON o.OBJECT_ID = fc.referenced_object_id
|]

-- | get all the table names from a msql database
getDBListSqlMS :: Sql (DBMS db) '[] '[SelCol Text]
getDBListSqlMS =
  mkSql
    "getDBListSqlMS"
    [st|SELECT name

FROM sys.sysdatabases
WHERE HAS_DBACCESS(name) = 1|]

-- | vinyl labelled record for sql role information
type GetRolesMS = F '["name" ::: Text, "roletype" ::: Text, "auth" ::: Text, "owner" ::: Text, "created" ::: UTCTime, "modified" ::: UTCTime]

-- | select statement for pulling role information
getRolesSqlMS :: Sql (DBMS db) '[] '[Sel GetRolesMS]
getRolesSqlMS =
  mkSql
    "getRolesSqlMS"
    [st|
SELECT name, type_desc, authentication_type_desc
,isnull(USER_NAME(mem.role_principal_id),'''') AS AssociatedRole ,create_date,modify_date
FROM sys.database_principals prin
LEFT OUTER JOIN sys.database_role_members mem ON prin.principal_id=mem.member_principal_id
WHERE prin.sid IS NOT NULL and prin.sid NOT IN (0x00) and
prin.is_fixed_role <> 1 AND prin.name NOT LIKE '##%'
|]

-- | get table information using vinyl labelled records as output
getMSTableCountsSql :: Sql (DBMS a) '[] '[Sel (F '["schema" ::: Text, "table" ::: Text, "rows" ::: Int, "create_date" ::: UTCTime, "modified" ::: UTCTime])]
getMSTableCountsSql =
  mkSql
    "getMSTableCountsSql"
    [st|
SELECT
 SCHEMA_NAME(schema_id) AS SchemaName
,'[' + t.name +']' AS TableName
,SUM(p.rows) AS TotalRowCount
,t.create_date
,t.modify_date
FROM sys.tables AS t
JOIN sys.partitions AS p
ON t.object_id = p.object_id
AND p.index_id IN ( 0, 1 )
GROUP BY SCHEMA_NAME(schema_id), t.name, t.create_date, t.modify_date
|]

{- | 'wrapNonTransaction' is used for mssql only for running outside of transaction
 dont need GConnWrite cos enforced by U0 when you try to run it
-}
wrapNonTransaction :: Sql db '[] '[Upd] -> Sql db '[] '[Alle U0]
wrapNonTransaction sql =
  mkSql
    "wrapNonTransaction"
    [st|
  commit transaction
  SET IMPLICIT_TRANSACTIONS OFF
  #{sSql sql}
  SET IMPLICIT_TRANSACTIONS ON
  begin transaction
 |]

-- | vinyl labelled record for a connection options
type OptionsMSSQL = F '["name" ::: String, "flag" ::: Bool]

-- | select statement for extracting connection options
mssqlOptions :: Sql (DBMS a) '[] '[Sel OptionsMSSQL]
mssqlOptions =
  mkSql
    "mssqlOptions"
    [st|
DECLARE @options INT
SELECT @options = @@OPTIONS
select 'options=' + convert(varchar(10),@options),1
union all
select 'IMPLICIT_TRANSACTIONS',case when (2 & @options) = 2 then 1 else 0 end
union all
select 'CURSOR_CLOSE_ON_COMMIT',case when (4 & @options) = 4 then 1 else 0 end
union all
select 'ANSI_WARNINGS',case when (8 & @options) = 8 then 1 else 0 end
union all
select 'ANSI_PADDING',case when (16 & @options) = 16 then 1 else 0 end
union all
select 'ANSI_NULLS',case when (32 & @options) = 32 then 1 else 0 end
union all
select 'ARITHABORT',case when (64 & @options) = 64 then 1 else 0 end
union all
select 'ARITHIGNORE',case when (128 & @options) = 128 then 1 else 0 end
union all
select 'QUOTED_IDENTIFIER',case when (256 & @options) = 256 then 1 else 0 end
union all
select 'NOCOUNT',case when (512 & @options) = 512 then 1 else 0 end
union all
select 'ANSI_NULL_DFLT_ON',case when (1024 & @options) = 1024 then 1 else 0 end
union all
select 'ANSI_NULL_DFLT_OFF',case when (2048 & @options) = 2048 then 1 else 0 end
union all
select 'CONCAT_NULL_YIELDS_NULL',case when (4096 & @options) = 4096 then 1 else 0 end
union all
select 'NUMERIC_ROUNDABORT',case when (8192 & @options) = 8192 then 1 else 0 end
union all
select 'XACT_ABORT',case when (16384 & @options) = 16384 then 1 else 0 end
|]

-- | bcp authorisation options extracted from 'MSAuthn'
bcpauth :: MSAuthn -> [String]
bcpauth Trusted = ["-T"]
bcpauth (UserPwd uid (Secret pwd)) = ["-U" <> T.unpack uid, "-P" <> T.unpack pwd]

-- | identifies the bcp output file
newtype LogId = LogId {unLogId :: Int}
  deriving stock (Show, Eq, G.Generic)
  deriving newtype (Num, Enum, ToText)

instance NFData LogId

-- | bcp compatible name
showTableForBCP :: Table a -> LogId -> String
showTableForBCP Table{..} lid =
  let q0 = case (tDb, tSchema) of
        (Nothing, Schema Nothing) -> mempty
        (Nothing, ConnSchema) -> mempty
        (Just a, Schema Nothing) -> showTName a <> "__"
        (Just a, ConnSchema) -> showTName a <> "__"
        (Nothing, Schema (Just b)) -> showTName b <> "_"
        (Just a, Schema (Just b)) -> showTName a <> "_" <> showTName b <> "_"
   in T.unpack [st|#{q0}#{tName}.#{lid}|]
