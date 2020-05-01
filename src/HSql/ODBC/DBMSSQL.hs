{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{- |
Module      : HSql.ODBC.DBMSSQL
Description : MSSQL Server
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
Maintainer  : gbwey9@gmail.com

Implementation of GConn for ms sql server.
-}
module HSql.ODBC.DBMSSQL (
    module HSql.ODBC.DBMSSQL
  , module Database.MSSql
  ) where
import Prelude hiding (FilePath)
import Text.Shakespeare.Text
import Data.Text (Text)
import qualified Data.Text as T
import HSql.ODBC.GConn
import Data.Time
import HSql.Core.Sql
import HSql.Core.Decoder
import HSql.Core.Encoder
import HSql.Core.VinylUtils
import Control.Arrow
import Data.Vinyl
import GHC.Stack
import Data.Text.Lazy.Builder (fromText)
import Language.Haskell.TH.Syntax -- (Lift)
import qualified Language.Haskell.TH.Syntax as TH
import Database.MSSql

type instance WriteableDB (DBMS Writeable) = 'True

-- | c# connection string
connCSharpText :: DBMS a -> String
connCSharpText DBMS {..} = T.unpack [st|Server=#{_msserver};Database=#{_msdb};#{connAuthMSSQLCSharp _msauthn};Connection Timeout=0;MultipleActiveResultSets=true;|] -- Packet Size=32767

instance GConn (DBMS a) where
  loadConnTH _ k = do
    c <- runIO $ loadConn @(DBMS a) k
    TH.lift c

  ignoreDisconnectError _ = True

  getAllTablesCountSql _ = Just $ mkSql "getAllTablesCountSql" [st|
SELECT
 SCHEMA_NAME(schema_id) + '.' + t.name AS tname
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

  getAllTablesSql _ = mkSql "getAllTablesSql MSSQL" [st|
     select SCHEMA_NAME(schema_id) + '.' + name
     from sys.tables
     order by SCHEMA_NAME(schema_id), name
  |]
  getAllViewsSql _ = mkSql "getAllViewsSql MSSQL" [st|
     select SCHEMA_NAME(schema_id) + '.' + name
     from sys.views
     order by SCHEMA_NAME(schema_id), name
  |]
  existsTableSql db table = mkSql "existsTableSql MSSQL" [st|
IF OBJECT_ID('#{getEffectiveTable db table}', 'U') IS NOT NULL
BEGIN
  select '#{found}'
END
ELSE select '#{notfound}'
|]
  dropTableIfExistsSql db table = mkSql "dropTableIfExistsSql MSSQL" [st|
IF OBJECT_ID('#{getEffectiveTable db table}', 'U') IS NOT NULL
BEGIN
  drop table #{table}
END
|]
  dropViewIfExistsSql db table = mkSql "dropViewIfExistsSql MSSQL" [st|
IF OBJECT_ID('#{getEffectiveTable db table}', 'V') IS NOT NULL
BEGIN
  drop view #{table}
END
|]

  getColumnMetaSql db t = (mssqlType, getMSColumnMetaSql db t)

  translateColumnMeta _ (cd, ColumnMeta {..}) =
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
       COther o -> error $ "translateColumnMeta: dont know how to convert this columnmeta to mssql " ++ show o
  limitSql _ = maybe mempty (\n -> [st|top #{n}|])

getMSColumnMetaSql :: DBMS db -> Table (DBMS db) -> Sql (DBMS db) '[] '[Sel ColumnMeta]
getMSColumnMetaSql db t =
  let cl = maybe mempty (<> ".") (getEffectiveSchema db t)
  in mkSql "getMSColumnMetaSql" [st|
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
  c.object_id = OBJECT_ID('#{cl}#{_tName t}')
order by c.column_id
|]


mssqlType :: ColumnMeta -> ColDataType
mssqlType (cLength &&& T.toLower . cType -> (len,ss))
  | ss == "varchar" = if len == -1 then CCLOB else CString
  | ss == "char" = CFixedString
  | ss `elem` ["int", "integer", "bigint"] = CInt
  | ss `elem` ["decimal", "float", "double"] = CFloat
  | ss `elem` ["tinyint", "bit"] = CBool
  | ss `elem` ["datetime", "datetime2"] = CDateTime
  | ss == "date" = CDate
  | ss == "varbinary" = if len == -1 then CBLOB else CBinary
  | ss == "binary" = CBinary
  | ss `elem` ["blob","image"] = CBLOB
  | ss `elem` ["clob","text"] = CCLOB
  | otherwise = COther ss

connAuthMSSQLCSharp :: MSAuthn -> String
connAuthMSSQLCSharp Trusted = "Trusted_Connection=True"
connAuthMSSQLCSharp (UserPwd uid (Secret pwd)) = T.unpack [st|User Id=#{uid};Password=#{pwd}|]

mkTrusted :: HasCallStack => DBMS a -> DBMS a
mkTrusted z@DBMS {..} = z { _msauthn = case _msauthn of
                                          Trusted -> error "mkTrusted: already trusted!!!"
                                          UserPwd {} -> Trusted }

mssql :: forall b a db . (DefDec (Rec SingleIn b), DefEnc (Rec Enc a)) => Text -> Text -> Sql (DBMS db) a b
mssql x w = mkSql x ("set arithabort on;\n" <> w)

mssql' :: forall b a db . Text -> Rec Enc a -> Rec SingleIn b -> Text -> Sql (DBMS db) a b
mssql' x y z w = Sql x y z ("set arithabort on;\n" <> w)

type PKeysMSSQL db = F '["tab" ::: Table (DBMS db), "pkname" ::: Text, "fillfactor" ::: Int, "itype" ::: Text, "cnames" ::: Text]

getPrimaryKeysMS :: GConn (DBMS db) => Sql (DBMS db) '[] '[Sel (PKeysMSSQL db)]
getPrimaryKeysMS = mkSql "getPrimaryKeysMS" [st|
with A as (
select top 1000000000
   schema_name(ta.schema_id) as SchemaName
  ,ta.name  as TableName
  ,ind.name as pkname
  ,indcol.key_ordinal ord1
  ,col.name  cname
  ,ind.type_desc
  ,ind.fill_factor
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
 order by
   ta.name
  ,indcol.key_ordinal
) select p.SchemaName + '.' + p.TableName,p.pkname,p.fill_factor,p.type_desc,cnames=convert(varchar(200),stuff(
(select ',' + c.cname from A c
  where p.SchemaName=c.SchemaName and p.TableName=c.TableName and p.pkname=c.pkname
  order by c.ord1
  FOR XML PATH ('')
  )
 , 1, 1, ''))
 from A p
group by SchemaName, TableName, pkname, p.fill_factor, p.type_desc
|]

data Clustered = Clustered | NonClustered deriving (Show,Eq)

instance ToText Clustered where
  toText = fromText . T.pack . show

addPrimaryKeyMS :: GConnWrite (DBMS db) => Table (DBMS db) -> Text -> Clustered -> Text -> Sql (DBMS db) '[] '[U0]
addPrimaryKeyMS tab pkname clustered cnames =
  mkSql "addPrimaryKeyMS" [st|ALTER TABLE #{tab} ADD CONSTRAINT [#{pkname}] PRIMARY KEY #{clustered} ( #{cnames} )|]

createIndexMS :: GConnWrite (DBMS db) => Table (DBMS db) -> Text -> Text -> Sql (DBMS db) '[] '[U0]
createIndexMS tab ixname cnames = mkSql "createIndexMS" [st|create index [ix_#{ixname}] on #{tab} ( #{cnames} )|]

type FKeysMSSQL db = F '["tab" ::: Table (DBMS db), "colname" ::: Text, "reftab" ::: Table (DBMS db), "refcolname" ::: Text, "fkeyname" ::: Text]

getForeignKeysMS :: GConn (DBMS db) => Sql (DBMS db) '[] '[Sel (FKeysMSSQL db)]
getForeignKeysMS = mkSql "getForeignKeysMS" [st|
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

getDBListSqlMS :: Sql (DBMS db) '[] '[Sel Text]
getDBListSqlMS = mkSql "getDBListSqlMS" [st|SELECT name
FROM sys.sysdatabases
WHERE HAS_DBACCESS(name) = 1|]

type GetRolesMS = F '["name" ::: Text, "roletype" ::: Text, "auth" ::: Text, "owner" ::: Text, "created" ::: UTCTime, "modified" ::: UTCTime]

getRolesSqlMS :: Sql (DBMS db) '[] '[Sel GetRolesMS]
getRolesSqlMS = mkSql "getRolesSqlMS" [st|
SELECT name, type_desc, authentication_type_desc
,isnull(USER_NAME(mem.role_principal_id),'''') AS AssociatedRole ,create_date,modify_date
FROM sys.database_principals prin
LEFT OUTER JOIN sys.database_role_members mem ON prin.principal_id=mem.member_principal_id
WHERE prin.sid IS NOT NULL and prin.sid NOT IN (0x00) and
prin.is_fixed_role <> 1 AND prin.name NOT LIKE '##%'
|]

getMSTableCountsSql :: Sql (DBMS a) '[] '[Sel (F '["schema" ::: Text, "table" ::: Text, "rows" ::: Int, "create_date" ::: UTCTime, "modified" ::: UTCTime])]
getMSTableCountsSql = mkSql "getMSTableCountsSql" [st|
SELECT
 SCHEMA_NAME(schema_id) AS SchemaName
,t.name AS TableName
,SUM(p.rows) AS TotalRowCount
,t.create_date
,t.modify_date
FROM sys.tables AS t
JOIN sys.partitions AS p
ON t.object_id = p.object_id
AND p.index_id IN ( 0, 1 )
GROUP BY SCHEMA_NAME(schema_id), t.name, t.create_date, t.modify_date
|]

-- | 'wrapNonTransaction' is used for mssql only for running outside of transaction
-- dont need GConnWrite cos enforced by U0 when you try to run it
wrapNonTransaction :: Sql db '[] '[Upd] -> Sql db '[] '[Alle U0]
wrapNonTransaction sql = mkSql "wrapNonTransaction" [st|
  commit transaction
  SET IMPLICIT_TRANSACTIONS OFF
  #{_sSql sql}
  SET IMPLICIT_TRANSACTIONS ON
  begin transaction
 |]

type OptionsMSSQL = F '["name" ::: String, "flag" ::: Bool]

mssqlOptions :: Sql (DBMS a) '[] '[Sel OptionsMSSQL]
mssqlOptions = mkSql "mssqlOptions" [st|
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

bcpauth :: MSAuthn -> [String]
bcpauth Trusted = ["-T"]
bcpauth (UserPwd uid (Secret pwd)) = ["-U" <> T.unpack uid, "-P" <> T.unpack pwd]

