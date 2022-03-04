{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : HSql.ODBC.DBOracle
Description : Oracle
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

Implementation of GConn for oracle.
-}
module HSql.ODBC.DBOracle (
  module HSql.ODBC.DBOracle,
  module Database.Oracle,
) where

import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)
import Database.Oracle
import GHC.Stack
import HSql.Core.Sql
import HSql.ODBC.GConn
import qualified Language.Haskell.TH.Syntax as TH (lift, runIO)
import Text.Shakespeare.Text (st)
import Utils.Error
import Prelude hiding (FilePath)

-- | writeable instance for an oracle database
type instance WriteableDB (DBOracle Writeable) = 'True

instance GConn (DBOracle a) where
  loadConnTH _ k = do
    c <- TH.runIO $ loadConn @(DBOracle a) k
    TH.lift c

  -- ;FWC=T;StatementCache=T;
  getAllTablesSql DBOracle{..} =
    mkSql
      "getAllTablesSql Oracle"
      [st|
    select owner || '.' || TABLE_NAME
    from SYS.ALL_TABLES
    where owner='#{orschema}'
    order by TABLE_NAME
  |]
  getAllViewsSql DBOracle{..} =
    mkSql
      "getAllViewsSql Oracle"
      [st|
    select owner || '.' || VIEW_NAME
    from SYS.ALL_VIEWS
    where owner='#{orschema}'
    order by VIEW_NAME
  |]

  -- bearbeiten: schema handling
  existsTableSql db t =
    let cl = maybe mempty (\sch -> [st| and owner = '#{sch}'|]) (getEffectiveSchema db t)
     in mkSql
          "existsTableSql Oracle"
          [st|
SELECT case when EXISTS (
    SELECT 1
    from SYS.ALL_TABLES
    where table_name = '#{tName t}'
    #{cl}
) then '#{found}'
else '#{notfound}' end from dual
|]

  -- unfortunately no such thing as if exists!!! so will have to check separately unless using an extremely new version of oracle
  dropTableIfExistsSql db table = mkSql "dropTableIfExistsSql Oracle" [st|drop table #{getEffectiveTable db table}|]
  dropViewIfExistsSql db table = mkSql "dropViewIfExistsSql Oracle" [st|drop view #{getEffectiveTable db table}|]

  getColumnMetaSql db t = (oracleType, getOracleColumnMetaSql db t)

  translateColumnMeta _ (cd, ColumnMeta{..}) =
    case cd of
      CString -> [st|VARCHAR2(#{cLength})|]
      CFixedString -> [st|CHAR(#{cLength})|]
      CInt -> "INTEGER"
      CDateTime -> "TIMESTAMP"
      CDate -> "DATE"
      CFloat -> "FLOAT" -- number doesnt translate through odbc driver: ie defaults to bytestring: hopefully some these types work!
      CBool -> "INTEGER"
      CBLOB -> "BLOB"
      CCLOB -> "CLOB"
      CBinary -> [st|RAW(#{cLength})|]
      COther o -> normalError $ "translateColumnMeta: dont know how to convert this columnmeta to oracle " ++ show o

  --  limitSql _ = maybe mempty (\n -> [st|OFFSET 0 ROWS FETCH NEXT #{n} ROWS ONLY|])
  limitSql _ = maybe mempty (\n -> [st|rownum < #{n}|]) -- older versions but trickier to get right

-- | sql for getting metadata from an oracle table
getOracleColumnMetaSql :: HasCallStack => DBOracle a -> Table (DBOracle a) -> Sql (DBOracle a) '[] '[Sel ColumnMeta]
getOracleColumnMetaSql db t =
  let sch = fromMaybe (normalError $ "getOracleColumnMetaSql: missing schema!!" <> show t) (getEffectiveSchema db t)
   in mkSql
        "getOracleColumnMetaSql"
        [st|
  select
     t.column_name
   , t.data_type
   , case when t.nullable='Y' then 1 else 0 end as nullable
   , t.data_length
   , t.data_precision
   , t.data_scale
   , 0 as iscomputed
   , case when t.identity_column='Y' then 1 else 0 end
   , case when cons.constraint_type='P' then 1 else 0 end
  from all_tab_cols t
      left outer join all_cons_columns cols
        on t.owner=cols.owner
        and t.column_name=cols.column_name
      left outer join all_constraints cons
        on cons.table_name=cols.table_name
        and cons.constraint_name=cols.constraint_name
        and t.owner=cons.owner
        and cons.constraint_type='P'
  where t.owner='#{sch}'
  and t.table_name='#{tName t}'
  order by t.table_name, t.column_id
 |]

-- | convert from oracle datatype to 'ColDataType'
oracleType :: ColumnMeta -> ColDataType
oracleType (cType &&& cScale -> (ss, mscale))
  | ss == "NUMERIC" && mscale == Just 0 = CInt
  | ss `elem` ["VARCHAR", "VARCHAR2"] = CString
  | ss `elem` ["CHAR", "CHAR2"] = CFixedString
  | ss `elem` ["INT", "INTEGER", "NUMBER"] = CInt
  | ss == "FLOAT" = CFloat
  | ss `elem` ["DATETIME", "DATE"] = CDate
  | ss `elem` ["VARBINARY", "BINARY"] = CBinary
  | ss == "CLOB" = CCLOB
  | ss == "BLOB" = CBLOB
  | otherwise = COther ss
