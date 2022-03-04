{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : HSql.ODBC.SqlUtils_TH
Description : Utilties for 'HSql.ODBC.Sql_TH'
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

Template haskell utility functions for 'HSql.ODBC.Sql_TH'
-}
module HSql.ODBC.SqlUtils_TH where

import BaseUtils.Extra
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad (forM, when)
import qualified Control.Monad.Except as E
import qualified Control.Monad.State.Strict as S
import Data.Char (isAlphaNum, isLower, isUpper, toLower)
import qualified Data.Map.Strict as M (lookup)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vinyl
import qualified Database.HDBC as H
import Database.MSSql (DBMS)
import HSql.Core.Common (RMeta)
import HSql.Core.Sql
import HSql.Core.VinylUtils
import HSql.ODBC.DBConn
import HSql.ODBC.DBMSSQL ()
import qualified Language.Haskell.TH as TH
import Logging
import Text.Shakespeare.Text (st)
import qualified UnliftIO.Exception as U (Exception, throwIO)
import Prelude hiding (FilePath)

-- | exception when generating template haskell
data SqlTHException
  = InvalidSql !Text
  | MissingSqlMetaData !Text
  | MetaDataInvalidState !Text
  deriving stock (Show)

instance U.Exception SqlTHException

-- | 'getMssqlMeta' runs sql and extracts the metadata used for creating mssql tables
getMssqlMeta :: ML e m => DBMS a -> Sql (DBMS a) '[] '[r] -> m [MSSqlMetaData]
getMssqlMeta mssqldb sql = do
  ys <- getMssqlMetaImpl mssqldb sql
  -- when (or (fmap (H.SqlNull `elem`) ys)) $ U.throwIO $ MissingColumnName $ T.pack $ show ys
  forM (zip [1 :: Int ..] ys) $ \(i, z) ->
    case (rvalf #name &&& rvalf #fulltype &&& rvalf #nullable) z of
      (_, (Nothing, _)) -> U.throwIO $ MissingSqlMetaData [st|getMssqlMeta: missing type for column #{i} sql[#{sSql sql}] z=#{show z}|]
      (Nothing, (Just _, _)) -> U.throwIO $ MissingSqlMetaData [st|getMssqlMeta: missing name for column #{i} sql[#{sSql sql}] z=#{show z}|]
      (Just a, (Just b, c)) -> return $ MSSqlMetaData a b c

-- | generates metadata for Rec ElField
getMssqlMetaTH ::
  ML e m =>
  (Text -> Text) ->
  DBMS a ->
  Sql (DBMS a) '[] '[r] ->
  m [MSSqlTHMetaData]
getMssqlMetaTH fn mssqldb sql = do
  ts <- getMssqlMetaImpl mssqldb sql
  case flip S.evalStateT Set.empty $ forM (itoList ts) $ uncurry (getSqlTHMetaData fn) of
    Left e -> rethrow "getMssqlMetaTH" e
    Right xs -> return xs

-- | helper for generating metadata: see 'getMssqlMetaTH'
getSqlTHMetaData ::
  (E.MonadError SqlTHException m, S.MonadState (Set Text) m) =>
  (Text -> Text) ->
  Int ->
  GetGenericSchemaMSSQL db ->
  m MSSqlTHMetaData
getSqlTHMetaData fn i t = do
  t1 <- getCleanName fn i (rvalf #name t)
  (c, x) <- case rvalf #systype t of
    Nothing -> E.throwError $ MissingSqlMetaData [st|getSqlTHMetaData: missing type for column #{i} meta[#{show t}]|]
    Just c -> case M.lookup c systypes of
      Just x -> return (c, x)
      Nothing -> E.throwError $ MetaDataInvalidState [st|getSqlTHMetaData: programmer error? type is not in systypes column #{i} meta[#{show t}]|]
  return $ MSSqlTHMetaData t1 c (rvalf #nullable t) (rvalf #precision t) (rvalf #scale t) x

-- | helper for generating mssql metadata: see 'getMssqlMetaTH'
getMssqlMetaImpl ::
  ML e m =>
  DBMS db ->
  Sql (DBMS db) '[] '[r] ->
  m [GetGenericSchemaMSSQL db]
getMssqlMetaImpl mssqldb sql = do
  ys <- runSqlE mssqldb RNil (getMssqlSchemaSqlGeneric sql)
  when (null ys) $ U.throwIO $ InvalidSql $ sSql sql
  return ys

-- | clean up the name of the column so we can use it as a valid label in an ElField
cleanName :: Text -> Maybe Text
cleanName t =
  let t1 = T.filter (\c -> isAlphaNum c || c == '_') t
   in T.uncons t1 <&> \case
        (c, cs)
          | isLower c || c == '_' -> T.cons c cs
          | isUpper c -> T.cons (toLower c) cs
          | otherwise -> T.cons '_' (T.cons c cs)

-- | generate haskell data from metadata for a table
getSqlMetasHdbc ::
  (Text -> Text) ->
  RMeta ->
  Either SqlTHException [(String, TH.Name, Bool)]
getSqlMetasHdbc fn hms =
  flip S.evalStateT Set.empty $
    forM (itoList hms) $ uncurry (getSqlMetaHdbc fn)

-- | generate haskell data from metadata for a column
getSqlMetaHdbc ::
  (E.MonadError SqlTHException m, S.MonadState (Set Text) m) =>
  (Text -> Text) ->
  Int ->
  H.SqlColDesc ->
  m (String, TH.Name, Bool)
getSqlMetaHdbc fn i meta = do
  nm' <- getCleanName fn i (Just (T.pack $ H.colName meta))
  t <- case convertTypeMeta (H.colType meta) of
    Nothing -> E.throwError $ MissingSqlMetaData [st|getSqlMetaHdbc: missing type for column #{i} meta[#{show meta}]|]
    Just c -> return c
  b <- case H.colNullable meta of
    Nothing -> E.throwError $ MissingSqlMetaData [st|getSqlMetaHdbc: missing nullable for column #{i} meta[#{show meta}]|]
    Just c -> return c
  return (T.unpack nm', t, b)

-- | 'getCleanName' creates a valid 'TH.Name' that will work with labels and makes the fields unique
getCleanName ::
  ( E.MonadError SqlTHException m
  , S.MonadState (Set Text) m
  ) =>
  (Text -> Text) ->
  Int ->
  Maybe Text ->
  m Text
getCleanName fn i mt =
  case mt >>= cleanName . fn of
    Nothing -> tryout 2 ("anon_" <> T.pack (show i))
    Just t1 -> tryout 3 t1

-- | try getting a new variable until we find one that isnt taken already
tryout ::
  (E.MonadError SqlTHException m, S.MonadState (Set Text) m) =>
  Int ->
  Text ->
  m Text
tryout mx = go (1 :: Int)
 where
  go i t
    | i <= mx = do
        s <- S.get
        if T.toLower t `Set.member` s
          then go (i + 1) (t <> "_" <> T.pack (show i))
          else do
            id %= Set.insert (T.toLower t)
            return t
    | otherwise = do
        s <- S.get
        E.throwError $ MetaDataInvalidState [st|tryout: maxed out on different names t=#{t} mx=#{show mx} set[#{show s}]|]

-- | labelled vinyl record for mssql schema
type GetGenericSchemaMSSQL db = F '["name" ::: Maybe Text, "fulltype" ::: Maybe Text, "nullable" ::: Bool, "precision" ::: Int, "scale" ::: Int, "systype" ::: Maybe Text, "usertype" ::: Maybe Text, "isidentity" ::: Bool, "isupdateable" ::: Bool, "iscomputed" ::: Bool]

-- | 'getMssqlSchemaSqlGeneric' runs dm_exec_describe_first_result_set to parse out the metadata
getMssqlSchemaSqlGeneric :: Sql (DBMS a) '[] '[r] -> Sql (DBMS a) '[] '[Sel (GetGenericSchemaMSSQL a)]
getMssqlSchemaSqlGeneric sql' =
  let sql = T.replace "'" "''" $ flattenSql $ sSql sql'
   in mkSql
        "getMssqlSchemaSqlGeneric"
        [st|
SET NOCOUNT ON
DECLARE @sql NVARCHAR(MAX)
SET @sql = N'#{sql};'
select f.name, f.system_type_name, f.is_nullable, f.precision, f.scale, h.name, g.name, f.is_identity_column, f.is_updateable, f.is_computed_column
from  sys.dm_exec_describe_first_result_set
      (
        @sql, NULL, 0
      ) AS f
       left outer join sys.types as h on f.system_type_id=h.system_type_id and h.is_user_defined=0 and h.user_type_id=h.system_type_id
       left outer join sys.types as g on f.user_type_id=g.user_type_id
order by column_ordinal
 |]

-- | MSSql metadata for creating a table
data MSSqlMetaData = MSSqlMetaData
  { metaCol :: !Text
  , metaType :: !Text
  , metaNullable :: !Bool
  }
  deriving stock (Show, Eq)

-- | MSSql metadata using sys.dm_exec_describe_first_result_set
data MSSqlTHMetaData = MSSqlTHMetaData
  { thMetaCol :: !Text
  , thMetaSysType :: !Text
  , thMetaNullable :: !Bool
  , thMetaPrecision :: !Int
  , thMetaScale :: !Int
  , thMetaTHName :: !TH.Name
  }
  deriving stock (Show, Eq)
