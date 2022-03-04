{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : HSql.ODBC.Sql_TH
Description : Template haskell for generating Sql and Sql signatures
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

At compile-time gets metadata for sql select statement and generates the signature and code
for a labelled vinyl record
-}
module HSql.ODBC.Sql_TH where

import BaseUtils.Extra
import Control.Monad (forM)
import Data.Char (isLower)
import qualified Data.List as L
import Data.Proxy
import Data.Tagged
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vinyl (ElField, Rec (..))
import Database.HDBC (SqlValue (SqlNull))
import Database.MSSql (DBMS)
import GHC.Stack (HasCallStack)
import HSql.Core.Decoder (defDec)
import HSql.Core.Encoder (defEnc)
import HSql.Core.Sql
import HSql.ODBC.DBConn
import HSql.ODBC.SqlUtils_TH
import Language.Haskell.TH
import qualified Language.Haskell.TH as TH (Name, Type)
import qualified Language.Haskell.TH.Syntax as TS
import Language.Haskell.TH.Syntax.Compat (Splice, liftSplice)
import Logging
import Text.Shakespeare.Text (st)
import qualified UnliftIO.Exception as U (throwIO)
import Utils.Error

-- | options for customizing the generated Sql functions
data GenOpts = GenOpts
  { goEnc :: !(Q Type)
  , goNameFunc :: !(Text -> Text)
  -- ^ pretransform the metadata column name before cleaning and unduping
  , goDBParam :: !TH.Name
  -- ^ database parameter eg '''ReadOnly' '''Writeable (mkName "a")
  , goSel :: !TH.Name
  -- ^ '''Sel or '''SelRow
  }

-- | default 'GenOpts'
defGenOpts :: GenOpts
defGenOpts = GenOpts [t|'[]|] T.toLower (mkName "a") ''Sel

-- | show 'GenOpts' in the 'Q' monad
showGenOpts :: GenOpts -> Q Text
showGenOpts GenOpts{..} = do
  enc <- goEnc
  return [st| GenOpts: dbparam=#{show goDBParam} sel=#{show goSel} enc=#{show enc} |]

{- | 'genMSSimple' generates a Sql function for a 'Sel' query for mssql but without any input parameters
 ie Sql (DBMS a) '[] '[Sel ...]
-}
genMSSimple :: String -> DBMS a -> Text -> Q [TS.Dec]
genMSSimple fn db sql = genMSWith defGenOpts fn db (const sql)

-- | 'genMS' same as 'genMSSimple' but additionally allows you to specify the DBMS parameter
genMS :: String -> DBMS a -> TH.Name -> Text -> Q [TS.Dec]
genMS fn db nm sql = genMSWith defGenOpts{goDBParam = nm} fn db (const sql)

{- | 'genMSWith' is the most general method for creating mssql functions
 uses the stored process describe first resultset to get the metadata for the sql query
-}
genMSWith :: GenOpts -> String -> DBMS a -> FN1 -> Q [TS.Dec]
genMSWith opts@GenOpts{..} fn db sql = do
  txt <- showGenOpts opts
  runIO $ putStrLn $ "\ngenMSWith " ++ fn ++ " " ++ T.unpack txt
  smds <- runIO $ fs $ getMssqlMetaTH goNameFunc db $ mkSql' @'[U0] (sql (const "null"))
  let nm = mkName fn
  encs <- goEnc
  return [SigD nm (iiMeta (simpleTypeTH goDBParam) goSel encs smds), ValD (VarP nm) (NormalB (AppE (AppE (AppE (AppE (ConE 'Sql) (LitE (StringL fn))) (VarE 'defEnc)) (VarE 'defDec)) (LitE (StringL (T.unpack (sql id)))))) []]

-- | creates a ElField style Sql signature for mssql eg Sql (DBMS Writeable) '[Char,Int] '[SelRow (Rec ElField '[ "aaa" ::: Int, "bbb" ::: String, "ccc" ::: Bool ]]
iiMeta :: TH.Type -> TH.Name -> Type -> [MSSqlTHMetaData] -> Type
iiMeta tp sel encs ss =
  let z = sqlMetaToTH ss
   in AppT (AppT (AppT (ConT ''Sql) (AppT (ConT ''DBMS) tp)) encs) (AppT (AppT PromotedConsT (AppT (ConT sel) (AppT (AppT (ConT ''Rec) (ConT ''ElField)) z))) PromotedNilT)

-- | 'genMSType' generates a type synonym for part of the Sql signature ie Rec ElField '[...]
genMSType :: String -> DBMS a -> FN1 -> Q [TS.Dec]
genMSType fn db sql = do
  smds <- runIO $ fs $ getMssqlMetaTH id db $ mkSql' @'[U0] (sql (const "null"))
  let z = sqlMetaToTH smds
  return [TySynD (mkName fn) [] (AppT (AppT (ConT ''Rec) (ConT ''ElField)) z)]

-- | generates an ElField type based on the all the fields in the metadata
sqlMetaToTH :: [MSSqlTHMetaData] -> Type
sqlMetaToTH = foldr (AppT . AppT PromotedConsT . sqlMetaToTHImpl) PromotedNilT

-- | helper for 'sqlMetaToTH'
sqlMetaToTHImpl :: MSSqlTHMetaData -> Type
sqlMetaToTHImpl s = p ((if thMetaNullable s then AppT (ConT ''Maybe) else id) ret)
 where
  ret = ConT (thMetaTHName s)
  p = AppT (AppT (PromotedTupleT 2) (LitT (StrTyLit (T.unpack (thMetaCol s)))))

-- | 'getlen' finds the number of encodings ie number of input parameters -- most often one to one but we need to be able to override this
getlen :: HasCallStack => Type -> Int
getlen (AppT _ xs) = 1 + getlen xs
getlen PromotedNilT = 0
getlen o = normalError $ "getlen: unknown type=" ++ show o

-- https://markkarpov.com/tutorial/th.html#typed-expressions
--   It appears that returning something polymorphic is not yet possible! so have to use genConn

{- | generates a database connection adt from the given parameters
 uses the corresponding key into the conn.dhall configuration file
-}
genConn :: forall db a. GConn (db a) => String -> TH.Name -> Q [TS.Dec]
genConn key nn = do
  z <- loadConnTH (Proxy @(db a)) (T.pack key)
  let nm = mkName key
  let tp = AppT (ConT (getDbDefault (Proxy @(db a)))) (simpleTypeTH nn)
  return [SigD nm tp, ValD (VarP nm) (NormalB z) []]

{- | generates a database connection adt from the given parameters
 uses the corresponding key into the conn.dhall
-}
genConn2 :: forall db. GConn db => String -> Splice Q db
genConn2 key = liftSplice $ do
  z <- loadConnTH (Proxy @db) (T.pack key)
  return $ TS.TExp z -- return [SigD nm tp, ValD (VarP nm) (NormalB z) []]

-- | 'simpleTypeTH' extracts the template haskell type from the given name: eg ''Writeable becomes ConT ''Writeable and ConT (mkName "a") becomes ConT (VarT "a")
simpleTypeTH :: TH.Name -> TH.Type
simpleTypeTH nn@(TS.Name (TS.OccName x) _) =
  case L.uncons x of
    Just (c, _) | isLower c -> VarT nn
    _ok -> ConT nn

{- | 'FN3' use this if you total control on how the sql is generated
 two stage process: generates sql for extracting the metadata which could be different from the runtime sql
 if you call the first continuation with a Text that text will be ignored in the TH stage and only used at runtime
 if you call the second continuation with a Text that text will be used in the TH stage and ignored at runtime
 the third continuation is a combination of the above: so will use the first parameter in the TH stage and the second at runtime
 this is not so useful for simple stuff but could be useful when
 you have a very complex CTE query and you need limits and == some impossible value or even 2 separate queries:one for meta and the other for runtime
-}
type FN3 = (Text -> Text) -> (Text -> Text) -> (Text -> Text -> Text) -> Text

-- | 'FN1' in the TH stage uses a sql limit clause instead of the text passed to the function but uses that text in the runtime stage
type FN1 = (Text -> Text) -> Text

-- | 'liftSqlLR' convenience function that lifts a 'FN1' to 'FN3'
liftSqlLR :: Text -> FN1 -> FN3
liftSqlLR txt sql _ _ lr = lr (sql (const txt)) (sql id)

-- | lhs is the sql values for compile time (TH phase) and rhs is for runtime sql
getSqlLR :: FN3 -> (Text, Text)
getSqlLR sql =
  let ll = sql id (const "") const -- uses the left and ignores data from the right
      rr = sql (const "") id (const id) -- vice versa
   in (ll, rr)

-- | run 'genSqlWith' using default options
genSql :: GConn db => String -> db -> FN1 -> Q [TS.Dec]
genSql = genSqlWith defGenOpts

-- | creates a splice that contains code for a labelled vinyl record select statement including column metadata
genSqlWith :: GConn db => GenOpts -> String -> db -> FN1 -> Q [TS.Dec]
genSqlWith opts fn db sql =
  let limit = limitSql (tagSelf db) (Just 0)
   in genSqlLRWith opts fn db (liftSqlLR limit sql)

-- | calls 'genSqlLRWith' using default 'GenOpts'
genSqlLR :: GConn db => String -> db -> FN3 -> Q [TS.Dec]
genSqlLR = genSqlLRWith defGenOpts

-- | gives more fine-grained control of the query into compiletime and runtime phases using FN3
genSqlLRWith :: GConn db => GenOpts -> String -> db -> FN3 -> Q [TS.Dec]
genSqlLRWith opts@GenOpts{..} fn db sql = do
  let dbname = getDbDefault (Tagged db)
  txt <- showGenOpts opts
  runIO $ putStrLn $ "\ngenSqlLRWith " ++ fn ++ " " ++ T.unpack txt
  enc <- goEnc
  let w = AppT (AppT (ConT ''Sql) (AppT (ConT dbname) (simpleTypeTH goDBParam))) enc
  let binders = getlen enc
  let (ll, rr) = getSqlLR sql
  ms <- runIO $ fs $ runRawCol db (replicate binders SqlNull) ll
  xxs <- forM ms $ \m ->
    case getSqlMetasHdbc goNameFunc m of
      Left e -> rethrow "genSqlLRWith" e
      Right smds -> return smds
  let nm = mkName fn
  let z = foldr (AppT . AppT PromotedConsT . createSignatureFromMeta goSel) PromotedNilT xxs
  return [SigD nm (AppT w z), ValD (VarP nm) (NormalB (AppE (AppE (AppE (AppE (ConE 'Sql) (LitE (StringL (fn <> " (generated)")))) (VarE 'defEnc)) (VarE 'defDec)) (LitE (StringL (T.unpack rr))))) []]

-- | create a signature from the sql metadata
createSignatureFromMeta :: TH.Name -> [(String, TH.Name, Bool)] -> Type
createSignatureFromMeta sel ss =
  let z = foldr (AppT . AppT PromotedConsT . ffHdbc) PromotedNilT ss
   in AppT (ConT sel) (AppT (AppT (ConT ''Rec) (ConT ''ElField)) z)

-- | create a type based on name of the field and type
ffHdbc :: (String, TH.Name, Bool) -> Type
ffHdbc (nm, tp, b) = p ((if b then AppT (ConT ''Maybe) else id) ret)
 where
  ret = ConT tp
  p = AppT (AppT (PromotedTupleT 2) (LitT (StrTyLit nm)))

{- | 'genTypeList' creates a type synonym for a promoted list of Sel
 need specify the number of binders. For mssql it has to be exact but for the other
 database types it has to be at least as many input parameters
-}
genTypeList :: GConn db => String -> db -> FN1 -> Q [TS.Dec]
genTypeList = genTypeList' ''Sel

-- | 'genTypeListOne' creates a type synonym for a promoted list of SelRow
genTypeListOne :: GConn db => String -> db -> FN1 -> Q [TS.Dec]
genTypeListOne = genTypeList' ''SelRow

-- | generates a type signature and code for constrained sql: use this for non-MSSQL databases
genTypeList' :: GConn db => TH.Name -> String -> db -> FN1 -> Q [TS.Dec]
genTypeList' sel fn db sqlfn = do
  runIO $ putStrLn $ "\ngenTypeList " ++ fn ++ " " ++ show sel ++ " " ++ show (getDbDefault (Tagged db))
  let limit = limitSql (tagSelf db) (Just 0)
  let sql = sqlfn (const limit)
  ms <- runIO $ fs $ runRawCol db (replicate (countBinders sql) SqlNull) sql
  xxs <- forM ms $ \m ->
    case getSqlMetasHdbc id m of
      Left e -> rethrow "genTypeList'" e
      Right smds -> return smds
  let z = foldr (AppT . AppT PromotedConsT . createSignatureFromMeta sel) PromotedNilT xxs
  return [TySynD (mkName fn) [] z]

-- | 'genTypeFirst' creates a type synonym of the form Sel (Rec ElField '[...])
genTypeFirst :: GConn db => String -> db -> FN1 -> Q [TS.Dec]
genTypeFirst = genTypeFirst' ''Sel

-- | 'genTypeFirstOne' is the same as 'genTypeFirst' but for 'SelRow' instead of 'Sel'
genTypeFirstOne :: GConn db => String -> db -> FN1 -> Q [TS.Dec]
genTypeFirstOne = genTypeFirst' ''SelRow

-- | 'genTypeFirst'' generates a type synonym for the first resultset (fails if first is an update) and stops before running any more resultsets
genTypeFirst' :: GConn db => TH.Name -> String -> db -> FN1 -> Q [TS.Dec]
genTypeFirst' sel fn db sqlfn = do
  runIO $ putStrLn $ "\ngenTypeFirst " ++ fn ++ " " ++ show sel ++ " " ++ show (getDbDefault (Tagged db))
  let limit = limitSql (tagSelf db) (Just 0)
  let sql = sqlfn (const limit)
  ms <- runIO $ fs $ runRawCol db (replicate (countBinders sql) SqlNull) sql
  m <- case ms of
    [] -> U.throwIO $ GBException [st|genTypeFirst': no resultset found and so no metadata|]
    r : _ -> pure r
  xs <- case getSqlMetasHdbc id m of
    Left e -> rethrow "genTypeFirst'" e
    Right smds -> return smds
  return [TySynD (mkName fn) [] (createSignatureFromMeta sel xs)]

-- todo: eventually use sql parser

-- | count the number of binders in sql
countBinders :: Text -> Int
countBinders = T.length . T.filter (== '?')
