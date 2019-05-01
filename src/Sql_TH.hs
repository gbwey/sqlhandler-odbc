-- add filter on column names and column type
-- bearbeiten: there is duplication here when generating signatures createSignatureFromMeta  iiMeta
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
Module      : Sql_TH
Description : Template haskell for generating Sql and Sql signatures
Copyright   : (c) Grant Weyburne, 2016
License     : GPL-3
Maintainer  : gbwey9@gmail.com

This is very experimental
-}
module Sql_TH where
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH
--import Language.Haskell.TH.Syntax
import qualified Language.Haskell.TH.Syntax as TS
import Control.Monad
import Data.List
import Data.Char
import Control.Lens.TH
import Data.Vinyl (Rec(..), ElField)
import qualified UnliftIO.Exception as UE
import DBMSSQL
import DBConn
import GConn
import qualified Data.Text as T
import Data.Text (Text)
import SqlUtils_TH
import Util
import Sql
import Data.Tagged
import Data.Proxy
import Text.Shakespeare.Text

-- | options for customizing the generated Sql functions
data GenOpts = GenOpts {
    _goEnc :: Q Type
  , _goNameFunc :: Text -> Text -- ^ pretransform the metadata column name before cleaning and unduping
  , _goDBParam :: TH.Name  -- ^ database parameter eg '''ReadOnly' '''Writeable (mkName "a")
  , _goSel :: TH.Name -- ^ '''Sel or '''SelOne
  }

makeLenses ''GenOpts

defGenOpts :: GenOpts
defGenOpts = GenOpts [t| '[] |] T.toLower (mkName "a") ''Sel

showGenOpts :: GenOpts -> Q Text
showGenOpts GenOpts {..} = do
  enc <- _goEnc
  return [st| GenOpts: dbparam=#{show _goDBParam} sel=#{show _goSel} enc=#{show enc} |]

-- | 'genMSSimple' generates a Sql function for a 'Sel' query for mssql but without any input parameters
-- ie Sql (DBMS a) '[] '[Sel ...]
genMSSimple :: String -> DBMS a -> Text -> Q [TS.Dec]
genMSSimple fn db sql = genMSWith defGenOpts fn db (const sql)

-- | 'genMS' same as 'genMSSimple' but additionally allows you to specify the DBMS parameter
genMS :: String -> DBMS a -> TH.Name -> Text -> Q [TS.Dec]
genMS fn db nm sql = genMSWith defGenOpts { _goDBParam = nm } fn db (const sql)

-- | 'genMSWith' is the most general method for creating mssql functions
-- uses the stored process describe first resultset to get the metadata for the sql query
genMSWith :: GenOpts -> String -> DBMS a -> FN1 -> Q [TS.Dec]
genMSWith opts@GenOpts {..} fn db sql = do
  txt <- showGenOpts opts
  runIO $ putStrLn $ "\ngenMSWith " ++ fn ++ " " ++ T.unpack txt
  smds <- runIO $ fs $ getMssqlMetaTH _goNameFunc db $ mkSql' @'[U0] (sql (const "null"))
  let nm = mkName fn
  encs <- _goEnc
  return [SigD nm (iiMeta (simpleTypeTH _goDBParam) _goSel encs smds), ValD (VarP nm) (NormalB (AppE (AppE (AppE (AppE (ConE 'Sql) (LitE (StringL fn))) (VarE 'defEnc)) (VarE 'defDec)) (LitE (StringL (T.unpack (sql id)))))) []]

-- | creates a ElField style Sql signature for mssql eg Sql (DBMS Writeable) '[Char,Int] '[SelOne (Rec ElField '[ "aaa" ::: Int, "bbb" ::: String, "ccc" ::: Bool ]]
iiMeta :: TH.Type -> TH.Name -> Type -> [MSSqlTHMetaData] -> Type
iiMeta tp sel encs ss =
  let z = sqlMetaToTH ss
  in AppT (AppT (AppT (ConT ''Sql) (AppT (ConT ''DBMS) tp)) encs) (AppT (AppT PromotedConsT (AppT (ConT sel) (AppT (AppT (ConT ''Rec) (ConT ''ElField)) z))) PromotedNilT)

-- just provides Rec ElField and you decide how you want to handle it! ie you can add the Sel SelOne the dbtype and extra Upds
-- means we can also later on specify (DBMS a) which we cant do with getMSType without adding type Fn a = ...

-- | 'genMSType' generates a type synonym for part of the Sql signature ie Rec ElField '[...]
genMSType :: String -> DBMS a -> FN1 -> Q [TS.Dec]
genMSType fn db sql = do
  smds <- runIO $ fs $ getMssqlMetaTH id db $ mkSql' @'[U0] (sql (const "null"))
  let z = sqlMetaToTH smds
  return [TySynD (mkName fn) [] (AppT (AppT (ConT ''Rec) (ConT ''ElField)) z)]

-- | 'sqlMetaToTH' generates an ElField type based on the all the fields in the metadata
sqlMetaToTH :: [MSSqlTHMetaData] -> Type
sqlMetaToTH = foldr (AppT . AppT PromotedConsT . sqlMetaToTHImpl) PromotedNilT

sqlMetaToTHImpl :: MSSqlTHMetaData -> Type
sqlMetaToTHImpl s = p ((if thMetaNullable s then AppT (ConT ''Maybe) else id) ret)
        where ret = ConT (thMetaTHName s)
              p = AppT (AppT (PromotedTupleT 2) (LitT (StrTyLit (T.unpack (thMetaCol s)))))

-- | 'getlen' finds the number of encodings ie number of input parameters -- most often one to one but we need to be able to override this
getlen :: Type -> Int
getlen (AppT _ xs) = 1 + getlen xs
getlen PromotedNilT = 0
getlen o = error $ "getlen: unknown type=" ++ show o


-- | generates a database connection adt from the given parameters
-- uses 'fn' as the key into the db.cfg file
genConn :: forall db a . GConn (db a) => String -> TH.Name -> Q [TS.Dec]
genConn fn nn = do
  z <- loadConnTH (Proxy @(db a)) (T.pack fn)
  let nm = mkName fn
  let tp = AppT (ConT (getDbDefault (Proxy @(db a)))) (simpleTypeTH nn)
  return [SigD nm tp, ValD (VarP nm) (NormalB z) []]

-- | 'simpleTypeTH' extracts the template haskell type from the given name: eg ''Writeable becomes ConT ''Writeable and ConT (mkName "a") becomes ConT (VarT "a")
simpleTypeTH :: TH.Name -> TH.Type
simpleTypeTH nn@(TS.Name (TS.OccName x) _) =
  case uncons x of
    Just (c,_) | isLower c -> VarT nn
    _ -> ConT nn
-- This That These: gives you fine grained control to the point where you can just provide 2 separate querys and just put in These
-- | 'FN3' use this if you total control on how the sql is generated
-- two stage process: generates sql for extracting the metadata which could be different from the runtime sql
-- if you call the first continuation with a Text that text will be ignored in the TH stage and only used at runtime
-- if you call the second continuation with a Text that text will be used in the TH stage and ignored at runtime
-- the third continuation is a combination of the above: so will use the first parameter in the TH stage and the second at runtime
-- this is not so useful for simple stuff but could be useful when
-- you have a very complex CTE query and you need limits and == some impossible value or even 2 separate queries:one for meta and the other for runtime
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
      rr = sql (const "") id (flip const) -- vice versa
  in (ll, rr)

genSql :: GConn db => String -> db -> FN1 -> Q [TS.Dec]
genSql = genSqlWith defGenOpts

genSqlWith :: GConn db => GenOpts -> String -> db -> FN1 -> Q [TS.Dec]
genSqlWith opts fn db sql =
  let limit = limitSql (tagSelf db) (Just 0)
  in genSqlLRWith opts fn db (liftSqlLR limit sql)

genSqlLR :: GConn db => String -> db -> FN3 -> Q [TS.Dec]
genSqlLR = genSqlLRWith defGenOpts

-- | gives more fine-grained control of the query into compiletime and runtime phases using FN3
genSqlLRWith :: GConn db => GenOpts -> String -> db -> FN3 -> Q [TS.Dec]
genSqlLRWith opts@GenOpts {..} fn db sql = do
  let dbname = getDbDefault (Tagged db)
  txt <- showGenOpts opts
  runIO $ putStrLn $ "\ngenSqlLRWith " ++ fn ++ " " ++ T.unpack txt
  enc <- _goEnc
  let w = AppT (AppT (ConT ''Sql) (AppT (ConT dbname) (simpleTypeTH _goDBParam))) enc
  let binders = getlen enc
  let (ll, rr) = getSqlLR sql
  ms <- runIO $ fs $ runRawCol db (replicate binders SqlNull) ll
  xxs <- forM ms $ \m ->
          case getSqlMetasHdbc _goNameFunc m of
            Left e -> UE.throwIO e
            Right smds -> return smds
  let nm = mkName fn
  let z = foldr (\a b -> AppT (AppT PromotedConsT (createSignatureFromMeta _goSel a)) b) PromotedNilT xxs
  -- this works but dont gain a lot : cant splice in the name of a function in the type declaration
  -- we can splice the name for the value itself
  -- aa :: ... [aa cannot be spliced!] but aa = [here the aa can be spliced
  -- also can bust it up into [t| |] but and [d| |] but [t| allows no splicing at all
  -- doing this doesnt gain much

  -- LitE is TH.Exp but litE is Q TH.Exp so we can use it in a splice without pure
  -- even better stringE does what we want
-- (SigD nm (AppT w z) :) <$> [d|$(pure (VarP nm)) = Sql $(pure (LitE (StringL (fn <> " (generated)")))) defEnc defDec $(pure (LitE (StringL (T.unpack rr))))|]
-- (SigD nm (AppT w z) :) <$> [d|$(varP nm) = Sql $(litE (StringL (fn <> " (generated)"))) defEnc defDec $(litE (StringL (T.unpack rr)))|]
-- reason for "This pattern-binding binds no variables:" is cos we separate out the splices
-- for SigD and [d|...|] so template haskell cant tell us if we screwed up
-- [t| |] doesnt allow splicing and cant make the typename in the signature dynamic hence we split out the SigD and the [d|..|] stuff
--  (SigD nm (AppT w z) :) <$> [d|$(varP nm) = Sql $(stringE (fn <> " (generated)")) defEnc defDec $(stringE (T.unpack rr))|]
  return [SigD nm (AppT w z), ValD (VarP nm) (NormalB (AppE (AppE (AppE (AppE (ConE 'Sql) (LitE (StringL (fn <> " (generated)")))) (VarE 'defEnc)) (VarE 'defDec)) (LitE (StringL (T.unpack rr))))) []]

{-
  -- (SigD nm (AppT w z) :) <$> [d|$(pure (VarP nm)) = Sql $(pure (LitE (StringL (fn <> " (generated)")))) defEnc defDec $(pure (LitE (StringL (T.unpack rr))))|]

C:\haskell\sqlhandler-odbc\src\Sql_TH.hs:562:33-156: warning: [-Wunused-pattern-binds]

    This pattern-binding binds no variables:
      $(pure (VarP nm))
        = Sql
            $(pure (LitE (StringL (fn <> " (generated)"))))
            defEnc
            defDec
            $(pure (LitE (StringL (T.unpack rr))))
    |
562 |   (SigD nm (AppT w z) :) <$> [d|$(pure (VarP nm)) = Sql $(pure (LitE (StringL (fn
<> " (generated)")))) defEnc defDec $(pure (LitE (StringL (T.unpack rr))))|]
    |                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}

createSignatureFromMeta :: TH.Name -> [(String, TH.Name, Bool)] -> Type
createSignatureFromMeta sel ss =
  let z = foldr (AppT . AppT PromotedConsT . ffHdbc) PromotedNilT ss
  in AppT (ConT sel) (AppT (AppT (ConT ''Rec) (ConT ''ElField)) z)

ffHdbc :: (String, TH.Name, Bool) -> Type
ffHdbc (nm, tp, b) = p ((if b then AppT (ConT ''Maybe) else id) ret)
        where ret = ConT tp
              p = AppT (AppT (PromotedTupleT 2) (LitT (StrTyLit nm)))



-- type synonym which has a list of lists of elfields: not sure how useful it is
-- need to be able to pull just the one: P.Hd or P.Lst or P.FindAt 0
-- | 'genTypeList' creates a type synonym for a promoted list of Sel
-- need specify the number of binders. For mssql it has to be exact but for the other
-- database types it has to be at least as many input parameters
genTypeList :: GConn db => Int -> String -> db -> FN1 -> Q [TS.Dec]
genTypeList = genTypeList' ''Sel

-- | 'genTypeListOne' creates a type synonym for a promoted list of SelOne
genTypeListOne :: GConn db => Int -> String -> db -> FN1 -> Q [TS.Dec]
genTypeListOne = genTypeList' ''SelOne

genTypeList' :: GConn db => TH.Name -> Int -> String -> db -> FN1 -> Q [TS.Dec]
genTypeList' sel binders fn db sql = do
  runIO $ putStrLn $ "\ngenTypeList " ++ fn ++ " " ++ show sel ++ " " ++ show (getDbDefault (Tagged db))
  let limit = limitSql (tagSelf db) (Just 0)
  ms <- runIO $ fs $ runRawCol db (replicate binders SqlNull) (sql (const limit))
  xxs <- forM ms $ \m ->
          case getSqlMetasHdbc id m of
            Left e -> UE.throwIO e
            Right smds -> return smds
  let z = foldr (\a b -> AppT (AppT PromotedConsT (createSignatureFromMeta sel a)) b) PromotedNilT xxs
  return [TySynD (mkName fn) [] z]

-- | 'genTypeFirst' creates a type synonym of the form Sel (Rec ElField '[...])
genTypeFirst :: GConn db => Int -> String -> db -> FN1 -> Q [TS.Dec]
genTypeFirst = genTypeFirst' ''Sel

-- | 'genTypeFirstOne' is the same as 'genTypeFirst' but for 'SelOne' instead of 'Sel'
genTypeFirstOne :: GConn db => Int -> String -> db -> FN1 -> Q [TS.Dec]
genTypeFirstOne = genTypeFirst' ''SelOne

-- | 'genTypeFirst'' generates a type synonym for the first resultset (fails if first is an update) and stops before running any more resultsets
genTypeFirst' :: GConn db => TH.Name -> Int -> String -> db -> FN1 -> Q [TS.Dec]
genTypeFirst' sel binders fn db sql = do
  runIO $ putStrLn $ "\ngenTypeFirst " ++ fn ++ " " ++ show sel ++ " " ++ show (getDbDefault (Tagged db))
  let limit = limitSql (tagSelf db) (Just 0)
  m <- runIO $ fs $ runRawCol1 db (replicate binders SqlNull) (sql (const limit))
  xs <- case getSqlMetasHdbc id m of
            Left e -> UE.throwIO e
            Right smds -> return smds
  return [TySynD (mkName fn) [] (createSignatureFromMeta sel xs)]
