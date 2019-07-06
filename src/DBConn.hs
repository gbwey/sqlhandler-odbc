{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS -Wall #-}
-- {-# OPTIONS -Wno-redundant-constraints #-}
{- |
Module      : DBConn
Description : Contains methods for running sql against databases
Copyright   : (c) Grant Weyburne, 2016
License     : GPL-3
Maintainer  : gbwey9@gmail.com

Generic methods for running sql / comparing databases / printing and logging.
-}
module DBConn where
import Control.Monad.Logger
import Control.Monad.IO.Class
import Data.Time
import Prelude hiding (FilePath)
import Text.Shakespeare.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Control.Monad
import qualified Database.HDBC as H
import qualified Database.HDBC.ODBC as H
import Database.HDBC (SqlValue(..))
import Data.List
import Control.Arrow
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import Data.Function
import Data.Ord
import Util
import Data.These
import qualified Formatting as F
import Formatting ((%.),(%))
import Data.Typeable
import GConn
import Data.Data
import Data.Text.Lazy.Builder (fromText)
import Control.Lens
import Sql
import GHC.Stack
import Data.Tuple.Extra (dupe)
import Data.Vinyl
import qualified Data.Vinyl as V
import qualified Data.Vinyl.Functor as V
import Data.Vinyl.TypeLevel
import qualified UnliftIO.Exception as UE
import qualified PCombinators as P
import GHC.TypeLits
import qualified Language.Haskell.TH as TH
import Data.UUID (UUID)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe

type HSQL = [(Integer, Maybe [[SqlValue]])]

newtype HConn a = HConn H.Connection deriving H.IConnection

-- | 'SqlPairRW' contains name of a table and the sql needed to create that table
type SqlPairRW db dbw a b = GConnWrite dbw => (Table dbw, Maybe (Sql db a b))

mkSqlPair :: GConnWrite dbw => Table dbw -> Sql db a b -> SqlPairRW db dbw a b
mkSqlPair table sql = (table, Just sql)

-- | 'TVTable' distinguishes between tables and views
data TVTable = TVTable | TVView deriving (Show,Eq)

isTable :: TVTable -> Bool
isTable TVTable = True
isTable TVView = False

data CreateTable = CreateTable | SkipCreate deriving (Show,Eq)

instance ToText CreateTable where
  toText = fromText . T.pack . show

{-
old code had a splitter to bust up dml sql
ERROR: H.SqlError {seState = "[\"42000\",\"42000\"]", seNativeError = -1, seErrorMsg = "execute execute: [\"111: [Microsoft][SQL Server Native Client 11.0][SQL Server]'CREATE/ALTER PROCEDURE' must be the first statement in a query batch.\",\"8180: [Microsoft][SQL Server Native Client 11.0][SQL Server]
Statement(s) could not be prepared.\"]"}
-}

-- |'RunSqlOK' checks to see that we are not trying to write to a readonly database
type RunSqlOk b db = P.FailUnless (P.Impl (WriteableRS b) (WriteableDB db)) ('Text "Readonly database does not allow Upd") P.Mempty -- (() :: Constraint)

-- |'TSql' has a list of constraints for that need to be fulfilled by runSql*
type TSql b a = (RecAll ZZZ b SingleZ
               , V.ReifyConstraint Show V.Identity a
               , V.RecordToList a
               , V.RMap a
               , ValidateNested b)

-- |'runSql' executes the sql for typed input and output
runSql :: (TSql b a
         , ML e m
         , GConn db
         , RunSqlOk b db
         )
        => db -> Rec V.Identity a -> Sql db a b -> m (Rec ZZZ b)
runSql = runSqlUnsafe

-- |'runSql_' is the same as 'runSql' but throws away the output
runSql_ :: (TSql b a
         , ML e m
         , GConn db
         , RunSqlOk b db
         )
        => db -> Rec V.Identity a -> Sql db a b -> m ()
runSql_ a b c = void $ runSqlUnsafe a b c

-- order is important for using @ b then a
-- | 'runSqlReadOnly' runs a typed query but explicitly requires the sql to be a non-update
runSqlReadOnly :: (TSql b a
                 , WriteableRS b ~ 'False
                 , ML e m
                 , GConn db
                 )
                => db -> Rec V.Identity a -> Sql db a b -> m (Rec ZZZ b)
runSqlReadOnly = runSqlUnsafe

-- | 'runSqlReadOnly' runs a typed query but doesnt check RunSqlOk
runSqlUnsafe :: (TSql b a
               , ML e m
               , GConn db
                 ) => db -> Rec V.Identity a -> Sql db a b -> m (Rec ZZZ b)
runSqlUnsafe db vals sql = withDB db $ \cany -> runSqlI cany vals sql

-- | 'runSqlI' is like 'runSql' but reuses the connection from 'withDB'
runSqlI :: (TSql b a
          , ML e m) =>
            HConn db -> Rec V.Identity a -> Sql db a b -> m (Rec ZZZ b)
runSqlI cany vals (Sql desc enc dec sql) = do
  let hs = encodeVals enc vals
  $logDebug [st|runSqlI: #{desc} encoded vals=#{show hs}|]
  rrs <- runSqlRawI desc cany hs sql
  case processRet' (toZZZ dec) rrs of
    Left es -> do
                logSqlHandlerExceptions es
                UE.throwIO $ GBException [st|runSqlI #{desc} #{showSE es} vals=#{show vals} sql=#{sql}|]
    Right zzz -> return zzz

logSqlHandlerExceptions :: ML e m => SE -> m ()
logSqlHandlerExceptions es = do
  let len = length es
  forM_ (itoList es) $ \(i,e) -> $logError [st|#{succ i} of #{len}: #{seShortMessage e}|]


-- |'runSqlCol' is the same as 'runSql' but adds meta data to the query ouput
runSqlCol :: (TSql b a
            , RunSqlOk b db
            , ML e m
            , GConn db
            ) => db -> Rec V.Identity a -> Sql db a b -> m (Rec ZZZ b)
runSqlCol = runSqlColUnsafe -- db vals sql = withDB db $ \cany -> runSqlI cany vals sql

-- |'runSqlColUnsafe' is the same as 'runSqlUnsafe' but adds meta data to the query ouput
runSqlColUnsafe :: (TSql b a
                  , ML e m
                  , GConn db
                  ) => db -> Rec V.Identity a -> Sql db a b -> m (Rec ZZZ b)
runSqlColUnsafe db vals sql = withDB db $ \cany -> runSqlColI cany vals sql

-- | 'runSqlColI' is like 'runSqlI' but add meta data to the query output
runSqlColI :: (TSql b a
             , ML e m
             , GConn db
             ) => HConn db -> Rec V.Identity a -> Sql db a b -> m (Rec ZZZ b)
runSqlColI cany vals (Sql desc enc dec sql) = do
  let hs = encodeVals enc vals
  $logDebug [st|runSqlColI: #{desc} encoded vals=#{show hs}|]
  rrs <- runSqlRawColI desc cany hs sql
  case processRetCol' (toZZZ dec) rrs of
    Left es -> do
                logSqlHandlerExceptions es
                UE.throwIO $ GBException [st|runSqlColI #{desc} #{showSE es} vals=#{show vals} sql=#{sql}|]
    Right zzz -> return zzz

-- | 'runSqlRawI runs an untyped query
runSqlRaw :: (ML e m, GConn db) => db -> [SqlValue] -> Text -> m [HRet]
runSqlRaw db hs sql = withDB db $ \cany -> runSqlRawI "runSqlRaw" cany hs sql

-- | 'runSqlRawI' runs an untyped query using an existing connection using 'withDB'
runSqlRawI :: (ML e m) => String -> HConn db -> [SqlValue] -> Text -> m [HRet]
runSqlRawI desc cany hs sql = do
  let callback stmt i = do
                          $logDebug [st|runSqlRawI: #{desc} callback i=#{i}|]
                          liftIO (H.fetchAllRows' stmt)
  $logDebug [st|runSqlRawI: #{desc} encoded hs=#{show hs}|]
  runSqlImpl desc callback cany hs (T.unpack sql)

-- | 'runSqlRawCol' runs an untyped query with metadata
runSqlRawCol :: (ML e m, GConn db) => db -> [SqlValue] -> Text -> m [HRetCol]
runSqlRawCol db hs sql = withDB db $ \cany -> runSqlRawColI "runSqlRawCol" cany hs sql

-- | 'runSqlRawColI' runs an untyped query with metadata using an existing connection using 'withDB'
runSqlRawColI :: ML e m => String -> HConn db -> [SqlValue] -> Text -> m [HRetCol]
runSqlRawColI desc cany hs sql = do
  let callback stmt i = do
                          $logDebug [st|runSqlRawColI: #{desc} callback i=#{i}|]
                          liftIO $ do
                                      m <- H.describeResult stmt
                                      r <- H.fetchAllRows' stmt
                                      return (m,r)
  $logDebug [st|runSqlRawColI: #{desc} encoded hs=#{show hs}|]
  runSqlImpl desc callback cany hs (T.unpack sql)

-- | 'runSqlImpl' runs an untyped query where you pass in a callback that does the work
runSqlImpl :: (ML e m, H.IConnection b) => String -> (H.Statement -> Int -> m ret) -> b -> [SqlValue] -> String -> m [Either Int ret]
runSqlImpl desc callback conn ps sql = do
  -- dt <- liftIO getZonedTime
  $logDebug [st|runSqlImpl: running #{desc} sql=#{newline}#{sql}|]
  UE.bracket (liftIO $ H.prepare conn sql) (liftIO . H.finish) $ \stmt -> do
    rc <- liftIO $ H.execute stmt ps
    let go _ Nothing = return []
        go !i (Just Nothing) = do
                        $logDebug [st|runSqlImpl: Result set! i=#{i}|]
                        r <- callback stmt i
--                        $logDebug [st|runSqlImpl fetchAllRows' #{show r}|]
                        zz <- liftIO $ H.nextResultSet stmt
--                        $logDebug [st|runSqlImpl nextResultSet' #{show zz}|]
                        yy <- go (i+1) zz
                        return $ Right r: yy
        go !i (Just (Just n)) = do
                        $logDebug [st|runSqlImpl: Update statement! ie rc==#{show n} i=#{i}|]
                        zz <- liftIO $ H.nextResultSet stmt
--                        $logDebug [st|runSqlImpl nextResultSet' #{show zz}|]
                        yy <- go (i+1) zz
                        return $ Left n: yy
    go 0 (Just rc)

-- | 'runRawCol' just returns the metadata for the first resultset and ignores the rest used by TH
runRawCol :: (ML e m, GConn db) => db -> [SqlValue] -> Text -> m [HMeta]
runRawCol db hs sql = withDB db $ \cany -> runRawCol' "runRawCol" cany hs sql

-- still runs the query! so we still have to specify limit 0/top 0
runRawCol' :: ML e m => String -> HConn db -> [SqlValue] -> Text -> m [HMeta]
runRawCol' desc cany hs sql = do
  $logDebug [st|runRawCol': #{desc} encoded hs=#{show hs}|]
  runSqlMetaImpl True desc cany hs (T.unpack sql)

-- | 'runRawCol1' gets metadata for the first and only result set: used by TH
runRawCol1 :: (ML e m, GConn db) => db -> [SqlValue] -> Text -> m HMeta
runRawCol1 db hs sql = withDB db $ \cany -> runRawCol1' "runRawCol1" cany hs sql

-- still runs the query! so we still have to specify limit 0/top 0
runRawCol1' :: ML e m => String -> HConn db -> [SqlValue] -> Text -> m HMeta
runRawCol1' desc cany hs sql = do
  $logDebug [st|runRawCol1': #{desc} encoded hs=#{show hs}|]
  rs <- runSqlMetaImpl False desc cany hs (T.unpack sql)
  case rs of
    [r] -> return r
    [] -> UE.throwIO $ GBException [st|runRawCol1': no resultset found|]
    _:_:_ -> UE.throwIO $ GBException [st|runRawCol1': expected only one resultset but found #{length rs}: rs=#{show rs}|]

-- | 'runSqlMetaImpl' returns the metadata
runSqlMetaImpl :: (ML e m, H.IConnection b) => Bool -> String -> b -> [SqlValue] -> String -> m [HMeta]
runSqlMetaImpl domany desc conn ps sql = do
  $logDebug [st|runSqlMetaImpl: running #{desc} sql=#{newline}#{sql}|]
  UE.bracket (liftIO $ H.prepare conn sql) (liftIO . H.finish) $ \stmt -> do
    rc <- liftIO $ H.execute stmt ps
    let go _ Nothing = return []
        go !(i::Int) (Just Nothing) = do
                        $logDebug [st|runSqlImpl: Result set! i=#{i}|]
                        r <- liftIO $ H.describeResult stmt
                        if domany then do
                          zz <- liftIO $ H.nextResultSet stmt
                          yy <- go (i+1) zz
                          return (r:yy)
                        else return [r]
        go !i (Just (Just n)) =
                        UE.throwIO $ GBException [st|runSqlMetaImpl: Update not allowed! rc==#{show n} i=#{i}|]
    go 0 (Just rc)

-- dont need to unwrap HConn cos made it an instance of H.IConnection so it is isomorphic
-- | 'withDB' opens a database connection and allows the caller to run commands using the connection and closes the connection at the end
withDB :: forall m e a b . (ML e m, GConn a) => a -> (HConn a -> m b) -> m b
withDB db fn =
  bracketDB db $ \canyx ->
    withTransaction canyx $ \cany -> fn cany

-- | 'bracketDB' handles closing the connection. If there are errors then they are logged
bracketDB :: (ML e m, GConn a) => a -> (HConn a -> m c) -> m c
bracketDB db fn =
  let ma = UE.bracket (HConn <$> liftIO (getconn db))
             (\c -> UE.handle (\(e :: H.SqlError) -> if ignoreDisconnectError (Just db) then
                                                        $logWarn [st|bracketDB: #{show e} db=#{showDb db}|]
                                                     else do
                                                        $logError [st|bracketDB: #{show e} db=#{showDb db}|]
                                                        UE.throwIO e
                               )
                               (liftIO (H.disconnect c))
             ) fn
  in UE.withException ma (\(e::H.SqlError) -> $logError [st|bracketDB: #{show e} db=#{showDb db}|])

allTablesCount' :: forall a m e . (GConn a, ML e m) => a -> m [(Table a, Int, Maybe UTCTime, Maybe UTCTime)]
allTablesCount' = allTablesCount (const True)

-- | 'allTablesCount' returns a list of table plus number of rows using a predicate
allTablesCount :: (GConn a, ML e m) => (Table a -> Bool) -> a -> m [(Table a, Int, Maybe UTCTime, Maybe UTCTime)]
allTablesCount p db =
  case getAllTablesCountSql (Just db) of
    Just s -> do
      xs <- ext <$> runSql db RNil s
      return $ map (\z -> (rvalf #name z, rvalf #size z, rvalf #created z, rvalf #updated z)) xs
    Nothing -> do
      ts <- allTables p db
      forM ts $ \t -> do
        lrn <- UE.try $ getOneTableRowCount db t
        case lrn of
          Left (e :: UE.SomeException) -> do
                  $logWarn [st|allTablesAndViewsCount: ignoring exception #{show e}|]
                  return (t,-1,Nothing,Nothing)
          Right n -> return (t,n,Nothing,Nothing)

allViews :: forall a m e . (GConn a, ML e m) => (Table a -> Bool) -> a -> m [Table a]
allViews = allTablesAndViews TVView

allTables :: forall a m e . (GConn a, ML e m) => (Table a -> Bool) -> a -> m [Table a]
allTables = allTablesAndViews TVTable

allTables' :: forall a m e . (GConn a, ML e m) => a -> m [Table a]
allTables' = allTables (const True)

allTablesAndViews :: forall a m e . (GConn a, ML e m) => TVTable -> (Table a -> Bool) -> a -> m [Table a]
allTablesAndViews tv p db = do
  vs <- ext <$> runSql db RNil ((if isTable tv then getAllTablesSql else getAllViewsSql) db)
  return $ filter p $ map (\t -> t { _tTable = isTable tv }) vs

-- can use with wprint
getAllTablesSqlImpl :: forall db m e . (ML e m, GConn db)
  => db -> m (Rec ZZZ '[Sel (Table db)])
getAllTablesSqlImpl db = runSql db RNil $ getAllTablesSql db

-- can use with wprint
getAllViewsSqlImpl :: forall db m e . (ML e m, GConn db)
  => db -> m (Rec ZZZ '[Sel (Table db)])
getAllViewsSqlImpl db = runSql db RNil $ getAllViewsSql db

-- todo: do we use this?
{-
allTableCountsPrint :: (GConn a, ML e m) => (Table a -> Bool) -> a -> m [(Table a, Int)]
allTableCountsPrint p anydb = do
  xs <- allTablesCount p anydb
  return $ filter ((/=0) . snd) $ sortBy (flip (comparing snd)) xs
-}

-- doesnt work for oracle ie will try to drop or will fail!
-- | 'dropTable' is a convenience method for dropping a table
dropTable :: (ML e m, GConnWrite db) => db -> Table db -> m ()
dropTable db table =
  void $ runSqlUnsafe db RNil $ dropTableIfExistsSql db table

existsTable :: (GConn a, ML e m) => a -> Table a -> m Bool
existsTable db table = do
  ts <- ext <$> runSql db RNil (existsTableSql db table)
  if ts == found then do
     $logDebug [st|found #{table} db=#{showDb db}|]
     return True
  else if ts == notfound then do
     $logWarn [st|could not find #{table} db=#{showDb db}|]
     return False
  else UE.throwIO $ GBException [st|unexpected value found[#{ts}] table[#{table}]|]

-- | my version of the withTransaction from hdbc: need to do this to lift over ML
withTransaction :: (ML e m, H.IConnection conn) => conn -> (conn -> m a) -> m a
withTransaction conn func =
    do r <- UE.onException (func conn) doRollback
       liftIO $ H.commit conn
       return r
    where doRollback =
              -- Discard any exception from (rollback conn) so original
              -- exception can be re-raised
              UE.catch (liftIO $ H.rollback conn) doRollbackHandler
          doRollbackHandler (_ :: E.SomeException) = return ()

-- | 'compareDatabase' compares tables in two databases by name and count of rows. in A and in B / in A only / in B only -- ie These
compareDatabase :: (GConn a, GConn b, ML e m) => a -> b -> m [These (Table a, Int) (Table b, Int)]
compareDatabase = compareDatabaseImpl (Just . T.toLower . _tName) (Just . T.toLower . _tName)

compareDatabaseImpl :: (GConn a, GConn b, ML e m) => (Table a -> Maybe Text) -> (Table b -> Maybe Text) -> a -> b -> m [These (Table a, Int) (Table b, Int)]
compareDatabaseImpl p1 p2 db1 db2 = do
  tp1 <- fmap (\(a,b,_,_) -> (a,b)) <$> allTablesCount (isJust . p1) db1
  tp2 <- fmap (\(a,b,_,_) -> (a,b)) <$> allTablesCount (isJust . p2) db2
  let xt = let cmp p = fromJust . p . fst
           in either (cmp p1) (cmp p2)
  let ret = divvyKeyed xt tp1 tp2
  mapM_ ($logInfo . TL.toStrict) (compareIt ret)
  return ret

compareIt :: (GConn a, GConn b) => [These (Table a, Int) (Table b, Int)] -> [TL.Text]
compareIt tps =
  let (bb, (ll, rr)) = partitionThese tps
      (oks, errs) = partition (uncurry (==) . (snd *** snd)) bb
      (lterrs,gterrs) = partition (uncurry (<) . (snd *** snd)) errs
      padn :: GConn x => Table x -> TL.Text
      padn = F.format (F.right 50 ' ') . showTable
      padi = F.format (F.right 10 ' ')
      f (a,i) = F.format (F.text % " " % F.int) (padn a) i
  in pure [lt|left only #{length ll}|]
            <> map f ll
            <> pure [lt|right only #{length rr}|]
            <> map f rr
            <> pure [lt|both and all good #{length oks}|]
            <> map (f . fst) oks
            <> pure [lt|both but with errors LT #{length lterrs}|]
            <> map (\((a,i),(_,j)) -> [lt|#{padn a} #{padi i} #{padi j}|]) lterrs
            <> pure [lt|both but with errors GT #{length gterrs}|]
            <> map (\((a,i),(_,j)) -> [lt|#{padn a} #{padi i} #{padi j}|]) gterrs
            <> pure [lt|Left=#{length ll} Right=#{length rr} Ok=#{length oks} Errs=#{length errs} LTErrs=#{length lterrs} GTErrs=#{length gterrs}|]


-- | 'logDatabaseAll' logs table name and row counts for a given database
logDatabaseAll :: (GConn a, ML e m) => a -> m ()
logDatabaseAll = logDatabase "" (const True)

logDatabase :: (GConn a, ML e m) => Text -> (Table a -> Bool) -> a -> m ()
logDatabase txt p anydb = do
  tps <- allTablesCount p anydb
  when (null tps) $ UE.throwIO $ GBException [st|logDatabase has no tables after filtering: #{txt}|]
  mapM_ ($logInfo . TL.toStrict) (logDatabaseImpl txt anydb tps)

logDatabaseImpl :: GConn a => Text -> a -> [(Table a, Int, Maybe UTCTime, Maybe UTCTime)] -> [TL.Text]
logDatabaseImpl txt anydb (fmap (\(a,b,_,_) -> (a,b)) -> tps) =
  let len = T.length $ maximumBy (comparing T.length) $ map (showTable . view _1) tps
      (x1,x2,x3) = (5,4,8)
      len2 = x1 + len + x2 + x3
      pad = TL.replicate (fromIntegral len2) "-"
      ff j (tab,i) = F.format ((F.left 5 ' ' %. F.int) % " " % F.right (len+4) ' ' % (F.left 8 ' ' %. F.int)) j (showTable tab) i
  in pure pad
         <> pure [lt|start logDatabase [#{txt}] #{showDb anydb} #{length tps} tables|]
         <> pure pad
         <> zipWith ff [1 :: Int ..] tps
         <> pure pad
         <> zipWith ff [1 :: Int ..] (sortOn (\(x,y) -> (Down y, T.toLower (_tName x))) tps)
         <> pure [lt|end logDatabase [#{txt}] #{showDb anydb} #{length tps} tables|]

prtDiff :: (GConn a, GConn b) => (TP, These (Table a) (Table b)) -> ((Int,These (Table a) (Table b)),TL.Text)
prtDiff (tp,tab) =
  let (x0,x3,len) = (14,10,55)
      t = these showTable showTable (const showTable) tab
      fmt :: Text -> Int -> TL.Text
      fmt ts = F.format ((F.right x0 '_' %. F.stext) % F.right len ' ' % (F.left x3 ' ' %. F.int)) ts t
      fmtend avg n2 = F.format ((F.left x3 ' ' %. F.int) % (F.left x3 ' ' %. F.int) % "%") n2 (-avg)
      txt = case tp of
               LeftOnly n       -> fmt "Left only" n
               RightOnly n      -> fmt "Right only" n
               LEmpty n         -> fmt "Left empty" n
               REmpty n         -> fmt "Right empty" n
               Less (avg,n1,n2) -> fmt "Less" n1 <> fmtend n2 avg
               More (avg,n1,n2) -> fmt "More" n1 <> fmtend n2 avg
               Same n           -> fmt "Same" n
  in ((constrIndex (toConstr tp),tab),txt)
-- in order of badness  -- have to make Less and More more negative for sorting cos the worst is higher values
-- tuple with tablename to get (TP,Text) and then sort on this
data TP = LeftOnly !Int
        | REmpty !Int
        | RightOnly !Int
        | LEmpty !Int
        | Less !(Int,Int,Int) -- -ve values for sorting
        | More !(Int,Int,Int) -- -ve values for sorting
        | Same !Int -- defaults to by tablename cos mostly zero
        deriving (Show,Eq,Ord,Data)

logDiffDatabase :: (ML e m, GConn a, GConn b) => a -> b -> m ()
logDiffDatabase db1 db2 = do
  txt <- diffDatabase db1 db2
  mapM_ ($logInfo . TL.toStrict) txt

diffDatabase :: (ML e m, GConn a, GConn b) => a -> b -> m [TL.Text]
diffDatabase db1 db2 = do
  xs <- allTablesCount (const True) db1
  ys <- allTablesCount (const True) db2
  let cs = diffDatabase' xs ys
  let hdr2 :: TL.Text -> TL.Text
      hdr2 txt = [lt|#{txt} logDiffDatabase #{db1} #{db2} #{length cs} db1=#{length xs} db2=#{length ys} tables|]
  let hdr = TL.replicate (TL.length (hdr2 "start")) "-"
  return $   pure hdr
          <> pure (hdr2 "start")
          <> cs
          <> pure (hdr2 "end  ")
          <> pure hdr

diffDatabase' :: HasCallStack => (GConn a, GConn b) => [(Table a,Int,Maybe UTCTime,Maybe UTCTime)] -> [(Table b,Int,Maybe UTCTime,Maybe UTCTime)] -> [TL.Text]
diffDatabase' xs ys =
  let fmat = either (_tName . view _1) (_tName . view _1)
      as = sortOn fmat (map Left xs <> map Right ys)
      bs = groupBy (on (==) fmat) as
      cs = flip map bs $ \x ->
              case sort x of
                [Left (t1,n1,_,_),Right (t2,n2,_,_)] | _tName t1/= _tName t2 -> error "difDatabase': oops"
                                             | otherwise -> (handleBoth (n1,n2), These t1 t2)
                [Left (t,n,_,_)]  -> (LeftOnly n,This t)
                [Right (t,n,_,_)] -> (RightOnly n,That t)
                o -> error $ "diffDatabase': " ++ show o
      ds = groupBy (on (==) fst) $ sortOn fst $ map prtDiff cs
  in (concatMap.map) snd ds

handleBoth :: (Int,Int) -> TP
handleBoth (n1,n2) =
  case (n1,n2) of
    (0,0) -> Same 0
    (0,_) -> LEmpty n2
    (_,0) -> REmpty n1
    _  -> let avg = round ((100 * (fromIntegral (n2-n1)/fromIntegral n1)) :: Double)
          in case compare n1 n2 of
               LT -> Less (-avg,n1,n2)
               GT -> More (-avg,n1,n2)
               EQ -> Same n1

flattenSql :: Text -> Text
flattenSql zs = cv $ T.replace "\r" " " $ T.replace "\n" " " $ T.strip zs
  where cv xs = let r = T.replace "  " " " xs in if r==xs then r else cv r

toBcpFromSql :: HasCallStack => SqlValue -> ByteString
toBcpFromSql = \case
    SqlByteString bs -> bs
    SqlNull -> ""
    SqlString ss -> B8.pack ss
    SqlInteger i -> B8.pack (show i)
    SqlInt64 i -> B8.pack (show i)
    SqlInt32 i -> B8.pack (show i)
    SqlDouble d -> B8.pack (show d)
    SqlChar c -> B8.pack (show c)
    SqlUTCTime dt -> B8.pack (formatTime defaultTimeLocale "%F %T" dt)
    SqlLocalDate dt -> B8.pack (formatTime defaultTimeLocale "%F %T" dt)
    SqlLocalTime dt -> B8.pack (formatTime defaultTimeLocale "%F %T" dt)
    SqlZonedTime dt -> B8.pack (formatTime defaultTimeLocale "%F %T" dt)
    o -> error $ T.unpack [st|copyDBSqltoBCPFile: dont know how to handle [#{show o}]|]

createDBTableFrom :: (ML e m, GConn src, GConnWrite tgt) => src -> Table src -> tgt -> Table tgt -> m ()
createDBTableFrom srcdb tabin tgtdb tabout = do
  meta <- getColumnInfo srcdb tabin
  void $ runSqlUnsafe tgtdb RNil (createDBTableFromSql meta tabout)

-- todo: can we have table with no fields in any db (apparently in postgres you can)
createDBTableFromSql :: (HasCallStack, GConn tgt, WriteableDB tgt ~ 'True) => [(ColDataType,ColumnMeta)] -> Table tgt -> Sql tgt '[] '[Upd]
createDBTableFromSql [] _ = error "how does this happen!!! should be stopped before here"
createDBTableFromSql zs tabout =
  let mid (cd,meta) = escapeField tabout (cName meta)
            <> " "
            <> translateColumnMeta tabout (cd,meta)
            <> " "
            <> if cIsNull meta then "null" else "not null"
            <> " "
  in mkSql "createDBTableFromSql" [st|create table #{tabout}
                (
                  #{T.intercalate "\n  , " (map mid zs)}
                )
             |]
getDBSelectSql :: GConn src => [ColumnMeta] -> Table src -> Text
getDBSelectSql meta srctable =
  [st|select #{T.intercalate "," (map (escapeField srctable . cName) meta)} from #{srctable}|]

getDBInsertSql :: GConnWrite tgt => [ColumnMeta] -> Table tgt -> Text
getDBInsertSql meta tgttable =
  let xs = map (escapeField tgttable . cName) meta
  in [st|insert into #{tgttable} #{vvs xs} values#{qqs meta}|]

getOneTableRowCount :: forall m db e . (GConn db, ML e m) => db -> Table db -> m Int
getOneTableRowCount anydb table =
  ext <$> runSql anydb RNil (mkSql ("getOneTableRowCount " ++ show table) [st|select count(*) from #{table}|] :: Sql db '[] '[SelOne Int])

getOneTableRowCount' :: (GConn db, ML e m) => db -> Sql db a b -> m Int
getOneTableRowCount' anydb sql =
  ext <$> runSql anydb RNil (mkSql ("getOneTableRowCount' " ++ show (_sSql sql)) [st|select count(*) from #{_sSql sql}|] :: Sql db '[] '[SelOne Int])

newtype LogId = LogId { unLogId :: Int } deriving (Show,Eq,Num,Enum,ToText)

getColumnInfo' :: (ML e m, GConn db) => db -> Table db -> m (Rec ZZZ '[Sel ColumnMeta])
getColumnInfo' db t = runSql db RNil $ snd (getColumnMetaSql db t)


getColumnInfo :: (ML e m, GConn db) => db -> Table db -> m [(ColDataType, ColumnMeta)]
getColumnInfo db t = do
  let (f, sql) = getColumnMetaSql db t
  cs <- ext <$> runSql db RNil sql
  when (null cs) $ UE.throwIO $ GBException [st|getColumnInfo: missing metadata for #{t} in #{showDb db}|]
  return $ map (f &&& id) cs

compareFields :: (ML e m, GConn db1, GConn db2) => db1 -> Table db1 -> db2 -> Table db2 -> m ()
compareFields db1 t1 db2 t2 = do
  m1 <- getColumnInfo db1 t1
  m2 <- getColumnInfo db2 t2
  let ts = compareFields' m1 m2
  mapM_ $logInfo ts

compareFields' :: HasCallStack => [(ColDataType, ColumnMeta)] -> [(ColDataType, ColumnMeta)] -> [Text]
compareFields' m1 m2 =
  let fmat = either (T.toLower . cName . snd) (T.toLower . cName . snd)
      as = sortOn fmat (map Left m1 <> map Right m2)
      bs = groupBy (on (==) fmat) as
      cs = flip map bs $ \case
                      [Left t1@(_,z1), Right t2@(_,z2)]
                        | T.toLower (cName z1) /= T.toLower (cName z2) -> error "compareFields' oops"
                        | otherwise -> ((0::Int,cName z1),These t1 t2)
                      [Left t@(_,m)]  -> ((1,cName m),This t)
                      [Right t@(_,m)] -> ((2,cName m),That t)
                      o -> error $ "compareFields': " ++ show o
  in map (prtFieldDiff . snd) (sortOn fst cs)

prtFieldDiff :: These (ColDataType, ColumnMeta) (ColDataType, ColumnMeta) -> Text
prtFieldDiff tab =
  case tab of
    These (c1,t1) (c2,t2)
      | c1 == c2 -> [st|Same #{cName t1} #{cType t1} #{cType t2} #{show c1}|]
      | otherwise -> [st|***oops!!! Same name but different types c1=#{show c1} c2=#{show c2} t1=#{show t1} t2=#{show t2}|]
    This (_,t) -> [st|Left only #{cName t} #{cType t}|]
    That (_,t) -> [st|Right only #{cName t} #{cType t}|]

defaultCompareData :: [Text] -> [Text] -> [(Text, Text)]
defaultCompareData xs ys = map dupe (intersectBy (on (==) T.toLower) xs ys)

removeKeys :: [Text] -> [(Text, Text)] -> [(Text, Text)]
removeKeys (map (T.strip . T.toLower) -> cs) tps =
  let ff = T.strip . T.toLower
  in filter (\(a,b) -> not (ff a `elem` cs || ff b `elem` cs)) tps
--  filter (\(a,b) -> a `elem` map dupe (intersectBy (on (==) T.toLower) xs ys) map (T.strip . T.toLower *** T.strip . T.toLower)
--  f (intersectBy (on (==) T.toLower) xs ys)

compareTableDataBoth :: (GConn a, ML e m) => a -> Table a -> Table a -> ([Text] -> [Text] -> [(Text, Text)]) -> m [[SqlValue]]
compareTableDataBoth db t1 t2 cmp = do
  xs <- compareTableData db t1 t2 cmp
  ys <- compareTableData db t2 t1 cmp
  return (xs <> ys)

compareTableData :: forall m e db . (GConn db, ML e m) => db -> Table db -> Table db -> ([Text] -> [Text] -> [(Text, Text)]) -> m [[SqlValue]]
compareTableData db t1 t2 cmp = do
  when (t1==t2) $ UE.throwIO $ GBException [st|compareTableData comparing the same tables t1==t2 t1=#{t1} t2=#{t2}|]
  let ff tab = do
        ms <- getColumnInfo db tab
        return $ ms ^.. traverse . _2 . to cName
  c1 <- ff t1
  c2 <- ff t2
  let (fs1,fs2) = (T.intercalate "," *** T.intercalate ",") (unzip (cmp c1 c2))
  xs <- ext <$> runSql db RNil (mkSql ("compareTableData " ++ show (t1,t2)) [st|select #{fs1} from #{t1} except select #{fs2} from #{t2}|] :: Sql db '[] '[SelRaw])
  if null xs then $logInfo [st|compareTableData #{t1} #{t2} is good!!|]
  else $logError [st|compareTableData #{t1} #{t2} is bad!! len=#{length xs}|]
  return xs

convertTypeMeta :: H.SqlTypeId -> Maybe TH.Name
convertTypeMeta tp
  | tp `elem` [H.SqlCharT, H.SqlWCharT, H.SqlVarCharT, H.SqlWVarCharT, H.SqlLongVarCharT, H.SqlWLongVarCharT, H.SqlCharT, H.SqlWCharT] = Just ''String
  | tp `elem` [H.SqlDecimalT, H.SqlNumericT, H.SqlRealT, H.SqlFloatT, H.SqlDoubleT] = Just ''Double
  | tp `elem` [H.SqlSmallIntT, H.SqlIntegerT, H.SqlBitT, H.SqlTinyIntT] = Just ''Int
  | tp == H.SqlBigIntT = Just ''Integer
  | tp `elem` [H.SqlBinaryT, H.SqlVarBinaryT, H.SqlLongVarBinaryT] = Just ''ByteString
  | tp `elem` [H.SqlDateT, H.SqlTimeT, H.SqlTimeWithZoneT, H.SqlTimestampT, H.SqlTimestampWithZoneT, H.SqlUTCTimeT, H.SqlUTCDateTimeT] = Just ''UTCTime
  | tp == H.SqlGUIDT = Just ''UUID  -- change to char(32) and add a conversion
  | otherwise = Nothing --error $ "convertTypeMeta: no idea " ++ show tp

convertType :: H.SqlTypeId -> Maybe Int -> String
convertType tp (Just sz)
  | tp `elem` [H.SqlCharT, H.SqlWCharT] = "char(" <> show sz <> ")"
  | tp `elem` [H.SqlVarCharT, H.SqlWVarCharT, H.SqlLongVarCharT, H.SqlWLongVarCharT] = "varchar(" <> show sz <> ")"
  | tp `elem` [H.SqlDecimalT, H.SqlNumericT, H.SqlRealT, H.SqlFloatT, H.SqlDoubleT] = "float" --  <> show sz <> ")" -- could be more specific here
  | tp `elem` [H.SqlSmallIntT, H.SqlIntegerT, H.SqlBitT, H.SqlTinyIntT] = "int"
  | tp == H.SqlBigIntT = "bigint"
  | tp `elem` [H.SqlCharT, H.SqlWCharT] = "char(" <> show sz <> ")"
  | tp ==  H.SqlBinaryT = "binary(" <> show sz <> ")"
  | tp `elem` [H.SqlVarBinaryT, H.SqlLongVarBinaryT] = "varbinary(" <> show sz <> ")"
  | tp == H.SqlDateT = "date"
  | tp `elem` [H.SqlTimeT, H.SqlTimeWithZoneT] = "time"
  | tp `elem` [H.SqlTimestampT, H.SqlTimestampWithZoneT, H.SqlUTCTimeT, H.SqlUTCDateTimeT] = "datetime2"
  | tp == H.SqlGUIDT = "uniqueidentifier"  -- change to char(32) and add a conversion
  | otherwise = error $ "convertType: no idea " ++ show tp
convertType tp Nothing = error $ "convertType: size is not set for " ++ show tp

systypes :: Map Text TH.Name
systypes =
  M.fromList [
   ("image", ''String)
  ,("text", ''String)
  ,("uniqueidentifier", ''String)
  ,("date", ''UTCTime)
  ,("time", ''String)
  ,("datetime2", ''UTCTime)
  ,("datetimeoffset", ''String)
  ,("tinyint", ''Int)
  ,("smallint", ''Int)
  ,("int", ''Integer)
  ,("smalldatetime", ''UTCTime)
  ,("real", ''Double)
  ,("money", ''Double)
  ,("datetime", ''UTCTime)
  ,("float", ''Double)
  ,("sql_variant", ''ByteString)
  ,("ntext", ''Text)
  ,("bit", ''Bool)
  ,("decimal", ''Double)
  ,("numeric", ''Double)
  ,("smallmoney", ''Double)
  ,("bigint", ''Integer)
  ,("hierarchyid", ''ByteString)
  ,("geometry", ''ByteString)
  ,("geography", ''ByteString)
  ,("varbinary", ''ByteString)
  ,("varchar", ''String)
  ,("binary", ''ByteString)
  ,("char", ''String)
  ,("timestamp", ''ByteString)
  ,("nvarchar", ''ByteString)
  ,("nchar", ''ByteString)
  ,("xml", ''ByteString)
  ,("sysname", ''ByteString)
  ]
