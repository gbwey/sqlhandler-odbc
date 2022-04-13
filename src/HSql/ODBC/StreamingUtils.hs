{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : HSql.ODBC.StreamingUtils
Description : sql streaming and multi-threading
Copyright   : (c) Grant Weyburne, 2021
License     : BSD-3
-}
module HSql.ODBC.StreamingUtils where

import BaseUtils.Extra
import qualified Conduit as C
import Control.Arrow
import qualified Control.Exception as E
import Control.Lens
import qualified Control.Monad.Extra as ME
import Control.Monad.State.Strict
import qualified Control.Monad.Trans.Resource as R
import Data.Bool
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8 (unpack)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Pos
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE (decodeUtf8')
import Data.Text.Lazy.Builder (fromText)
import qualified Data.Text.Read as TR (decimal, double)
import Data.Vinyl
import qualified Data.Vinyl.Functor as V
import Data.Void
import qualified Database.HDBC as H
import DocUtils.Doc
import GHC.Conc (getNumCapabilities)
import GHC.Generics as G (Generic)
import GHC.Stack (HasCallStack)
import HSql.Core
import HSql.Core.VinylUtils
import HSql.ODBC.DBConn
import Language.Haskell.TH.Syntax (qLocation)
import Logging
import Prettyprinter (Doc)
import Primus.Enum
import Primus.Error
import Primus.NonEmpty
import qualified Safe.Exact as Safe
import System.IO
import Text.Shakespeare.Text
import qualified UnliftIO as U

-- | total cpus available and suggested using RTS
data NumCap = NumCap {ncTotal :: !Pos, ncSuggested :: !Pos} deriving stock (Show)

-- | load 'NumCap'
getNumCap :: HasCallStack => IO NumCap
getNumCap = do
  n <- getNumCapabilities
  return NumCap{ncTotal = unsafePos "getNumCap._1" n, ncSuggested = maxP _1P (n - 1)}

instance ToText NumCap where
  toText NumCap{..} =
    fromText [st|NumCap #{show ncTotal} suggested=#{show ncSuggested}|]

-- | specification for alllocating work over a given number of threads
data StreamConcurrency = StreamConcurrency
  { sThPool :: !ThreadPool
  -- ^ number of threads
  , sNumBatches :: !(Maybe Pos)
  -- ^ divide work into x units: dont need to set this as will default to number of threads above: if you have 5 threads and 30 batches then will divvy up the work to keep filling up the threads
  , sPcntOrTxnCnt :: !Pos
  -- ^ width of insert OR number of rows/blocks per txn commit
  , sOneInsert :: !Pos
  -- ^ number of rows in a single insert statement -- defaults to 1
  }
  deriving stock (Show, Eq, G.Generic)

instance ToText StreamConcurrency where
  toText StreamConcurrency{..} =
    fromText [st|SC #{sThPool} batches=#{show sNumBatches} pcnt=#{show sPcntOrTxnCnt} oneinsert=#{show sOneInsert}|]

-- | default settings for 'StreamConcurrency'
defSC' :: Pos -> StreamConcurrency
defSC' n = StreamConcurrency threadNormal Nothing n _1P

-- | default settings for 'StreamConcurrency' using type level for 'sPcntOrTxnCnt'
defSC :: forall n. PosC n => StreamConcurrency
defSC = StreamConcurrency threadNormal Nothing (_P @n) _1P

-- | override the number of threads
newtype ThreadPool = ThreadPool {thOverride :: Maybe Pos}
  deriving stock (Show, Eq, G.Generic)

instance ToText ThreadPool where
  toText (ThreadPool ov) =
    fromText $ "pool=" <> maybe "Nothing" (("Just " <>) . T.pack . show) ov

-- | dont override number of threads
threadNormal :: ThreadPool
threadNormal = ThreadPool Nothing

-- | override number of threads
threadNormalOverride :: Pos -> ThreadPool
threadNormalOverride n = ThreadPool (Just n)

-- | dump number of threads for display
dumpNumThreads :: ML e m => ThreadPool -> m Text
dumpNumThreads th = do
  (nc, j) <- liftIO (getNumThreads th)
  let msg = [st|dumpNumThreads: #{nc} #{show j}|]
  $logWarn msg
  return msg

-- | get the number of threads incorporating overrides from the 'ThreadPool' parameter
getNumThreads :: (HasCallStack, MonadIO m) => ThreadPool -> m (NumCap, Pos)
getNumThreads (ThreadPool mth) = do
  nc <- liftIO getNumCap
  return (nc, fromMaybe (ncTotal nc) mth)

-- | pads out the input parameters for the sql query input requirements
paditC :: (Show a, MonadLogger m, MonadIO m) => Pos -> a -> [a] -> m [a]
paditC (Pos pcnt) defValue sqls = do
  $logDebug [st|paditC: begin: pcnt=#{pcnt} sqls=#{psi sqls}|]
  let len = length sqls
  case compare pcnt len of
    LT -> U.throwIO $ GBException [st|padit: too many params:expected max of pcnt=#{pcnt} found #{len} sqls=#{psi sqls}|]
    EQ -> return sqls
    GT -> do
      $logWarn [st|paditC: had to pad field: expected pcnt=#{pcnt} found #{len} sqls=#{psi sqls}|]
      return (sqls <> replicate (pcnt - len) defValue)

-- | calculate the effective number of threads for streaming
getWorkChunks :: MonadIO m => (Maybe Pos, ThreadPool) -> m Pos
getWorkChunks (numb, thp) =
  case numb of
    Just x -> return x
    Nothing -> do
      (_, ov) <- liftIO $ getNumThreads thp
      return ov

-- | call back holding the current thread id and the return value
type ThreadedAction m a c = Pos -> a -> m c

-- | same as 'threadedForM' but ignores the output
threadedForM_ ::
  ML e m =>
  ThreadPool ->
  NonEmpty a ->
  ThreadedAction m a () ->
  m ()
threadedForM_ th amb = void . threadedForM th amb

-- | concurrently run an action against multiple values
threadedForM ::
  forall e m a b.
  ML e m =>
  ThreadPool ->
  NonEmpty a ->
  ThreadedAction m a b ->
  m (NonEmpty (Pos, b))
threadedForM th xs act = do
  (NumCap n _, ov) <- liftIO $ getNumThreads th
  let enz :: forall x. NonEmpty x -> NonEmpty (Pos, x)
      enz = N.zip universe1
  when (lengthP xs < ov) $ $logWarn [st|threadedForM: more threads than tasks!! tasks=#{length xs} threads=#{show ov}|]
  if ov == _1P
    then
      timeCommand
        [st|threadedForM UNTHREADED Capabilities=#{show n} using #{show ov}|]
        (enz <$> mapM (uncurry act) (enz xs))
    else
      timeCommand [st|threadedForM Capabilities=#{show n} using #{show ov} #{show th}|] $
        U.pooledMapConcurrentlyN (unP ov) (\(i, j) -> (i,) <$> act i j) (enz xs)

-- | start a transaction within a Resource context
withTransactionCR ::
  (H.IConnection conn, C.MonadResource m, MonadLoggerIO m) =>
  Text ->
  conn ->
  m (R.ReleaseKey, ())
withTransactionCR txt conn = do
  lg <- getLogger $(qLocation >>= liftLoc)
  R.allocate (return ()) (\() -> lgDebug lg [st|#{txt} withTransactionC before commit|] >> H.commit conn >> lgDebug lg [st|#{txt} withTransactionC after commit|])

-- | get access to a file handle within a Resource context
withFnC ::
  R.MonadResource m =>
  FilePath ->
  IOMode ->
  m (R.ReleaseKey, Handle)
withFnC fn md =
  R.allocate
    (openBinaryFile fn md)
    (\a -> hClose a >> winfo [st|withFnC md=#{show md} fn[#{fn}] closing|])

-- | open a connection within a Resource context
bracketDBCR ::
  (GConn a1, R.MonadResource m, MonadLoggerIO m) =>
  Text ->
  a1 ->
  m (R.ReleaseKey, HConn a)
bracketDBCR txt db = do
  lg <- getLogger $(qLocation >>= liftLoc)
  R.allocate (HConn <$> getConn db) $ \c -> do
    lgDebug lg [st|#{txt} bracketDBC before ending|]
    U.catch (liftIO $ H.disconnect c) $ \(e :: H.SqlError) -> do
      let skip = ignoreDisconnectError (Just db)
      bool lgError lgWarn skip lg [st|Exception thrown: #{txt} bracketDBC disconnect #{psi e}|]
      unless skip $ U.throwIO $ GBWrapException "bracketDBCR" e
    lgDebug lg [st|#{txt} bracketDBC after ending|] -- fails on disconnect!

-- | allocate a statement within a Resource context
bracketStmtCR ::
  (H.IConnection conn, R.MonadResource m, MonadLoggerIO m) =>
  Text ->
  conn ->
  String ->
  m (R.ReleaseKey, H.Statement)
bracketStmtCR txt fromconn sql = do
  lg <- getLogger $(qLocation >>= liftLoc)
  R.allocate
    (H.prepare fromconn sql)
    (\a -> lgDebug lg [st|#{txt} bracketStmtC before ending|] >> H.finish a >> lgDebug lg [st|#{txt} bracketStmtC after ending|])

-- | conduit producer that runs a select statement lazily using 'H.fetchAllRows' and only works with one resultset
selectSourceLazyC ::
  (ML e m, R.MonadResource m, GConn db, RSqlInputC a, RSqlOutputC b) =>
  Text ->
  Sql db a b ->
  [SqlValue] ->
  db ->
  C.ConduitT () [SqlValue] m ()
selectSourceLazyC txt' sql hs db = do
  let txt = "selectSourceLazyC " <> txt'
  $logWarn [st|#{txt} starting|]
  (_, c) <- bracketDBCR txt db
  flip
    C.catchC
    ( \(e :: H.SqlError) -> do
        werrLS [st|#{txt} wrapSqlException: #{psi e}|]
        C.catchC (liftIO $ H.rollback c) $ \(f :: E.SomeException) -> wdebugL [st|#{txt} in exception #{psi f}|]
        liftIO $ E.throwIO $ GBWrapException txt e
    )
    $ do
      (_, ()) <- withTransactionCR txt c
      (_, selstmt) <- bracketStmtCR txt c ((T.unpack . sSql) sql)
      --  c <- HConn <$> liftIO (getConn db)
      --  selstmt <- liftIO $ H.prepare c ((T.unpack . sSql) sql)
      mrc <- liftIO $ H.execute selstmt hs
      case mrc of
        Right _meta -> do
          $logDebug [st|#{txt} starting select from #{psi sql}|]
          xs <- liftIO $ H.fetchAllRows selstmt -- cant run the nextresultset cos will close stuff:could add to bracketstmt finaliser but not worth it cos we close the connection after
          mapM_ C.yield xs
        Left rc -> liftIO $ E.throwIO $ GBException [st|#{txt} expecting a result set but found #{rc}|]
  $logWarn [st|#{txt} ending|]

-- probably slower but better

-- | conduit producer that runs a select statement lazily using 'H.fetchRow' but works with multiple resultsets
selectSourceLazyALTC ::
  (ML e m, R.MonadResource m, GConn db) =>
  Text ->
  Sql db a b ->
  [SqlValue] ->
  db ->
  C.ConduitT () [SqlValue] m ()
selectSourceLazyALTC txt' sql hs db = do
  let txt = "selectSourceLazyALTC " <> txt'
  $logWarn [st|#{txt} starting|]
  (_, c) <- bracketDBCR txt db
  flip
    C.catchC
    ( \(e :: H.SqlError) -> do
        werrLS [st|#{txt} wrapSqlException: #{psi e}|]
        C.catchC (liftIO $ H.rollback c) $ \(f :: E.SomeException) -> wdebugL [st|#{txt} in exception #{psi f}|]
        liftIO $ E.throwIO $ GBWrapException txt e
    )
    $ do
      (_, ()) <- withTransactionCR txt c
      (_, selstmt) <- bracketStmtCR txt c ((T.unpack . sSql) sql)
      --  c <- HConn <$> liftIO (getConn db)
      --  selstmt <- liftIO $ H.prepare c ((T.unpack . sSql) sql)
      mrc <- liftIO $ H.execute selstmt hs
      case mrc of
        Right _meta -> do
          $logDebug [st|#{txt} starting select from sql #{sSql sql}|]
          let go !(i :: Int) = do
                mx <- liftIO $ H.fetchRow selstmt
                case mx of
                  Nothing -> do
                    $logDebug [st|#{txt} ended with #{i} rows|]
                    mrs <- liftIO $ H.nextResultSet selstmt
                    case mrs of
                      Nothing -> $logWarn [st|#{txt} after running nextResultSet|]
                      Just _ -> do
                        werrLS [st|#{txt} after running nextResultSet oops found another resultSet!!|]
                  Just x -> do
                    when (i `mod` 1000 == 0) $ $logDebug [st|#{txt} running #{i}|]
                    C.yield x
                    go (i + 1)
          go 0
        Left rc -> liftIO $ E.throwIO $ GBException [st|#{txt} expecting a result set but found #{rc}|]
  $logWarn [st|#{txt} ending|]

-- | batch inserts together into a stream of Rights but if there are not enough entries for the letzte one then make them a stream of Lefts (ie single inserts)
fit1 :: Monad m => StreamConcurrency -> C.ConduitT [a] (Either [a] [a]) m ()
fit1 StreamConcurrency{sOneInsert = Pos sone} =
  let go !i !as = do
        ma <- C.await
        case ma of
          Nothing -> unless (null as) $ mapM_ (C.yield . Left) as
          Just a ->
            if i == sone - 1
              then C.yield (Right (concat as <> a)) >> go 0 mempty
              else go (i + 1) (as Seq.:|> a)
   in go (0 :: Int) mempty

-- | batch into size of n until you run out then output that last one
fit2 :: Monad m => Pos -> C.ConduitT a [a] m ()
fit2 (Pos n) =
  let go !i !as = do
        ma <- C.await
        case ma of
          Nothing -> unless (null as) $ C.yield $ toList as
          Just a ->
            if i == n - 1
              then C.yield (toList (as Seq.:|> a)) >> go 0 mempty
              else go (i + 1) (as Seq.:|> a)
   in go (0 :: Int) mempty

-- todo: this is not strict cos have to force fetchAllRows else odbc sequence error -- only works for first resultset

-- | typed version of 'selectSourceLazyC'
selectSourceLazyTypedC ::
  (R.MonadResource m, Show b, ML e m, GConn db, RSqlInputC a) =>
  Text ->
  Sql db a '[Sel b] ->
  Rec V.Identity a ->
  db ->
  C.ConduitT () b m ()
selectSourceLazyTypedC txt' z@(Sql desc encs decs sql) vals db = do
  let txt = "selectSourceLazyTypedC " <> txt'
  $logWarn [st|#{txt} starting|]
  (_, c) <- bracketDBCR txt db
  flip
    C.catchC
    ( \(e :: H.SqlError) -> do
        werrLS [st|#{txt} wrapSqlException: #{psi e}|]
        C.catchC (liftIO $ H.rollback c) $ \(f :: E.SomeException) -> wdebugL [st|#{txt} in exception #{psi f}|]
        liftIO $ E.throwIO $ GBWrapException txt e
    )
    $ do
      let E1 (SelP (Dec dec)) = decs
      (_, ()) <- withTransactionCR txt c
      (_, selstmt) <- bracketStmtCR txt c (T.unpack sql)
      mrc <- liftIO $ H.execute selstmt (encodeVals encs vals)
      case mrc of
        Right _meta -> do
          $logDebug [st|#{txt} #{psi z}|]
          xs <- liftIO $ H.fetchAllRows selstmt -- cant run the nextresultset cos will close stuff:could add to bracketstmt finaliser but not worth it cos we close the connection after
          forM_ (itoList xs) $ \(i, x) ->
            case dec x of
              Left e -> U.throwIO $ GBException [st|#{txt} #{desc} decode failed #{psi e} x=#{psi x} i=#{i}#{newline}#{psi z}|]
              Right (ret, hs)
                | null hs -> C.yield ret
                | otherwise -> U.throwIO $ GBException [st|#{txt} #{desc} decode failed leftovers found hs=#{psi hs} ret=#{psi ret} i=#{i}#{newline}#{psi z}|]
        Left rc -> liftIO $ U.throwIO $ GBException [st|#{txt} #{desc} expecting a result set rc=0 but found #{rc}#{newline}#{psi z}|]
  $logWarn [st|#{txt} ending|]

-- | conduit consumer that runs multiple sql inserts at a time
insertConduitC ::
  (R.MonadResource m, ML e m, GConnWrite db) =>
  Text ->
  StreamConcurrency ->
  (Pos -> (Sql db a b, Pos)) ->
  db ->
  C.ConduitT (Either [SqlValue] [SqlValue]) Void m Int
insertConduitC txt' sc isql db = do
  let txt = "insertConduitC " <> txt'
  $logWarn [st|#{txt} starting|]
  (_, c) <- bracketDBCR txt db
  ret <- flip
    C.catchC
    ( \(e :: H.SqlError) -> do
        werrLS [st|#{txt} wrapSqlException: #{psi e}|]
        C.catchC (liftIO $ H.rollback c) $ \(f :: E.SomeException) -> wdebugL [st|#{txt} in exception #{psi f}|]
        liftIO $ E.throwIO $ GBWrapException txt e
    )
    $ do
      $logDebug [st|insertConduitC: sc=#{sc}|]
      let (irows, Pos itxns) = (sOneInsert &&& sPcntOrTxnCnt) sc
      let (sqlONE, Pos pcntONE) = first (T.unpack . sSql) $ isql _1P
      let (sql, Pos pcnt) = first (T.unpack . sSql) $ isql irows
      (_, ()) <- withTransactionCR txt c
      (_, stmtONE) <- bracketStmtCR txt c sqlONE
      (_, stmt) <- bracketStmtCR txt c sql
      $logDebug [st|#{txt} sqlONE=#{sqlONE} sql=#{sql}|]
      let go !i !(j :: Int) !(k :: Int) = do
            mb <- C.await
            case mb of
              Nothing -> do
                when (i > 0) $ do
                  $logDebug [st|commit end txns left i=#{i} j=#{j} k=#{k}|]
                  liftIO $ H.commit c
                return k
              Just (Right hs) -> do
                let len = length hs
                if len == pcnt
                  then do
                    if len == pcntONE
                      then when (k `mod` 100 == 0) $ $logDebug [st|#{txt} inserting bulk (SINGLE ONLY) #{show irows} pcnt=#{pcnt} pcntONE=#{pcntONE} len=#{len} i=#{i} j=#{j} k=#{k}|] --  hs=#{psi hs}
                      else $logDebug [st|#{txt} inserting bulk #{show irows} pcnt=#{pcnt} pcntONE=#{pcntONE} len=#{len} i=#{i} j=#{j} k=#{k}|] --  hs=#{psi hs}
                    mrc <- C.catchC (liftIO $ H.execute stmt hs) $
                      \(e :: H.SqlError) -> do
                        werrLS [st|Exception thrown: #{txt} e=#{psi e} after insert sql=[#{sql}] hs=#{psi hs}|]
                        U.throwIO $ GBWrapException [st|#{txt} catch execute stmt|] e

                    case mrc of
                      Right _meta -> U.throwIO $ GBException [st|#{txt} we got another resultset in insertConduit bulk: how does that happen|]
                      Left rc -> do
                        i' <-
                          if i == itxns - 1
                            then do
                              $logDebug [st|#{txt} commit multiple itxns=#{itxns} j=#{j} k=#{k} rc=#{rc}|] --  hs=#{psi hs}
                              liftIO $ H.commit c
                              return 0
                            else return (i + 1)

                        ME.whenJustM (liftIO $ H.nextResultSet stmt) $
                          \nrc -> U.throwIO $ GBException [st|#{txt} we got another resultset in insertConduit bulk: how does that happen nrc=#{psi nrc}|]
                        go (i' :: Int) (j + 1) (k + unP irows)
                  else U.throwIO $ GBException [st|#{txt}: pcnt: wrong number of params:expected pcnt=#{pcnt} found #{len} (zero is also invalid) hs=#{psi hs}|]
              Just (Left hs) -> do
                let len = length hs
                if len == pcntONE
                  then do
                    $logDebug [st|#{txt} inserting one at a time!!! pcnt=#{pcnt} pcntONE=#{pcntONE} len=#{len} i=#{i} j=#{j} k=#{k}|] --  hs=#{psi hs}
                    mrc <- C.catchC (liftIO $ H.execute stmtONE hs) $
                      \(e :: H.SqlError) -> do
                        werrLS [st|Exception thrown: #{txt} e=#{psi e} after insertONE sql=[#{sqlONE}] hs=#{psi hs}|]
                        U.throwIO $ GBWrapException [st|#{txt} catch execute stmt|] e
                    case mrc of
                      Right _meta -> U.throwIO $ GBException [st|#{txt} we got another resultset in insertConduit single: how does that happen|]
                      Left rc -> do
                        i' <-
                          if i == itxns - 1
                            then do
                              $logDebug [st|#{txt} commit single itxns=#{itxns} j=#{j} k=#{k} rc=#{rc}|]
                              liftIO $ H.commit c
                              return 0
                            else do
                              $logDebug [st|#{txt} NO COMMIT single itxns=#{itxns} j=#{j} k=#{k} rc=#{rc}|]
                              return (i + 1)
                        ME.whenJustM (liftIO $ H.nextResultSet stmt) $
                          \nrc -> U.throwIO $ GBException [st|#{txt} we got another resultset in insertConduit single: how does that happen nrc=#{psi nrc}|]
                        go (i' :: Int) (j + 1) (k + 1)
                  else U.throwIO $ GBException [st|#{txt}: wrong number of params:expected pcntONE=#{pcntONE} found #{len} (zero is also invalid) hs=#{psi hs}|]

      go 0 0 0
  $logWarn [st|#{txt} ending|]
  return ret

-- typesafe version uses encoding and decoding. may need to change all to this format

-- | type safe conduit consumer that runs multiple sql inserts at a time
insertConduitWithStatementsC ::
  (ML e m, GConnWrite db) =>
  Bool ->
  ((x, [[SqlValue]]) -> (String, [[SqlValue]])) ->
  Text ->
  db ->
  C.ConduitT (x, [[SqlValue]]) Void m Int -- String is probably a sql string and [[SqlValue]] are rows to insert
insertConduitWithStatementsC doit fn txt' db = do
  let txt = "insertConduitWithStatementsC " <> txt'
  $logWarn [st|#{txt} starting|]
  c <- HConn <$> liftIO (getConn db)
  let go m !(n :: Int) = do
        mb <- C.await
        case mb of
          Nothing -> $logDebug [st|#{txt} done!|] >> return n
          Just z@(_, hhs') -> do
            let (sql, hhs) = fn z
            $logDebug [st|#{txt}: before insert sql[#{sql}] lenhhs'=#{length hhs'} lenhhs=#{length hhs}|]
            if null hhs
              then do
                $logWarn [st|#{txt} empty resultset sql[#{sql}]|]
                go m n
              else do
                (m', stmt) <- case M.lookup sql m of
                  Nothing -> do
                    $logDebug [st|#{txt} allocating new statement for sql[#{sql}]|]
                    xstmt <- liftIO $ H.prepare c sql
                    return (M.insert sql xstmt m, xstmt)
                  Just xstmt -> return (m, xstmt)

                forM_ hhs $ \hs ->
                  when doit $ do
                    mrc <- C.catchC (liftIO $ H.execute stmt hs) $
                      \(e :: H.SqlError) -> do
                        werrLS [st|Exception thrown: #{txt} e=#{psi e} after insert sql=[#{sql}] hhs=#{psi hhs} hs=#{psi hs}|]
                        U.throwIO $ GBWrapException [st|#{txt} catch execute stmt|] e
                    case mrc of
                      Right _meta -> U.throwIO $ GBException [st|#{txt} we got another resultset in insertConduit bulk: how does that happen|]
                      Left rc -> unless (rc == 1) $ U.throwIO $ GBException [st|#{txt} expected 1 for insert rc but found #{rc}|]

                    -- todo: why is this a logerror and not a throw
                    ME.whenJustM (liftIO $ H.nextResultSet stmt) $
                      \nrc -> do
                        werrLS [st|#{txt} we got another resultset in single: how does that happen nrc=#{psi nrc}|]

                $logDebug [st|#{txt} commit len=#{length hhs}|]
                C.catchC (liftIO $ H.commit c) $
                  \(e :: H.SqlError) -> do
                    werrLS [st|Exception thrown: #{txt} e=#{psi e} after commit sql=[#{sql}]|]
                    U.throwIO $ GBWrapException [st|#{txt} catch commit|] e
                go m' (n + length hhs)

  ret <- go M.empty 0
  $logWarn [st|#{txt} starting|]
  return ret

-- | strict conduit producer for selects
selectConduitStrictC ::
  (ML e m, R.MonadResource m, GConn db) =>
  Text ->
  (Sql db a b, Pos) ->
  db ->
  C.ConduitT [SqlValue] [[SqlValue]] m ()
selectConduitStrictC txt z db = selectConduitStrictC' (\_ (_, xxs) -> xxs) txt z db C..| C.concatC -- horizontal flattens the stream by stretching them out

-- | strict conduit producer for selects
selectConduitStrictC' ::
  (R.MonadResource m, Show ret, ML e m, GConn db) =>
  ([SqlValue] -> (RMeta, [[SqlValue]]) -> ret) ->
  Text ->
  (Sql db a b, Pos) ->
  db ->
  C.ConduitT [SqlValue] [ret] m ()
selectConduitStrictC' callback txt' (sql, pcnt) db = do
  let txt = [st|selectConduitStrictC' #{txt'} sql=#{sql}|]
  $logWarn [st|#{txt}: starting|]
  (_, c) <- bracketDBCR txt db
  ret <- flip
    C.catchC
    ( \(e :: H.SqlError) -> do
        werrLS [st|#{txt} wrapSqlException: #{psi e}|]
        C.catchC (liftIO $ H.rollback c) $ \(f :: E.SomeException) -> $logDebug [st|#{txt} in exception #{psi f}|]
        liftIO $ E.throwIO $ GBWrapException txt e
    )
    $ do
      (_, ()) <- withTransactionCR txt c
      (_, stmt) <- bracketStmtCR txt c ((T.unpack . sSql) sql)
      let go !(i :: Int) = do
            mb <- C.await
            case mb of
              Nothing -> $logDebug [st|#{txt}: batch #{i} Nothing|]
              Just !zs -> do
                hs <- paditC pcnt SqlNull zs
                $logDebug [st|#{txt}: before batch #{i} zs=#{psi zs}|] --  hs=#{psi hs}
                mrc1 <- liftIO (H.execute stmt hs)
                let goX (Right (!meta)) !stash = do
                      xxs <- liftIO $ H.fetchAllRows' stmt
                      let ys = callback hs (meta, xxs)
                      mrc <- liftIO $ H.nextResultSet stmt
                      case mrc of
                        Nothing -> do
                          $logDebug [st|#{txt}: after batch #{i}|]
                          C.yield (stash . (ys :) $ [])
                          go (i + 1)
                        Just nrc -> goX nrc (stash . (ys :))
                    -- could decide to handle non selects but we need a use case
                    -- at the moment it is an error: would mean that it could be GConnWrite!!! not good
                    goX (Left x) stash = liftIO $ E.throwIO $ GBException [st|#{txt} expecting a result set but found #{x} stash=#{psi (stash [])}|]
                goX mrc1 id

      go 0
  $logWarn [st|#{txt}: ending|]
  return ret

-- | conduit consumer for outputting to a file in bcp format
bcpSinkC ::
  (ML e m, R.MonadResource m) =>
  Text ->
  FilePath ->
  (ByteString, ByteString) ->
  C.ConduitT [SqlValue] Void m Int
bcpSinkC txt' fn (coldelim, rowdelim) = do
  let txt = "bcpSinkC " <> txt'
  $logWarn [st|#{txt} starting fn[#{fn}]|]
  (_, h) <- withFnC fn AppendMode
  let go !n = do
        ma <- C.await
        case ma of
          Nothing -> do
            $logInfo [st|#{txt} ending fn[#{fn}] rows output=#{n}|]
            return n
          Just xs -> do
            let zs = map toBcpFromSql xs
            liftIO $ B.hPutStr h (B.intercalate coldelim zs <> rowdelim)
            go (n + 1)
  ret <- go 0
  $logWarn [st|#{txt}: ending fn[#{fn}]|]
  return ret

-- | copies a table to another database using metadata parameter
copyDBtoDBCommonStreamingC ::
  (ML e m, GConn src, GConnWrite tgt, RSqlInputC a, RSqlOutputC b) =>
  StreamConcurrency ->
  src ->
  Sql src a b ->
  [SqlValue] ->
  tgt ->
  Table tgt ->
  NonEmpty (ColDataType, ColumnMeta) ->
  m (Int, Doc ann)
copyDBtoDBCommonStreamingC sc srcdb sel params tgtdb tabout metas =
  timeCommandDoc' True [st|copyDBtoDBCommonStreaming #{sc} src=#{showDb srcdb} tgt=#{showDb tgtdb} tabout[#{tabout}]|] $ do
    let ins = getDBInsertSqlStream metas tabout
    ret <-
      C.runConduitRes $
        selectSourceLazyC [st|copyDBtoDBCommonStreaming #{showDb srcdb}|] sel params srcdb
          C..| C.mapC (Safe.zipWithExactNote "copyDBtoDBCommonStreamingC" convertUsingMeta (N.toList metas))
          C..| fit1 sc
          C..| insertConduitC [st|copyDBtoDBCommonStreaming #{tabout}|] sc ins tgtdb
    $logInfo [st|copyDBtoDBCommonStreaming #{tabout} r=#{ret}|]
    return (ret, [])

-- | copies a table to another database
copyDBtoDBSqlStreaming ::
  (ML e m, GConn src, GConnWrite tgt, RSqlInputC a, RSqlOutputC b) =>
  StreamConcurrency ->
  src ->
  Sql src a b ->
  [SqlValue] ->
  tgt ->
  Table tgt ->
  m (Doc ann)
copyDBtoDBSqlStreaming sc srcdb sel params tgtdb tabout = timeCommandDocI [st|copyDBtoDBSqlStreaming src=#{showDb srcdb} tgt=#{showDb tgtdb} tabout[#{tabout}]|] $ do
  b <- existsTable tgtdb tabout
  unless b $ U.throwIO $ GBException [st|copyDBtoDBSqlStreaming: table doesnt exist!!! #{tabout}|]
  $logInfo [st|sel[#{sSql sel}]|]
  meta <- getColumnInfo tgtdb tabout
  (_, r1) <-
    copyDBtoDBCommonStreamingC
      sc
      srcdb
      sel
      params
      tgtdb
      tabout
      meta
  return [r1]

-- | calls 'copyDBtoDBStreaming'' with default 'TableCreate' and the name of the target table the same as the source table name
copyDBtoDBStreaming ::
  (ML e m, GConn src, GConnWrite tgt) =>
  StreamConcurrency ->
  src ->
  tgt ->
  Table src ->
  m (Doc ann)
copyDBtoDBStreaming sc srcdb tgtdb tabin =
  copyDBtoDBStreaming'
    sc
    TableCreate
    srcdb
    tabin
    tgtdb
    (unsafeCastTable tgtdb tabin)

-- | copy a table from one database to another: handles creating the table if needed
copyDBtoDBStreaming' ::
  (ML e m, GConn src, GConnWrite tgt) =>
  StreamConcurrency ->
  TableCreate ->
  src ->
  Table src ->
  tgt ->
  Table tgt ->
  m (Doc ann)
copyDBtoDBStreaming' sc cre srcdb tabin tgtdb tabout = timeCommandDocI [st|copyDBtoDB cre=#{cre} src=#{showDb srcdb} tgt=#{showDb tgtdb} tabin[#{tabin}] tabout[#{tabout}]|] $ do
  b <- existsTable tgtdb tabout
  case cre of
    DropTableCreate -> do
      when b $ dropTable tgtdb tabout
      void $ createDBTableFrom srcdb tabin tgtdb tabout
      $logInfo [st|creating table #{tabout}|]
    TableCreate -> do
      when b $ U.throwIO $ GBException [st|table already exists: #{tabout}|]
      void $ createDBTableFrom srcdb tabin tgtdb tabout
      $logInfo [st|creating table #{tabout}|]
    SkipTableCreate -> do
      unless b $ U.throwIO $ GBException [st|table doesnt exist!!! #{tabout}|]
      $logInfo [st|using existing table #{tabout}|]

  cnt <- getOneTableRowCount srcdb tabin
  $logInfo [st|#{showDb srcdb} #{showDb tgtdb} rows for #{tabin} rows=#{cnt}|]

  metas <- getColumnInfo srcdb tabin
  -- todo fix this: it is not a Upd but does it make sense to make it more specific
  let sel = mkSql @'[Upd] @'[] [st|copyDBtoDBStreaming' #{tabin}|] (getDBSelectSql (N.map snd metas) tabin)
  $logInfo [st|sel[#{sSql sel}]|]
  (ret, r1) <- copyDBtoDBCommonStreamingC sc srcdb sel [] tgtdb tabout metas
  cnt1 <- getOneTableRowCount tgtdb tabout
  $logInfo [st|mssql rows for #{tabout} rows=#{cnt1} rows=#{cnt} ret=#{psi ret}|]
  if cnt == cnt1
    then $logInfo "awesome!! counts match"
    else U.throwIO $ GBException [st|counts dont match #{showDb srcdb}=#{cnt} #{tgtdb}=#{cnt1}|]
  return [r1]

-- | used by 'copyDBtoDBCommonStreamingC' for creating the multiple sql insert statement
getDBInsertSqlStream ::
  GConnWrite tgt =>
  NonEmpty (ColDataType, ColumnMeta) ->
  Table tgt ->
  Pos ->
  (Sql tgt '[] '[Upd], Pos)
getDBInsertSqlStream metas tgttable =
  let xs = N.map (escapeField tgttable . cName . snd) metas
      c = lengthP metas
   in \r -> (mkSql "getDBInsertSqlStream" [st|insert into #{tgttable} #{vvs xs} values#{qqrc (r,c)}|], r *! c)

-- | convert the sql value to a more specific type based on the meta data
convertUsingMeta :: HasCallStack => (ColDataType, ColumnMeta) -> SqlValue -> SqlValue
convertUsingMeta (cd, ColumnMeta{}) =
  \case
    z@(SqlByteString bs) -> case cd of
      CFixedString -> SqlString $ B8.unpack bs
      CString -> SqlString $ B8.unpack bs
      CInt -> case TR.decimal =<< left show (TE.decodeUtf8' bs) of
        Right (d, e)
          | T.null e -> SqlInteger d
          | otherwise -> normalError $ msg ++ "CInt " ++ show (d, e) ++ " bs=" ++ show bs
        Left e -> normalError $ msg ++ "CInt failed e=" ++ e
      CDateTime -> normalError $ msg ++ "unsupported type " ++ show cd ++ " bs=" ++ show bs
      CDate -> normalError $ msg ++ "unsupported type " ++ show cd ++ " bs=" ++ show bs
      CFloat -> case TR.double =<< left show (TE.decodeUtf8' bs) of
        Right (d, e)
          | T.null e -> SqlDouble d
          | otherwise -> normalError $ msg ++ "CFloat " ++ show (d, e) ++ " bs=" ++ show bs
        Left e -> normalError $ msg ++ "CFloat failed e=" ++ e
      CBool -> case TR.decimal =<< left show (TE.decodeUtf8' bs) of
        Right (d, e)
          | T.null e -> SqlInt32 d
          | otherwise -> normalError $ msg ++ "CBool " ++ show (d, e) ++ " bs=" ++ show bs
        Left e -> normalError $ msg ++ "CBool failed e=" ++ e
      CBinary -> z
      CCLOB -> z
      CBLOB -> z
      COther{} -> z
    z@(SqlChar ch) -> case cd of
      CBool -> SqlInt32 $ bool 0 1 (ch == '\1')
      _o -> z
    o -> o
 where
  msg :: String
  msg = "convertUsingMeta: "
