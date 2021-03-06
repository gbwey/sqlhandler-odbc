{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{- | provides helper methods for streaming database queries
-}

module HSql.ODBC.ConcurrencyUtils where
import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO,MonadIO)
import Text.Shakespeare.Text (ToText(toText),st)
import qualified Data.Text as T
import qualified Data.Text.Read as TR (decimal,double)
import qualified Data.Text.Encoding as TE (decodeUtf8')
import Database.HDBC (SqlValue(..))
import qualified Data.ByteString.Char8 as B8 (unpack)
import HSql.ODBC.GConn
import Data.Text.Lazy.Builder (fromText)
import Control.Arrow ((&&&),left)
import qualified UnliftIO as U
import Logging (pattern GBException, ML, timeCommand)
import GHC.Stack (HasCallStack)
import GHC.Generics as G (Generic)
import qualified Data.List as L
import GHC.Conc (getNumCapabilities)
import Control.Monad
import Data.Maybe
import Dhall (FromDhall, ToDhall)
-- | specification for alllocating work over a given number of threads
data StreamConcurrency = StreamConcurrency
    { sThPool :: !ThreadPool -- ^ number of threads
    , sNumBatches :: !(Maybe Int) -- ^ divide work into x units: dont need to set this as will default to number of threads above: if you have 5 threads and 30 batches then will divvy up the work to keep filling up the threads
    , sPcntOrTxnCnt :: !Int -- ^ width of insert OR number of rows/blocks per txn commit
    , sOneInsert :: !Int -- ^ number of rows in a single insert statement -- defaults to 1
    } deriving (Show, Eq, G.Generic)

instance ToText StreamConcurrency where
  toText = fromText . T.pack . show

-- | default settings for 'StreamConcurency'
defSC :: Int -> StreamConcurrency
defSC n = StreamConcurrency threadNormal Nothing n 1

-- | calculate the effective number of threads for streaming
getWorkChunks :: MonadIO m => (Maybe Int, ThreadPool) -> m Int
getWorkChunks (numb, thp) =
  case numb of
    Just x -> return x
    Nothing -> do
      (_,ov) <- liftIO $ getNumThreads (thOverride thp)
      return ov

-- | pads out the input parameters to fir the sql query input requirements
paditC :: (MonadLogger m,MonadIO m) => Int -> [SqlValue] -> m [SqlValue]
paditC pcnt (length &&& id -> (len, x)) =
   case compare pcnt len of
     LT -> U.throwIO $ GBException [st|padit: too many params:expected max of pcnt=#{pcnt} found #{len} x=#{show x}|]
     EQ -> return x
     GT -> do
             $logWarn [st|padit: had to pad field: expected pcnt=#{pcnt} found #{len} x=#{show x}|]
             return (x <> replicate (pcnt - len) SqlNull)

-- | convert the sql value to a more specific type based on the meta data
convertUsingMeta :: HasCallStack => (ColDataType, ColumnMeta) -> SqlValue -> SqlValue
convertUsingMeta (cd, ColumnMeta{}) a = case a of
  SqlByteString bs -> case cd of
                        CFixedString -> SqlString $ B8.unpack bs
                        CString      -> SqlString $ B8.unpack bs
                        CInt         -> case TR.decimal =<< left show (TE.decodeUtf8' bs) of
                                          Right (d,e) | T.null e -> SqlInteger d
                                                     | otherwise -> error $ "convertUsingMeta: CInt " ++ show (d,e) ++ " bs=" ++ show bs
                                          Left e -> error $ "convertUsingMeta: CInt failed e=" ++ e
                        CDateTime    -> error $ "convertUsingMeta: unsupported type " ++ show cd ++ " bs=" ++ show bs
                        CDate        -> error $ "convertUsingMeta: unsupported type " ++ show cd ++ " bs=" ++ show bs
                        CFloat       -> case TR.double =<< left show (TE.decodeUtf8' bs) of
                                          Right (d,e) | T.null e -> SqlDouble d
                                                     | otherwise -> error $ "convertUsingMeta: CFloat " ++ show (d,e) ++ " bs=" ++ show bs
                                          Left e -> error $ "convertUsingMeta: CFloat failed e=" ++ e

                        CBool        -> case TR.decimal =<< left show (TE.decodeUtf8' bs) of
                                          Right (d,e) | T.null e -> SqlInt32 d
                                                     | otherwise -> error $ "convertUsingMeta: CBool " ++ show (d,e) ++ " bs=" ++ show bs
                                          Left e -> error $ "convertUsingMeta: CBool failed e=" ++ e
                        CBinary      -> a
                        CCLOB        -> a
                        CBLOB        -> a
                        COther _     -> a
  SqlChar ch -> case cd of
                        CBool        -> SqlInt32 $ if ch=='\1' then 1 else 0
                        _ -> a
  o -> o

newtype ThreadPool = ThreadPool { thOverride :: Maybe Int } deriving (Show, Eq, G.Generic)

threadNormal :: ThreadPool
threadNormal = ThreadPool Nothing

threadNormalOverride :: Int -> ThreadPool
threadNormalOverride n = ThreadPool (Just n)

newtype NC = NC { unNC :: Int } deriving (Show, Eq, G.Generic)

dumpNumThreads :: ML e m => ThreadPool -> m ()
dumpNumThreads th = do
  (NC i,j) <- liftIO (getNumThreads (thOverride th))
  $logWarn $ "dumpNumThreads: NC=" <> T.pack (show i) <> " " <> T.pack (show j)

getNumThreads :: MonadIO m => Maybe Int -> m (NC, Int)
getNumThreads mth = do
  n <- liftIO getNumCapabilities
  let z = fromMaybe n mth
  if z < 1 then error $ "getNumThreads: number of threads < 1 z=" ++ show z ++ " n=" ++ show n
  else return (NC n, z)

data PoolStrategy =
     PoolUnliftIO
   | PoolCustom
   deriving (Show, Eq, G.Generic)

deriving instance FromDhall PoolStrategy
deriving instance ToDhall PoolStrategy

threadedForM_ :: ML e m
              => PoolStrategy
              -> ThreadPool
              -> [a]
              -> (Int -> a -> m b)
              -> m ()
threadedForM_ pl th amb = void . threadedForM pl th amb

threadedForM :: ML e m
             => PoolStrategy
             -> ThreadPool
             -> [a]
             -> (Int -> a -> m b)
             -> m [(Int,b)]
threadedForM _ _ [] _ = $logWarn "threadedForM: nothing to do!" >> return []
threadedForM pl th@ThreadPool {..} xs act = do
  (NC n,ov) <- liftIO $ getNumThreads thOverride
  when (length xs < ov) $ $logWarn $ "threadedForM: more threads than tasks!! tasks=" <> T.pack (show (length xs)) <> " threads=" <> T.pack (show ov)
  if ov==1
  then timeCommand ("threadedForM UNTHREADED Capabilities=" <> T.pack (show n) <> " using " <> T.pack (show ov)) (zip [1..] <$> zipWithM act [1..] xs)
  else timeCommand ("threadedForM Capabilities=" <> T.pack (show n) <> " using " <> T.pack (show ov) <> " " <> T.pack (show pl) <> " " <> T.pack (show th)) $
          case pl of
            PoolUnliftIO -> U.pooledMapConcurrentlyN ov (\(i,j) -> (i,) <$> act i j) (zip [1::Int ..] xs)
            PoolCustom -> concurrentlyLimited ov (zipWith (\i j -> (i,\() -> act i j)) [1..] xs)

-- used bounded threads: seems to make no difference: we might be able to use more threads than logical processors using PoolUnliftIO
concurrentlyLimited :: ML e m => Int -> [(Int, () -> m a)] -> m [(Int, a)]
concurrentlyLimited n tasks = concurrentlyLimited' n tasks [] []

concurrentlyLimited' :: ML e m
                     => Int
                     -> [(Int, () -> m a)]
                     -> [(Int, U.Async (Int, a))]
                     -> [(Int, a)]
                     -> m [(Int, a)]
concurrentlyLimited' _ [] [] results = do
  $logInfo ("concurrentlyLimited' ended length results=" <> T.pack (show (length results)) <> " results=" <> T.pack (show (map fst results)))
  return $ L.sortOn fst results
concurrentlyLimited' 0 todo running results = do
    $logInfo ("concurrentlyLimited' before waitAny: FULL: counters: todo=" <> T.pack (show (length todo)) <> ", running=" <> T.pack (show (length running)) <> " done=" <> T.pack (show (length results)) <> " | running=" <> T.pack (show (map fst running)) <> " done=" <> T.pack (show (map fst results)))
    (task, newResult) <- U.waitAny $ map snd running
    $logInfo ("concurrentlyLimited' after waitAny: task is freed up id=" <> T.pack (show (fst newResult)) <> " counters: todo=" <> T.pack (show (length todo)) <> ", running=" <> T.pack (show (length running)) <> " done=" <> T.pack (show (length results)) <> " | running=" <> T.pack (show (map fst running)) <> " done=" <> T.pack (show (map fst results)))
    let running' = L.delete (fst newResult,task) running
    unless (length running -1 == length running') $ do
       let msg = "concurrentlyLimited' programmer error: couldnt find " <> T.pack (show (fst newResult)) <> " in " <> T.pack (show (map fst running))
       $logError msg
       U.throwIO $ GBException msg
    concurrentlyLimited' 1 todo running' (newResult:results)
concurrentlyLimited' _ [] running results = concurrentlyLimited' 0 [] running results
concurrentlyLimited' n ((i, task):todo) running results = do
    $logInfo ("concurrentlyLimited' " <> T.pack (show n) <> " available: scheduling task i=" <> T.pack (show i) <> " counters: todo=" <> T.pack (show (length todo)) <> ", running=" <> T.pack (show (length running)) <> " done=" <> T.pack (show (length results)) <> " | running=" <> T.pack (show (map fst running)) <> " done=" <> T.pack (show (map fst results)))
    -- make sure this is a bounded thread so it doesnt interfere with ghc
    t <- U.asyncBound $ (i,) <$> task () -- extra parameter () to make lazy else will have started running before we got here! asyncBound so uses a real thread else resource disposed issues
    concurrentlyLimited' (n-1) todo ((i,t):running) results

