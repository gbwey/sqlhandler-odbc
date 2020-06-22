{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
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
module HSql.ODBC.ConcurrencyUtils where
import Control.Monad.Logger
import Control.Monad.IO.Class
import Text.Shakespeare.Text
import qualified Data.Text as T
import Database.HDBC (SqlValue(..))
import qualified Data.ByteString.Char8 as B8
import HSql.ODBC.GConn
import Control.Monad.State.Strict
import Control.Lens hiding ((<.>), (:>))
import Data.Text.Lazy.Builder (fromText)
import Control.Arrow
import Data.List (delete, sortOn)
import Control.Concurrent (getNumCapabilities)
import Data.Maybe
import qualified UnliftIO.Async as UA
import qualified UnliftIO.Exception as UE
import Logging
import GHC.Stack
import qualified Dhall as D
import GHC.Generics (Generic)

newtype ThreadPool = ThreadPool { thOverride :: Maybe Int } deriving (Show,Eq)

newtype NC = NC { unNC :: Int } deriving (Show,ToText)

data StreamConcurrency = StreamConcurrency
    { _sThPool :: !ThreadPool
    , _sNumBatches :: !(Maybe Int) -- divide work into x units: dont need to set this as will default to number of threads above: if you have 5 threads and 30 batches then will divvy up the work to keep filling up the threads
    , _sPcntOrTxnCnt :: !Int -- width of insert OR number of rows/blocks per txn commit
    , _sOneInsert :: !Int -- how many rows in one individual insert statement -- defaults to 1
    } deriving (Show, Eq)

makeLenses ''StreamConcurrency

instance ToText StreamConcurrency where
  toText = fromText . T.pack . show

defSC :: Int -> StreamConcurrency
defSC n = StreamConcurrency threadNormal Nothing n 1

getWorkChunks :: MonadIO m => (Maybe Int, ThreadPool) -> m Int
getWorkChunks (numb, thp) =
  case numb of
    Just x -> return x
    Nothing -> do
      (_,ov) <- liftIO $ getNumThreads (thOverride thp)
      return ov

paditC :: (MonadLogger m,MonadIO m) => Int -> [SqlValue] -> m [SqlValue]
paditC pcnt (length &&& id -> (len, x)) =
   case compare pcnt len of
     LT -> UE.throwIO $ GBException [st|padit: too many params:expected max of pcnt=#{pcnt} found #{len} x=#{show x}|]
     EQ -> return x
     GT -> do
             $logWarn [st|padit: had to pad field: expected pcnt=#{pcnt} found #{len} x=#{show x}|]
             return (x <> replicate (pcnt - len) SqlNull)

convertUsingMeta :: HasCallStack => (ColDataType, ColumnMeta) -> SqlValue -> SqlValue
convertUsingMeta (cd, ColumnMeta{..}) a = case a of
  SqlByteString bs -> case cd of
                        CFixedString -> SqlString $ B8.unpack bs
                        CString      -> SqlString $ B8.unpack bs
                        CInt         -> SqlInteger $ read (B8.unpack bs)
                        CDateTime    -> error $ "convertUsingMeta: unsupported type " ++ show cd ++ " bs=" ++ show bs
                        CDate        -> error $ "convertUsingMeta: unsupported type " ++ show cd ++ " bs=" ++ show bs
                        CFloat       -> SqlDouble $ read (B8.unpack bs)
                        CBool        -> SqlInt32 $ read (B8.unpack bs)
                        CBinary      -> a
                        CCLOB        -> a
                        CBLOB        -> a
                        COther _     -> a
  SqlChar ch -> case cd of
                        CBool        -> SqlInt32 $ if ch=='\1' then 1 else 0
                        _ -> a
  o -> o

concurrentlyLimited :: ML e m => Int -> [(Int, () -> m a)] -> m [(Int, a)]
concurrentlyLimited n tasks = concurrentlyLimited' n tasks [] []

concurrentlyLimited' :: ML e m => Int -> [(Int, () -> m a)] -> [(Int, UA.Async (Int, a))] -> [(Int, a)] -> m [(Int, a)]
concurrentlyLimited' _ [] [] results = do
  $logInfo [st|concurrentlyLimited' ended length results=#{length results} results=#{show (map fst results)}|]
  return $ sortOn fst results
concurrentlyLimited' 0 todo running results = do
    $logInfo [st|concurrentlyLimited' before waitAny: FULL: counters: todo=#{length todo}, running=#{length running} done=#{length results} | running=#{show (map fst running)} done=#{show (map fst results)}|]
    (task, newResult) <- UA.waitAny $ map snd running
    $logInfo [st|concurrentlyLimited' after waitAny: task is freed up id=#{fst newResult} counters: todo=#{length todo}, running=#{length running} done=#{length results} | running=#{show (map fst running)} done=#{show (map fst results)}|]
    let running' = delete (fst newResult,task) running
    unless (length running -1 == length running') $ do
       let msg = [st|concurrentlyLimited' programmer error: couldnt find #{fst newResult} in #{show (map fst running)}|]
       $logError msg
       UE.throwIO $ GBException msg
    concurrentlyLimited' 1 todo running' (newResult:results)
concurrentlyLimited' _ [] running results = concurrentlyLimited' 0 [] running results
concurrentlyLimited' n ((i, task):todo) running results = do
    $logInfo [st|concurrentlyLimited' #{n} available: scheduling task i=#{i} counters: todo=#{length todo}, running=#{length running} done=#{length results} | running=#{show (map fst running)} done=#{show (map fst results)}|]
    -- should be doing this at the hdbc level but might not be possible
    -- make sure this is a bounded thread so it doesnt interfere with ghc
    t <- UA.asyncBound $ (i,) <$> task () -- extra parameter () to make lazy else will have started running before we got here! asyncBound so uses a real thread else resource disposed issues
    concurrentlyLimited' (n-1) todo ((i,t):running) results

dumpNumThreads :: ML e m => ThreadPool -> m ()
dumpNumThreads th = do
  (NC i,j) <- liftIO (getNumThreads (thOverride th))
  $logWarn [st|dumpNumThreads: NC=#{i} #{j} |]

getNumThreads :: MonadIO m => Maybe Int -> m (NC, Int)
getNumThreads mth = do
  n <- liftIO getNumCapabilities
  let z = fromMaybe n mth
  if z < 1 then error $ "getNumThreads: number of threads < 1 z=" ++ show z ++ " n=" ++ show n
  else return (NC n, z)

data PoolStrategy =
     PoolUnliftIO
   | PoolCustom
   deriving (Show,Eq,Generic)

instance D.FromDhall PoolStrategy

threadedForM_ :: ML e m => PoolStrategy -> ThreadPool -> [a] -> (Int -> a -> m b) -> m ()
threadedForM_ pl th amb = void . threadedForM pl th amb

threadedForM :: ML e m => PoolStrategy -> ThreadPool -> [a] -> (Int -> a -> m b) -> m [(Int,b)]
threadedForM _ _ [] _ = $logWarn "threadedForM: nothing to do!" >> return []
threadedForM pl th@ThreadPool {..} xs act = do
  (NC n,ov) <- liftIO $ getNumThreads thOverride
  when (length xs < ov) $ $logWarn [st|threadedForM: more threads than tasks!! tasks=#{length xs} threads=#{ov}|]
  if n==1
  then timeCommand [st|threadedForM UNTHREADED Capabilities=#{n}|] (zip [1..] <$> zipWithM act [1..] xs)
  else timeCommand [st|threadedForM Capabilities=#{n} using #{ov} #{show pl} #{show th}|] $
          case pl of
            PoolUnliftIO -> UA.pooledMapConcurrentlyN         ov (\(i,j) -> (i,) <$> act i j) (zip [1::Int ..] xs)
            PoolCustom -> concurrentlyLimited                 ov (zipWith (\i j -> (i,\() -> act i j)) [1..] xs)

threadNormal :: ThreadPool
threadNormal = ThreadPool Nothing

threadNormalOverride :: Int -> ThreadPool
threadNormalOverride n = ThreadPool (Just n)

