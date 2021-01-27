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
import Logging (pattern GBException, getNumThreads, threadNormal, ThreadPool (..))
import GHC.Stack (HasCallStack)
import GHC.Generics as G (Generic)

data StreamConcurrency = StreamConcurrency
    { sThPool :: !ThreadPool
    , sNumBatches :: !(Maybe Int) -- divide work into x units: dont need to set this as will default to number of threads above: if you have 5 threads and 30 batches then will divvy up the work to keep filling up the threads
    , sPcntOrTxnCnt :: !Int -- width of insert OR number of rows/blocks per txn commit
    , sOneInsert :: !Int -- how many rows in one individual insert statement -- defaults to 1
    } deriving (Show, Eq, G.Generic)

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
     LT -> U.throwIO $ GBException [st|padit: too many params:expected max of pcnt=#{pcnt} found #{len} x=#{show x}|]
     EQ -> return x
     GT -> do
             $logWarn [st|padit: had to pad field: expected pcnt=#{pcnt} found #{len} x=#{show x}|]
             return (x <> replicate (pcnt - len) SqlNull)

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

