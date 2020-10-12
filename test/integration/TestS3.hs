{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoStarIsType #-}
module Main where
import HSql.ODBC.DBConn
import HSql.ODBC.Sql_TH
import HSql.ODBC.DBSqlite ()
import Database.Sqlite
import HSql.Core.Sql
import Logging
import Text.Shakespeare.Text
import Data.Vinyl
import HSql.Core.TablePrinter
import TestConnections
import Data.Time
import Control.Monad.Logger
import Predicate
import Predicate.Refined
import Predicate.Refined2
import Predicate.Examples.Refined2
import Predicate.Refined3
import Predicate.Examples.Refined3
import GHC.TypeLits (Nat)
import Data.Kind (Type)
import TestSqlite_TH
import Spec
import Control.Monad.IO.Class
import HSql.Core.Common
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import qualified UnliftIO as UE

main :: IO ()
main = Spec.spec

anotherTest :: IO ()
anotherTest = do
  x <- fs $ unAppM $ runSqlX s3W RNil s3_test1
  print $ ext x
  when True $ UE.throwIO $ GBException [st|anotherTest: ...|]


-- as soon as we use liftIO we need monadio
-- as soon as we use logDebug we need monadlogger!
-- if we only use HSql stuff then we are ok
-- need ML e m cos of jim: so monadio and monadlogger are now redundant
-- fd $ unAppM fred
--fred :: (ML e m, HSql m, MonadIO m, MonadLogger m) => m [ResultSet]
fred :: (HSql m, MonadLogger m, MonadIO m) => m [ResultSet]
fred = timeCommandX [st|test|] $ do
  x <- runSqlRawX s3W [] "select 1"
  $logDebug [st|dude|]
  -- jim
  liftIO $ putStrLn "hello world"
  return x

jim :: ML e m => m ()
jim = do
  $logDebug [st|dudely|]
  x <- runSqlRaw s3W [] "select 1"
  liftIO $ print x

