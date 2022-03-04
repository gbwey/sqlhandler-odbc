{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Kind (Type)
import Data.Time
import Data.Vinyl
import Database.Sqlite
import GHC.TypeLits (Nat)
import HSql.Core.Common
import HSql.Core.Sql
import HSql.Core.TablePrinter
import HSql.ODBC.DBConn
import HSql.ODBC.DBSqlite ()
import HSql.ODBC.Sql_TH
import Logging

{-
import Predicate
import Predicate.Examples.Refined2
import Predicate.Examples.Refined3
import Predicate.Refined
import Predicate.Refined2
import Predicate.Refined3
-}
import Spec
import TestConnections
import TestSqlite_TH
import Text.Shakespeare.Text
import qualified UnliftIO as U

main :: IO ()
main = Spec.spec

test1 :: ML e m => m ()
test1 = do
  $logDebug [st|test1|]
  x <- runSqlRaw s3W [] "select 1"
  liftIO $ print x
