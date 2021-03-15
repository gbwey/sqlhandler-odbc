{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
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

test1 :: ML e m => m ()
test1 = do
  $logDebug [st|test1|]
  x <- runSqlRaw s3W [] "select 1"
  liftIO $ print x

