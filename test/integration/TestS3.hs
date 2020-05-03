{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
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
import Predicate
import Predicate.Refined
import Predicate.Refined3
import Predicate.Examples.Refined3
import GHC.TypeLits (Nat)
import Data.Kind (Type)
import TestSqlite_TH
import Spec

main :: IO ()
main = do
  Spec.spec
  x <- fs $ runSql s3W RNil s3_test1
  print $ ext x
