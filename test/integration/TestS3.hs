{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
module Main where
import DBConn
import GConn
import Sql_TH
import DBSqlite ()
import Database.Sqlite
import Sql
import Logging
import Text.Shakespeare.Text
import Data.Vinyl
import TablePrinter
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
