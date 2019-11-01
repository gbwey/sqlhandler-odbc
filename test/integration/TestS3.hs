{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Main where
import DBConn
import GConn
import Sql_TH
import DBSqlite
import Sql
import Logging
import Text.Shakespeare.Text
import Data.Vinyl
import TablePrinter
import TestConnections
import Data.Time
import Predicate
import Refined
import Refined3
import Refined3Helper
import GHC.TypeLits (Nat)
import Data.Kind (Type)
import TestSqlite_TH
import Spec

main :: IO ()
main = do
  Spec.spec
  x <- fs $ runSql s3W RNil s3_test1
  putStrLn $ show $ ext x
