{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module TestDBConn where
import Test.Tasty
import Test.Tasty.HUnit
import HSql.ODBC.GConn
import Data.Proxy
import Data.Functor
import HSql.ODBC.DBMSSQL
import HSql.ODBC.DBOracle
import HSql.ODBC.DBPG ()
import Database.Postgres

suite :: TestTree
suite = testGroup "TestDBConn"
  [ testCase "insertTable.ex1" $ (@?=) (insertTableSqlPrivate @(DBMS _) (1,5) "fred") ("insert into [fred] values (?,?,?,?,?)", 5)
  , testCase "insertTable.ex2" $ (@?=) (insertTableSqlPrivate @(DBMS _) (2,5) "dbo.fred") ("insert into dbo.[fred] values (?,?,?,?,?), (?,?,?,?,?)", 10)
  , testCase "insertTable.ex3" $ (@?=) (insertTableSqlPrivate @(DBPG _) (2,5) "fred") ("insert into \"fred\" values (?,?,?,?,?), (?,?,?,?,?)", 10)
  , testCase "insertTable.ex4" $ (@?=) (insertTableSqlPrivate @(DBOracle _) (3,3) "fred") ("insert into \"fred\" values (?,?,?), (?,?,?), (?,?,?)", 9)
  , testCase "parseTableLR.ex1" $ (@?=) (parseTableLR @(DBMS _) "aaa.bbb.[c]") (Right (Table (Just "aaa") (Schema (Just "bbb")) "c" True))
  , testCase "parseTableLR.ex2" $ void $ expectLeft (parseTableLR @(DBMS _) "c")
  , testCase "parseTableLR.ex3" $ (@?=) (parseTableLR @(DBPG _) "x.\"abc\"") (Right (Table Nothing (Schema (Just "x")) "abc" True))
  , testCase "escapeField.ex1" $ (@?=) (escapeField (Proxy @(DBMS _)) "fred") "[fred]"
  , testCase "escapeField.ex2" $ (@?=) (escapeField (Proxy @(DBPG _)) "fred") "\"fred\""
  , testCase "escapeField.ex3" $ (@?=) (escapeField (Proxy @(DBOracle _)) "fred") "\"fred\""
  ]

expectLeft :: Show b => Either a b -> IO ()
expectLeft = \case
  Left _ -> pure ()
  Right e -> assertFailure $ "expected Left but found Right " ++ show e

