{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TestDBConn where

import Data.Pos
import Data.Proxy
import Database.Postgres
import HSql.ODBC.DBMSSQL
import HSql.ODBC.DBOracle
import HSql.ODBC.DBPG ()
import HSql.ODBC.GConn
import Test.Tasty
import Test.Tasty.HUnit

suite :: TestTree
suite =
  testGroup
    "TestDBConn"
    [ testCase "insertTable.ex1" $ (@?=) (insertTableSqlPrivate @(DBMS _) (_P @1, _P @5) "[fred]") ("insert into [fred] values (?,?,?,?,?)", _P @5)
    , testCase "insertTable.ex2" $ (@?=) (insertTableSqlPrivate @(DBMS _) (_P @2, _P @5) "dbo.fred") ("insert into dbo.fred values (?,?,?,?,?), (?,?,?,?,?)", _P @10)
    , testCase "insertTable.ex3" $ (@?=) (insertTableSqlPrivate @(DBPG _) (_P @2, _P @5) "fred") ("insert into fred values (?,?,?,?,?), (?,?,?,?,?)", _P @10)
    , testCase "insertTable.ex4" $ (@?=) (insertTableSqlPrivate @(DBOracle _) (_P @3, _P @3) "\"fred\"") ("insert into \"fred\" values (?,?,?), (?,?,?), (?,?,?)", _P @9)
    , testCase "escapeField.ex1" $ (@?=) (escapeField (Proxy @(DBMS _)) "fred") "[fred]"
    , testCase "escapeField.ex2" $ (@?=) (escapeField (Proxy @(DBPG _)) "fred") "\"fred\""
    , testCase "escapeField.ex3" $ (@?=) (escapeField (Proxy @(DBOracle _)) "fred") "\"fred\""
    ]

expectLeft :: Show b => Either a b -> IO ()
expectLeft = \case
  Left _ -> pure ()
  Right e -> assertFailure $ "expected Left but found Right " ++ show e
