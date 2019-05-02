-- code for running against test databases
{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
module TestDBConn where
import DBConn
import GConn
import DBPG
import DBMSSQL
import DBOracle
import Sql
import Data.Proxy
import Data.Functor
import EasyTest

suite :: Test ()
suite = tests
  [ scope "insertTable.ex1" $ expectEq (insertTableSqlPrivate @(DBMS _) (1,5) "fred") ("insert into [fred] values (?,?,?,?,?)", 5)
  , scope "insertTable.ex2" $ expectEq (insertTableSqlPrivate @(DBMS _) (2,5) "dbo.fred") ("insert into dbo.[fred] values (?,?,?,?,?), (?,?,?,?,?)", 10)
  , scope "insertTable.ex3" $ expectEq (insertTableSqlPrivate @(DBPG _) (2,5) "fred") ("insert into \"fred\" values (?,?,?,?,?), (?,?,?,?,?)", 10)
  , scope "insertTable.ex4" $ expectEq (insertTableSqlPrivate @(DBOracle _) (3,3) "fred") ("insert into \"fred\" values (?,?,?), (?,?,?), (?,?,?)", 9)
  , scope "parseTableLR.ex1" $ expectEq (parseTableLR @(DBMS _) "aaa.bbb.[c]") (Right (Table (Just "aaa") (Schema (Just "bbb")) "c" True))
  , scope "parseTableLR.ex2" $ void $ expectLeft (parseTableLR @(DBMS _) "c")
  , scope "parseTableLR.ex3" $ expectEq (parseTableLR @(DBPG _) "x.\"abc\"") (Right (Table Nothing (Schema (Just "x")) "abc" True))
  , scope "escapeField.ex1" $ expectEq (escapeField (Proxy @(DBMS _)) "fred") "[fred]"
  , scope "escapeField.ex2" $ expectEq (escapeField (Proxy @(DBPG _)) "fred") "\"fred\""
  , scope "escapeField.ex3" $ expectEq (escapeField (Proxy @(DBOracle _)) "fred") "\"fred\""
  ]

