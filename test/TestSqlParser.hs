{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
module TestSqlParser where
import SqlParser
import Text.Regex.Applicative
import EasyTest

suite :: Test ()
suite = tests
  [ scope "sqlparser.blanklines.good" $ expectAll' (Just ()) (=~ matchBlankSqlRE) goods
  , scope "sqlparser.blanklines.bad" $ expectAll' Nothing (=~ matchBlankSqlRE) bads
  , scope "sqlparser.ex1" $ expectEq (matchSqlField (Just "tf") "   field1      int not null ") (Right [PColumn "field1" "int not null" "   tf.field1"])
  , scope "sqlparser.ex2" $ expectEq (matchSqlField Nothing "field1 int") (Right [PColumn "field1" "int" "field1"])
  , scope "sqlparser.ex3" $ expectEq (matchSqlField Nothing ",field1 int") (Right [PColumn "field1" "int" ",field1"])
  , scope "sqlparser.ex4" $ expectEq (matchSqlField Nothing "    ,  ") (Right [])
  , scope "sqlparser.ex5" $ expectEq (parseCreateTableSqlImpl "create table fred (a int )") (Right (PTable "fred" [PColumn "a" "int" "a"]))
  , scope "sqlparser.ex6" $ expectEq (parseCreateTableSqlImpl "create table fred (a int\n,b varchar(20) not null )") (Right (PTable "fred" [PColumn "a" "int" "a", PColumn "b" "varchar(20) not null" ",b"]))
  , scope "sqlparser.ex7" $ expectEq (matchSqlField (Just "tf") "   field1      int not null identity(1,1) ") (Right [PIdentity "field1" "int not null identity(1,1)" "   tf.field1"])
  , scope "sqlparser.ex8" $ expectEq (stripQuotes (Just ('\'','\'')) "'hello'") "hello"
  , scope "sqlparser.ex9" $ expectEq (stripQuotes (Just ('"','"')) "\"hello\"") "hello"
  , scope "sqlparser.ex10" $ expectEq (stripQuotes (Just ('[',']')) "[hello]") "hello"
  , scope "sqlparser.ex11" $ expectEq (stripQuotes (Just ('[',']')) "'hello'") "'hello'"
  ]

goods, bads :: [String]
goods =
  [""
  ,","
  ,"     "
  ,"    ,   "
  ,"/*x"
  ,"  , --"
  ]

bads =
  ["x"
  ,"   x--"
  ,"ab"
  ]


--testmatchblankgood = all ((== Just ()) . (=~ matchBlankSqlRE)) goods
--testmatchblankbad = all ((== Nothing) . (=~ matchBlankSqlRE)) bads

