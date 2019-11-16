{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wno-redundant-constraints #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
module TestSqlParser where
import SqlParser
import Text.Regex.Applicative
import Test.Tasty
import Test.Tasty.HUnit
import Data.Either
import Control.Arrow

suite :: TestTree
suite = testGroup "TestSqlParser"
  [ testCase "sqlparser.blanklines.good" $ expectAll' (Just ()) (=~ matchBlankSqlRE) goods
  , testCase "sqlparser.blanklines.bad" $ expectAll' Nothing (=~ matchBlankSqlRE) bads
  , testCase "sqlparser.ex1" $ (@?=) (matchSqlField (Just "tf") "   field1      int not null ") (Right [PColumn "field1" "int not null" "   tf.field1"])
  , testCase "sqlparser.ex2" $ (@?=) (matchSqlField Nothing "field1 int") (Right [PColumn "field1" "int" "field1"])
  , testCase "sqlparser.ex3" $ (@?=) (matchSqlField Nothing ",field1 int") (Right [PColumn "field1" "int" ",field1"])
  , testCase "sqlparser.ex4" $ (@?=) (matchSqlField Nothing "    ,  ") (Right [])
  , testCase "sqlparser.ex5" $ (@?=) (parseCreateTableSqlImpl "create table fred (a int )") (Right (PTable "fred" [PColumn "a" "int" "a"]))
  , testCase "sqlparser.ex6" $ (@?=) (parseCreateTableSqlImpl "create table fred (a int\n,b varchar(20) not null )") (Right (PTable "fred" [PColumn "a" "int" "a", PColumn "b" "varchar(20) not null" ",b"]))
  , testCase "sqlparser.ex7" $ (@?=) (matchSqlField (Just "tf") "   field1      int not null identity(1,1) ") (Right [PIdentity "field1" "int not null identity(1,1)" "   tf.field1"])
  , testCase "sqlparser.ex8" $ (@?=) (stripQuotes (Just ('\'','\'')) "'hello'") "hello"
  , testCase "sqlparser.ex9" $ (@?=) (stripQuotes (Just ('"','"')) "\"hello\"") "hello"
  , testCase "sqlparser.ex10" $ (@?=) (stripQuotes (Just ('[',']')) "[hello]") "hello"
  , testCase "sqlparser.ex11" $ (@?=) (stripQuotes (Just ('[',']')) "'hello'") "'hello'"
  ]

expectAll' :: (Show a, Show a1, Eq a, Eq a1) => a1 -> (a -> a1) -> [a] -> IO ()
expectAll' w p = expectAll (liftMaybe w . p)

expectAll :: (Eq a, Show a, HasCallStack, Show b) => (a -> Either b ()) -> [a] -> IO ()
expectAll p as = case lefts (map (\a -> left (a,) (p a)) as) of
                   [] -> pure ()
                   xs@(_:_) -> assertFailure $ "expected all to succeed but " <> show (length xs) <> " failed " <> show xs

liftMaybe :: Eq a => a -> a -> Either (a,a) ()
liftMaybe expected actual | expected == actual = Right ()
                          | otherwise = Left (expected, actual)




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

