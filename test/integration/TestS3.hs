{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.IO.Class
import HSql.ODBC.DBConn
import HSql.ODBC.DBSqlite ()
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
import Text.Shakespeare.Text

main :: IO ()
main = Spec.spec

test1 :: ML e m => m ()
test1 = do
  $logDebug [st|test1|]
  x <- runSqlRaw s3W [] "select 1"
  liftIO $ print x
