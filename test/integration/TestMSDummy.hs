{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module TestMSDummy where
import DBConn
import GConn
import Sql_TH
import DBMSSQL
import Sql
import Logging
import Text.Shakespeare.Text
import TablePrinter
import TestConnections
import Data.Vinyl
import Language.Haskell.TH.Syntax (mkName)
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH as TH
