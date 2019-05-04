{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS -Wall #-}
module TestConnections where
import Prelude hiding (FilePath)
import GConn
import DBMSSQL
import DBOracle
import DBSqlite
import DBPG
import DBMY
import System.IO (FilePath)
import Sql_TH
import Language.Haskell.TH

sqliteX :: FilePath -> DBSqlite Writeable
sqliteX = DBSqlite "DRIVER=SQLite3 ODBC Driver;Timeout=10000;NoTxn=1" Nothing

$(genConn @DBMY "myW" ''Writeable)
$(genConn @DBMS "msW" ''Writeable)
$(genConn @DBPG "pgW" ''Writeable)
$(genConn @DBOracle "orW" (mkName "a"))
$(genConn @DBSqlite "s3W" ''Writeable)

$(genConn @DBMS "msR" ''ReadOnly)
$(genConn @DBMS "msa" (mkName "a"))
