{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module TestConnections where

import Data.Text (Text)
import Database.MySql
import Database.Postgres
import Database.Sqlite
import HSql.ODBC.DBMSSQL
import HSql.ODBC.DBMY ()
import HSql.ODBC.DBOracle
import HSql.ODBC.DBPG ()
import HSql.ODBC.DBSqlite ()
import HSql.ODBC.GConn
import HSql.ODBC.Sql_TH
import Language.Haskell.TH

sqliteX :: Text -> DBSqlite Writeable
sqliteX fn = DBSqlite "{SQLite3 ODBC Driver}" fn (DbDict [("Timeout", "10000"), ("NoTxn", "1")])

myW :: DBMY Writeable
myW = $$(genConn2 "myW")

msW :: DBMS Writeable
msW = $$(genConn2 "msW")

pgW :: DBPG Writeable
pgW = $$(genConn2 "pgW")

s3W :: DBSqlite Writeable
s3W = $$(genConn2 "s3W")

msR :: DBMS ReadOnly
msR = $$(genConn2 "msW")

-- typed th doesnt work with polymorphic types hence using this
$(genConn @DBOracle "orW" (mkName "a"))
$(genConn @DBMS "msa" (mkName "a"))
