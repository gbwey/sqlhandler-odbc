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
module TestConnections where
import Data.Text (Text)
import HSql.ODBC.GConn
import HSql.ODBC.Sql_TH
import Language.Haskell.TH
import HSql.ODBC.DBMSSQL
import HSql.ODBC.DBOracle
import HSql.ODBC.DBSqlite ()
import HSql.ODBC.DBPG ()
import HSql.ODBC.DBMY ()
import Database.Postgres
import Database.MySql
import Database.Sqlite

sqliteX :: Text -> DBSqlite Writeable
sqliteX fn = DBSqlite "{SQLite3 ODBC Driver}" fn (DbDict [("Timeout", "10000"), ("NoTxn", "1")])

-- 1. big disadvantage over genConn is this is a value without the type signature so you get warnings
-- 2. big disadvantage is it doesnt work for 'a' : typed th doesnt support polymorphic types
-- 3. advantage is a bit clearer what is going on
myW :: DBMY Writeable
myW = $$(genConn2 "myW")

--myW = $$(genConn2 @(DBMY Writeable) "myW") -- this works but is less clear and gives warnings cos no signature

msW :: DBMS Writeable
msW = $$(genConn2 "msW")

pgW :: DBPG Writeable
pgW = $$(genConn2 "pgW")

s3W :: DBSqlite Writeable
s3W = $$(genConn2 "s3W")

msR :: DBMS ReadOnly
msR = $$(genConn2 "msW")

-- msR = $$(genConn1 @DBMS @ReadOnly "msR")

--msa = $$(genConn1 @DBMS "msa")
--orW = $$(genConn1 @DBOracle "orW")

-- typed th doesnt work with polymorphic types hence using this
$(genConn @DBOracle "orW" (mkName "a"))
$(genConn @DBMS "msa" (mkName "a"))

--msa = $$(genConn2 @DBMS @_ "msa")
{-
$(genConn @DBMY "myW" ''Writeable)
$(genConn @DBMS "msW" ''Writeable)
$(genConn @DBPG "pgW" ''Writeable)
$(genConn @DBOracle "orW" (mkName "a"))
$(genConn @DBSqlite "s3W" ''Writeable)

$(genConn @DBMS "msR" ''ReadOnly)
$(genConn @DBMS "msa" (mkName "a"))
-}