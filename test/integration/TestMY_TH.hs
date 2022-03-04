{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TestMY_TH where

import Data.Vinyl
import Database.DBMY
import HSql.Core.Sql
import HSql.Core.TablePrinter
import HSql.ODBC.DBConn
import HSql.ODBC.DBMY ()
import HSql.ODBC.Sql_TH
import Logging
import TestConnections
import Text.Shakespeare.Text

-- mysql count takes way too long
$(genSql "my1" myW (\f -> [st|select count(*) as cnt1 from mixed #{f ""}|]))
$(genSqlLR "my2" myW (\_ll _rr lr -> [st|select #{lr "1" "count(*)"} from mixed #{lr "limit 0" "limit 5"}|]))

-- can even split the queries entirely
$(genSqlLR "my3" myW (\_ll _rr lr -> lr "select count(*) as fred from mixed limit 0" "select count(*) from mixed"))

-- fd $ runSql myW (I2 1 "C0001%") my20d
$(genSqlLRWith defGenOpts{goEnc = [t|'[Int, String]|]} "my4" myW (\_ll _rr lr -> lr "select * from mixed limit 0" "select * from mixed where id>? and description not like ?"))

$(genSqlWith defGenOpts{goSel = ''SelRow} "my5" myW (\f -> [st|select count(*) as cnt1,'xxx' as field2,123 as numfld from mixed #{f ""}|]))

$(genSql "my6" myW (\f -> [st|select *,'somejunk' as xyz from mixed #{f ""}|]))
