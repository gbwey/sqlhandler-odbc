{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module TestMY_TH where
import HSql.ODBC.DBConn
import HSql.ODBC.Sql_TH
import HSql.ODBC.DBMY ()
import Database.DBMY
import HSql.Core.Sql
import Logging
import Text.Shakespeare.Text
import Data.Vinyl
import HSql.Core.TablePrinter
import TestConnections

-- mysql count takes way too long
$(genSql "my1" myW (\f -> [st|select count(*) as cnt1 from mixed #{f ""}|]))
$(genSqlLR "my2" myW (\_ll _rr lr -> [st|select #{lr "1" "count(*)"} from mixed #{lr "limit 0" "limit 5"}|]))

-- can even split the queries entirely
$(genSqlLR "my3" myW (\_ll _rr lr -> lr "select count(*) as fred from mixed limit 0" "select count(*) from mixed"))

-- fd $ runSql myW (I2 1 "C0001%") my20d
$(genSqlLRWith defGenOpts { _goEnc = [t| '[Int,String] |] } "my4" myW (\_ll _rr lr -> lr "select * from mixed limit 0" "select * from mixed where id>? and description not like ?"))

$(genSqlWith defGenOpts { _goSel = ''SelOne } "my5" myW (\f -> [st|select count(*) as cnt1,'xxx' as field2,123 as numfld from mixed #{f ""}|]))

$(genSql "my6" myW (\f -> [st|select *,'somejunk' as xyz from mixed #{f ""}|]))

