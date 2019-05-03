{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -Wno-unused-imports #-}
module TestPG_TH where
import DBConn
import GConn
import DBPG
import Sql_TH
import Sql
import Util
import Text.Shakespeare.Text
import Data.Vinyl
import TablePrinter
import TestConnections

$(genSql "pgsql1" pgW (\f -> [st|select * from orders #{f "limit 4"}; select * from agents #{f "limit 3"}|]))
$(genTypeList 10 "PG1" pgW (\f -> [st|select 14 as dude,'xx' ; select * from orders #{f "limit 4"}|]))

$(genSql "pgsql2" pgW (\f -> [st|select * from orders #{f "limit 10"}; select 14|]))

$(genSqlWith defGenOpts { _goEnc = [t| '[] |] } "pgsql3" pgW (\f -> [st|select * from orders #{f "limit 10"}; select 14|]))
$(genSqlWith defGenOpts { _goDBParam = ''Writeable } "pgsql4" pgW (\f -> [st|select * from orders #{f "limit 10"}; select 14|]))


$(genSql "pgsql5" pgW (\f -> [st|select * from orders #{f "limit 10"}; select 14|]))

testsql :: Sql (DBPG a) '[] '[Sel (Int,Double)]
testsql = mkSql' "select 1,5"

$(genTypeFirst 10 "PG6" pgW (\f -> [st|select 14 as dude,'xx' ; select * from orders #{f "limit 4"}|]))
$(genTypeFirst 10 "PG7" pgW (\f -> [st|select cast (? as int) as dude,'xx' ; select * from orders #{f "limit 4"}|]))
$(genTypeFirst 10 "PG8" pgW (\f -> [st|select * from orders where ord_num in (?,?,?) #{f "limit 4"}|]))
-- must have the exact number of binders for mssql but not postgres

-- gives more fine grained control for more complex sql: eg using ctes where you need to do interesting things
$(genSqlLR "pgsql6" pgW (\_ll _rr lr -> [st|select * from orders where 1=1 #{lr "limit 0" "limit 5"}|]))
$(genSqlLR "pgsql7" pgW (\ll rr lr -> [st|select * from orders where 1=#{ll "0"}#{rr "1"} #{lr "limit 0" "limit 5"}|]))

$(genSqlWith defGenOpts { _goEnc = [t| '[Int] |], _goSel = ''SelOne } "pgsql8" pgW (\f -> [st|select cast (? as int) as dude,'xx', count(*) from orders #{f ""}|]))

