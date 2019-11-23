{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
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
module TestPG_TH where
import DBConn
import GConn
import DBPG ()
import Database.Postgres
import Sql_TH
import Sql
import Logging
import Text.Shakespeare.Text
import Data.Vinyl
import TablePrinter
import TestConnections

$(genSql "pgsql1" pgW (\f -> [st|select * from mixed #{f "limit 4"}; select id,description from mixed where id>2 #{f "limit 3"}|]))
$(genTypeList "PG1" pgW (\f -> [st|select 14 as fixedval1,'xx' ; select * from mixed #{f "limit 4"}|]))

$(genSql "pgsql2" pgW (\f -> [st|select * from mixed #{f "limit 10"}; select 14|]))

$(genSqlWith defGenOpts { _goEnc = [t| '[] |] } "pgsql3" pgW (\f -> [st|select * from mixed #{f "limit 10"}; select 14|]))

$(genSqlWith defGenOpts { _goDBParam = ''Writeable } "pgsql4" pgW (\f -> [st|select * from mixed #{f "limit 10"}; select 14|]))

$(genSql "pgsql5" pgW (\f -> [st|select * from mixed #{f "limit 10"}; select 14|]))

testsql :: Sql (DBPG a) '[] '[Sel (Int,Double)]
testsql = mkSql' "select 1,5"

$(genTypeFirst "PG6" pgW (\f -> [st|select 14 as fixedval1,'xx' ; select * from mixed #{f "limit 4"}|]))
$(genTypeFirst "PG7" pgW (\f -> [st|select cast (? as int) as fixedval1,'xx' ; select * from mixed #{f "limit 4"}|]))
$(genTypeFirst "PG8" pgW (\f -> [st|select * from mixed where id in (?,?,?) #{f "limit 4"}|]))
-- must have the exact number of binders for mssql but not postgres

-- gives more fine grained control for more complex sql: eg using ctes where you need to do interesting things
$(genSqlLR "pgsql6" pgW (\_ll _rr lr -> [st|select * from mixed where 1=1 #{lr "limit 0" "limit 5"}|]))
$(genSqlLR "pgsql7" pgW (\ll rr lr -> [st|select * from mixed where 1=#{ll "0"}#{rr "1"} #{lr "limit 0" "limit 5"}|]))

$(genSqlWith defGenOpts { _goEnc = [t| '[Int] |], _goSel = ''SelOne } "pgsql8" pgW (\f -> [st|select cast (? as int) as fixedval1,'xx', count(*) from mixed #{f ""}|]))

