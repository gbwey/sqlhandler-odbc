{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TestPG_TH where

import Data.Vinyl
import Database.Postgres
import HSql.Core.Sql
import HSql.Core.TablePrinter
import HSql.ODBC.DBConn
import HSql.ODBC.DBPG ()
import HSql.ODBC.Sql_TH
import Logging
import TestConnections
import Text.Shakespeare.Text

$(genSql "pgsql1" pgW (\f -> [st|select * from mixed #{f "limit 4"}; select id,description from mixed where id>2 #{f "limit 3"}|]))
$(genTypeList "PG1" pgW (\f -> [st|select 14 as fixedval1,'xx' ; select * from mixed #{f "limit 4"}|]))

$(genSql "pgsql2" pgW (\f -> [st|select * from mixed #{f "limit 10"}; select 14|]))

$(genSqlWith defGenOpts{goEnc = [t|'[]|]} "pgsql3" pgW (\f -> [st|select * from mixed #{f "limit 10"}; select 14|]))

$(genSqlWith defGenOpts{goDBParam = ''Writeable} "pgsql4" pgW (\f -> [st|select * from mixed #{f "limit 10"}; select 14|]))

$(genSql "pgsql5" pgW (\f -> [st|select * from mixed #{f "limit 10"}; select 14|]))

testsql :: Sql (DBPG a) '[] '[Sel (Int, Double)]
testsql = mkSql' "select 1,5"

$(genTypeFirst "PG6" pgW (\f -> [st|select 14 as fixedval1,'xx' ; select * from mixed #{f "limit 4"}|]))
$(genTypeFirst "PG7" pgW (\f -> [st|select cast (? as int) as fixedval1,'xx' ; select * from mixed #{f "limit 4"}|]))
$(genTypeFirst "PG8" pgW (\f -> [st|select * from mixed where id in (?,?,?) #{f "limit 4"}|]))

-- must have the exact number of binders for mssql but not postgres

-- gives more fine grained control for more complex sql: eg using ctes where you need to do interesting things
$(genSqlLR "pgsql6" pgW (\_ll _rr lr -> [st|select * from mixed where 1=1 #{lr "limit 0" "limit 5"}|]))
$(genSqlLR "pgsql7" pgW (\ll rr lr -> [st|select * from mixed where 1=#{ll "0"}#{rr "1"} #{lr "limit 0" "limit 5"}|]))

$(genSqlWith defGenOpts{goEnc = [t|'[Int]|], goSel = ''SelRow} "pgsql8" pgW (\f -> [st|select cast (? as int) as fixedval1,'xx', count(*) from mixed #{f ""}|]))
