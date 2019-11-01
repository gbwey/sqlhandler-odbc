-- if you have an old version of mssql and cant use genMS*
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
module TestMSOld_TH where
import DBConn
import GConn
import Sql_TH
import DBMSSQL
import Sql
import Logging
import Text.Shakespeare.Text
import Data.Vinyl
import TablePrinter
import TestConnections

--main :: IO ()
--main = putStrLn "th stuff"

-- ms requires exact number of binders: the other dbs can be larger
$(genTypeFirst "MSOLD1" msW (\f -> [st|select 14 as fixedval1,'xx' ; select #{f "top 4"} * from mixed|]))

$(genTypeFirst "MSOLD2" msW (\f -> [st|select #{f "top 4"} * from mixed where id in (?,?,?)|]))

$(genSql "msold1" msW (\f -> [st|select #{f "top 5"} * from mixed where 1=1 |]))

$(genSqlWith defGenOpts { _goEnc = [t| '[Int,Integer,Int] |], _goDBParam = ''ReadOnly } "msold2" msW (\f -> [st|select #{f ""} * from mixed where id in (?,?,?) |]))

$(genSql "msold3" msW (\f -> [st|select #{f ""} count(*) as cnt1 from mixed |]))

$(genSqlWith defGenOpts { _goEnc = [t| '[Int,Integer] |], _goDBParam = ''Writeable } "msold4" msW (\f -> [st|select #{f ""} * from mixed where id between ? and ? |]))

-- gives more fine grained control for more complex sql: eg using ctes where you need to do interesting things
$(genSqlLR "msold5" msW (\_ll _rr lr -> [st|select top #{lr "0" "5"} * from mixed where 1=1 |]))

$(genSqlLR "msold6" msW (\ll rr lr -> [st|select top #{lr "0" "5"} * from mixed where 1=#{ll "0"}#{rr "1"} |]))

