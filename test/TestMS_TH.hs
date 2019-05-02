-- if you have an old version of mssql and cant use genMS*
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
module TestMS_TH where
import DBConn
import GConn
import Sql_TH
import DBMSSQL
import Sql
import Util
import Text.Shakespeare.Text
import Data.Vinyl
import TablePrinter
import TestConnections

-- ms requires exact number of binders: the other dbs can be larger
$(genTypeFirst 0 "MSOLD1" msW (\f -> [st|select 14 as dude,'xx' ; select #{f "top 4"} * from orders|]))

$(genTypeFirst 3 "MSOLD2" msW (\f -> [st|select #{f "top 4"} * from orders where ord_num in (?,?,?)|]))

$(genSql "msold1" msW (\f -> [st|select #{f "top 5"} * from Agents where 1=1 |]))

$(genSqlWith defGenOpts { _goEnc = [t| '[Int,Integer,Int] |], _goDBParam = ''ReadOnly } "msold2" msW (\f -> [st|select #{f ""} * from Orders where ord_num in (?,?,?) |]))

$(genSql "msold3" msW (\f -> [st|select #{f ""} count(*) as znork from Orders |]))

$(genSqlWith defGenOpts { _goEnc = [t| '[Int,Integer] |], _goDBParam = ''Writeable } "msold4" msW (\f -> [st|select #{f ""} * from orders where ord_num between ? and ? |]))

-- gives more fine grained control for more complex sql: eg using ctes where you need to do interesting things
$(genSqlLR "msold5" msW (\_ll _rr lr -> [st|select top #{lr "0" "5"} * from orders where 1=1 |]))

$(genSqlLR "msold6" msW (\ll rr lr -> [st|select top #{lr "0" "5"} * from orders where 1=#{ll "0"}#{rr "1"} |]))

