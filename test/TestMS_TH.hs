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
import SqlUtils_TH
import Sql_TH
import DBMSSQL
import DBFrame
import Sql
import Util
import Control.Monad.IO.Class
import Text.Shakespeare.Text
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vinyl
import TablePrinter
import Data.Time
import Control.Lens
import TestConnections

-- ms requires exact number of binders: the other dbs can be larger
$(genTypeFirst 0 "TP5A" msW (\f -> [st|select 14 as dude,'xx' ; select #{f "top 4"} * from orders|]))
$(genTypeFirst 3 "TP8A" msW (\f -> [st|select #{f "top 4"} * from orders where ord_num in (?,?,?)|]))

$(genSql "fn13" msW (\f -> [st|select #{f "top 5"} * from Agents where 1=1 |]))
$(genSqlWith defGenOpts { _goEnc = [t| '[Int,Integer,Int] |], _goDBParam = ''ReadOnly } "fn14" msW (\f -> [st|select #{f ""} * from Orders where ord_num in (?,?,?) |]))
$(genSql "fn15" msW (\f -> [st|select #{f ""} count(*) as znork from Orders |]))

$(genSqlWith defGenOpts { _goEnc = [t| '[Int,Integer] |], _goDBParam = ''Writeable } "fn17" msW (\f -> [st|select #{f ""} * from orders where ord_num between ? and ? |]))
-- gives more fine grained control for more complex sql: eg using ctes where you need to do interesting things
$(genSqlLR "fn17a" msW (\_ll _rr lr -> [st|select top #{lr "0" "5"} * from orders where 1=1 |]))
$(genSqlLR "fn17a1" msW (\ll rr lr -> [st|select top #{lr "0" "5"} * from orders where 1=#{ll "0"}#{rr "1"} |]))

