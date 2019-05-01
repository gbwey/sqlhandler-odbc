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
module TestMY_TH where
import DBConn
import GConn
import SqlUtils_TH
import Sql_TH
import DBMY
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

-- mysql count takes way too long
$(genSql "fn20" myW (\f -> [st|select count(*) as znork from orders #{f ""}|]))
$(genSqlLR "fn20a" myW (\_ll _rr lr -> [st|select #{lr "1" "count(*)"} from orders #{lr "limit 0" "limit 5"}|]))

-- can even split the queries entirely
$(genSqlLR "fn20c" myW (\_ll _rr lr -> lr "select count(*) as fred from orders limit 0" "select count(*) from orders"))

-- fd $ runSql myW (I2 1 "C0001%") fn20d
$(genSqlLRWith defGenOpts { _goEnc = [t| '[Int,String] |] } "fn20d" myW (\_ll _rr lr -> lr "select * from orders limit 0" "select * from orders where ord_num>? and cust_code not like ?"))

$(genSqlWith defGenOpts { _goSel = ''SelOne } "fn20b" myW (\f -> [st|select count(*) as znork,'xxx' as field2,123 as numfld from orders #{f ""}|]))

$(genSql "fn21" myW (\f -> [st|select *,'somejunk' as xyz from orders #{f ""}|]))

