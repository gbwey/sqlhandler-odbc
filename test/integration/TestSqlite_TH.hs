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
module TestSqlite_TH where
import DBConn
import GConn
import Sql_TH
import DBSqlite
import Sql
import Util
import Text.Shakespeare.Text
import Data.Vinyl
import TablePrinter
import TestConnections

$(genSqlWith defGenOpts { _goEnc = [t| '[Int,Integer,Int] |], _goDBParam = ''Writeable } "s3_1" s3W (\f -> [st|select * from mixed where id in (?,?,?) #{f ""}|]))

$(genSqlWith defGenOpts { _goEnc = [t| '[Int,Int] |] } "s3_2" s3W (\f -> [st|select * from mixed where id between ? and ? #{f ""}|]))

-- cos of limit 0 on count it says it is a string so this dont work unless we drop the limit
$(genSql "s3_3" s3W (\f -> [st|select * from mixed #{f "limit 10"}|]))

-- skip the limit but will be slower: but will give the right type
-- else will treat as a string!
$(genSql "s3_4" s3W (const "select count(*) as cnt1 from mixed"))

