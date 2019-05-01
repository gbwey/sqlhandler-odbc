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
module TestOracle_TH where
import DBConn
import GConn
import SqlUtils_TH
import Sql_TH
import DBOracle
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

$(genSql "myfn13" orW (\f -> [st|select * from Agents where 1=1 and #{f "rownum < 5"} |]))

$(genSqlWith defGenOpts { _goEnc = [t| '[Int,Integer,Int] |], _goDBParam = ''ReadOnly } "myfn14" orW (\f -> [st|select * from Orders where ord_num in (?,?,?) and #{f "1=1"}|]))

$(genSql "myfn15" orW (\f -> [st|select count(*) as znork from Orders where #{f "1=1"}|]))

$(genSqlWith defGenOpts { _goSel = ''SelOne } "myfn16" orW (\f -> [st|select 123.45, 123.45e, cast(123.45 as numeric(10,2)) as fldtest, cast (123.45 as number), cast (123.45 as decimal), cast (123.45 as float) from dual where #{f "1=1"} |]))

-- every literal is a Maybe in oracle: in sqlserver strings are not null but the rest are by default Maybe
$(genSqlWith defGenOpts { _goSel = ''SelOne } "myfn17" orW (\f -> [st|select cast(123.45 as float(5)) as fldtest, cast (123.45 as real), 'abcdef' from dual where #{f "1=1"} |]))

