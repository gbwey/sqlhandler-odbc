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
module TestOracle_TH where
import HSql.ODBC.DBConn
import HSql.ODBC.Sql_TH
import HSql.ODBC.DBOracle
import HSql.Core.Sql
import Logging
import Text.Shakespeare.Text
import Data.Vinyl
import HSql.Core.TablePrinter
import TestConnections

$(genSql "or1" orW (\f -> [st|select * from mixed where 1=1 and #{f "rownum < 5"} |]))

$(genSqlWith defGenOpts { _goEnc = [t| '[Int,Integer,Int] |], _goDBParam = ''ReadOnly } "or2" orW (\f -> [st|select * from mixed where id in (?,?,?) and #{f "1=1"}|]))

$(genSql "or3" orW (\f -> [st|select count(*) as znork from mixed where #{f "1=1"}|]))

$(genSqlWith defGenOpts { _goSel = ''SelOne } "or4" orW (\f -> [st|select 123.45, 123.45e, cast(123.45 as numeric(10,2)) as fldtest, cast (123.45 as number), cast (123.45 as decimal), cast (123.45 as float) from dual where #{f "1=1"} |]))

-- every literal is a Maybe in oracle: in sqlserver strings are not null but the rest are by default Maybe
$(genSqlWith defGenOpts { _goSel = ''SelOne } "or5" orW (\f -> [st|select cast(123.45 as float(5)) as fldtest, cast (123.45 as real), 'abcdef' from dual where #{f "1=1"} |]))

