{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TestOracle_TH where

import HSql.Core.Sql
import HSql.Core.TablePrinter
import HSql.ODBC.DBConn
import HSql.ODBC.DBOracle
import HSql.ODBC.Sql_TH
import TestConnections
import Text.Shakespeare.Text

$(genSql "or1" orW (\f -> [st|select * from mixed where 1=1 and #{f "rownum < 5"} |]))

$(genSqlWith defGenOpts{goEnc = [t|'[Int, Integer, Int]|], goDBParam = ''ReadOnly} "or2" orW (\f -> [st|select * from mixed where id in (?,?,?) and #{f "1=1"}|]))

$(genSql "or3" orW (\f -> [st|select count(*) as znork from mixed where #{f "1=1"}|]))

$(genSqlWith defGenOpts{goSel = ''SelRow} "or4" orW (\f -> [st|select 123.45, 123.45e, cast(123.45 as numeric(10,2)) as fldtest, cast (123.45 as number), cast (123.45 as decimal), cast (123.45 as float) from dual where #{f "1=1"} |]))

-- every literal is a Maybe in oracle: in sqlserver strings are not null but the rest are by default Maybe
$(genSqlWith defGenOpts{goSel = ''SelRow} "or5" orW (\f -> [st|select cast(123.45 as float(5)) as fldtest, cast (123.45 as real), 'abcdef' from dual where #{f "1=1"} |]))
