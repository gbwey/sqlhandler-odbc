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
module TestMS_SP_TH where
import DBConn
import GConn
import DBPG
import SqlUtils_TH
import Sql_TH
import DBMSSQL
import DBSqlite
import DBOracle
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
import Language.Haskell.TH.Syntax (mkName)

$(genMSType "FnA" msW (const "select count(*), 'abc' as field2 from orders"))
-- eg Sql (DBMS a) '[] '[Sel FnA, U0]

$(genMSType "FnB" msW (\f -> [st|select count(*), 'abc' as field2 from orders where cust_code <> #{f "?"} and ord_num>#{f "?"}|]))

myfn1a :: Sql (DBMS Writeable) '[String,Int] '[SelOne FnB]
myfn1a = mkSql' "select count(*), 'abc' as field2 from orders"

-- $(getMSTypeOLDSimple "Fn1" msW "select count(*), 'abc' as field2 from orders")

-- $(genMSSimple "myfn" msW "select top 10 * from orders")

-- yurk! way too much crap
-- $(getMSTypeOLD'' "Fn0" msW ''Writeable ''SelOne [t| '[String,Int] |] (const "select count(*), 'abc' as field2 from orders"))



$(genMSWith defGenOpts { _goDBParam = mkName "a", _goSel = ''Sel, _goEnc = [t| '[] |] } "myfn99" msW (const "select top 10 * from orders"))

$(genMSWith defGenOpts { _goSel = ''SelOne } "myfn1bad" msW (const "select 1 as ff,'dude' as astring,cast (123.4 as float),567.8,567.8z"))
$(genMSWith defGenOpts { _goSel = ''SelOne, _goDBParam = ''Writeable } "myfn2bad" msW (const "select 1 as ff,'dude' as astring,cast (123.4 as float),567.8"))

$(genMSWith defGenOpts { _goSel = ''SelOne } "myfn1" msW (const "select 1 as ff,'dude' as astring,cast (123.4 as float),567.8e,567.8e"))
$(genMSWith defGenOpts { _goSel = ''SelOne, _goDBParam = ''Writeable } "myfn2" msW (const "select 1 as ff,'dude' as astring,cast (123.4 as float),567.8e"))

-- dont have describe first resultset on work machine cos sqlserver express is too old
$(genMSWith defGenOpts { _goDBParam = ''Writeable } "myfn4" msW (const [st|
  select top 10 o.ord_num,o.ord_amount, c.cust_name, a.agent_name, a.commission
  from agents a, orders o, customers c
  where c.agent_code = a.agent_code
  and o.cust_code = c.cust_code
  and o.ord_num > '200125'
  order by o.ord_num desc
  |]))

$(genMS "myfn5" msW ''Writeable [st|
  select top 10 *
  from agents a, customers c
  where c.agent_code = a.agent_code
  |])

$(genMS "myfn6" msW (mkName "a") [st|
  select top 10 *
  from agents a, orders o, customers c
  where c.agent_code = a.agent_code
  and o.cust_code = c.cust_code
  and o.ord_num > '200125'
  order by o.ord_num desc
  |])

-- passing in parameters
-- [t| Sql (DBMS a) '[String] (Sel x) |]
$(genMSWith defGenOpts { _goEnc = [t| '[Int] |], _goDBParam = ''ReadOnly } "myfn6a" msW (\f -> [st|
  select top 10 *
  from agents a, orders o, customers c
  where c.agent_code = a.agent_code
  and o.cust_code = c.cust_code
  and o.ord_num > #{f "?"}
  order by o.ord_num desc
  |]))

$(genMSWith defGenOpts { _goSel = ''SelOne, _goNameFunc = id } "myfn6c" msW (const [st|
  select 'abc' as XYZ, 'def' as [    @$  aA], 'ghi', 'jkl' as XYZ, 'mno' as field1
  |]))

$(genMSWith defGenOpts { _goSel = ''SelOne } "myfn6d" msW (const [st|
  select 'abc' as XYZ, 'def' as [    @$  aA], 'ghi', 'jkl' as XYZ, 'mno' as field1
  |]))

-- cool treats bit as bool but is Maybe cos looks like numbers are treated not null by default but strings seem fine
$(genMSWith defGenOpts { _goSel = ''SelOne } "myfn6e" msW (const [st|
  select cast(0 as bit),cast (1 as bit),2,3
  |]))

-- a trick to make bool not null by wrapping in isnull (same with datetime): dont need to worry about strings
$(genMSWith defGenOpts { _goSel = ''SelOne, _goDBParam = ''ReadOnly } "myfn6f" msW (const [st|
  select isnull(cast(0 as bit),0),isnull(cast (1 as bit),1),isnull(cast('2009-01-20' as datetime),0),'abc'
  |]))

-- convert vs cast makes no diff
$(genMSWith defGenOpts { _goSel = ''SelOne, _goDBParam = ''Writeable } "myfn6g" msW (const [st|
  select convert(bit,0), convert(bit,1), cast('2009-01-20' as datetime)
  |]))



