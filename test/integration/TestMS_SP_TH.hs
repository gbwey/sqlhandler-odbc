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
import Sql_TH
import DBMSSQL
import Sql
import Util
import Text.Shakespeare.Text
import TablePrinter
import TestConnections
import Data.Vinyl
import Language.Haskell.TH.Syntax (mkName)

$(genMSType "MS1" msW (const "select count(*), 'abc' as field2 from mixed"))
-- eg Sql (DBMS a) '[] '[Sel MS1, U0]

$(genMSType "MS2" msW (\f -> [st|select count(*), 'abc' as field2 from mixed where description not like #{f "?"} and id>#{f "?"}|]))

ms1a :: Sql (DBMS Writeable) '[String,Int] '[SelOne MS2]
ms1a = mkSql' "select count(*), 'abc' as field2 from mixed"

$(genMSWith defGenOpts { _goDBParam = mkName "a", _goSel = ''Sel, _goEnc = [t| '[] |] } "ms1" msW (const "select top 10 * from mixed"))

$(genMSWith defGenOpts { _goSel = ''SelOne } "ms2bad" msW (const "select 1 as ff,'fixedval1' as astring,cast (123.4 as float),567.8,567.8z"))
$(genMSWith defGenOpts { _goSel = ''SelOne, _goDBParam = ''Writeable } "ms3bad" msW (const "select 1 as ff,'fixedval1' as astring,cast (123.4 as float),567.8"))

$(genMSWith defGenOpts { _goSel = ''SelOne } "ms4" msW (const "select 1 as ff,'fixedval1' as astring,cast (123.4 as float),567.8e,567.8e"))
$(genMSWith defGenOpts { _goSel = ''SelOne, _goDBParam = ''Writeable } "ms5" msW (const "select 1 as ff,'fixedval1' as astring,cast (123.4 as float),567.8e"))

$(genMSWith defGenOpts { _goDBParam = ''Writeable } "ms6" msW (const [st|
  select top 10 a.*, o.id as otherid, o.total as othertotal, o.*
  from mixed a, mixed o
  where a.id=o.id
  and a.description not like '%six%'
  order by o.id desc
  |]))

$(genMS "ms7" msW ''Writeable [st|
  select top 10 *
  from mixed a, mixed c
  where c.id = a.id
  |])

$(genMS "ms8" msW (mkName "a") [st|
  select top 10 *
  from mixed a, mixed c
  where c.id = a.id
  order by c.id desc
  |])

-- passing in parameters
-- [t| Sql (DBMS a) '[String] (Sel x) |]
$(genMSWith defGenOpts { _goEnc = [t| '[Int] |], _goDBParam = ''ReadOnly } "ms9" msW (\f -> [st|
  select top 10 *
  from mixed a, mixed o
  where a.id = o.id
  and o.id > #{f "?"}
  order by o.id desc
  |]))

$(genMSWith defGenOpts { _goSel = ''SelOne, _goNameFunc = id } "ms10" msW (const [st|
  select 'abc' as XYZ, 'def' as [    @$  aA], 'ghi', 'jkl' as XYZ, 'mno' as field1
  |]))

$(genMSWith defGenOpts { _goSel = ''SelOne } "ms11" msW (const [st|
  select 'abc' as XYZ, 'def' as [    @$  aA], 'ghi', 'jkl' as XYZ, 'mno' as field1
  |]))

-- ms treats bit as bool but is Maybe cos looks like numbers are treated not null by default but strings seem fine
$(genMSWith defGenOpts { _goSel = ''SelOne } "ms12" msW (const [st|
  select cast(0 as bit),cast (1 as bit),2,3
  |]))

-- a trick to make bool not null by wrapping in isnull (same with datetime): dont need to worry about strings
$(genMSWith defGenOpts { _goSel = ''SelOne, _goDBParam = ''ReadOnly } "ms13" msW (const [st|
  select isnull(cast(0 as bit),0),isnull(cast (1 as bit),1),isnull(cast('2009-01-20' as datetime),0),'abc'
  |]))

-- convert vs cast makes no diff
$(genMSWith defGenOpts { _goSel = ''SelOne, _goDBParam = ''Writeable } "ms14" msW (const [st|
  select convert(bit,0), convert(bit,1), cast('2009-01-20' as datetime)
  |]))



