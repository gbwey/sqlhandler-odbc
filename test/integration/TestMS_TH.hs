{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module TestMS_TH where

import HSql.Core.Sql
import HSql.ODBC.DBConn
import HSql.ODBC.DBMSSQL
import HSql.ODBC.Sql_TH
import Language.Haskell.TH.Syntax (mkName)
import TestConnections
import Text.Shakespeare.Text

main :: IO ()
main = putStrLn "th stuff"

$(genMSType "MS1" msW (const "select count(*) as fred, 'abc' as field2 from mixed"))

-- eg Sql (DBMS a) '[] '[Sel MS1, U0]

$(genMSType "MS2" msW (\f -> [st|select count(*), 'abc' as field2 from mixed where description not like #{f "?"} and id>#{f "?"}|]))

ms1a :: Sql (DBMS Writeable) '[String, Int] '[SelRow MS2]
ms1a = mkSql' "select count(*), 'abc' as field2 from mixed"

$(genMSWith defGenOpts{goDBParam = mkName "a", goSel = ''Sel, goEnc = [t|'[]|]} "ms1" msW (const "select top 10 * from mixed"))

$(genMSWith defGenOpts{goSel = ''SelRow} "ms2bad" msW (const "select 1 as ff,'fixedval1' as astring,cast (123.4 as float),567.8,567.8z"))
$(genMSWith defGenOpts{goSel = ''SelRow, goDBParam = ''Writeable} "ms3bad" msW (const "select 1 as ff,'fixedval1' as astring,cast (123.4 as float),567.8"))

$(genMSWith defGenOpts{goSel = ''SelRow} "ms4" msW (const "select 1 as ff,'fixedval1' as astring,cast (123.4 as float),567.8e,567.8e"))
$(genMSWith defGenOpts{goSel = ''SelRow, goDBParam = ''Writeable} "ms5" msW (const "select 1 as ff,'fixedval1' as astring,cast (123.4 as float),567.8e"))

$( genMSWith
    defGenOpts{goDBParam = ''Writeable}
    "ms6"
    msW
    ( const
        [st|
  select top 10 a.*, o.id as otherid, o.total as othertotal, o.*
  from mixed a, mixed o
  where a.id=o.id
  and a.description not like '%six%'
  order by o.id desc
  |]
    )
 )

$( genMS
    "ms7"
    msW
    ''Writeable
    [st|
  select top 10 *
  from mixed a, mixed c
  where c.id = a.id
  |]
 )

$( genMS
    "ms8"
    msW
    (mkName "a")
    [st|
  select top 10 *
  from mixed a, mixed c
  where c.id = a.id
  order by c.id desc
  |]
 )

-- passing in parameters
-- [t| Sql (DBMS a) '[String] (Sel x) |]
$( genMSWith
    defGenOpts{goEnc = [t|'[Int]|], goDBParam = ''ReadOnly}
    "ms9"
    msW
    ( \f ->
        [st|
  select top 10 *
  from mixed a, mixed o
  where a.id = o.id
  and o.id > #{f "?"}
  order by o.id desc
  |]
    )
 )

$( genMSWith
    defGenOpts{goSel = ''SelRow, goNameFunc = id}
    "ms10"
    msW
    ( const
        [st|
  select 'abc' as XYZ, 'def' as [    @$  aA], 'ghi', 'jkl' as XYZ, 'mno' as field1
  |]
    )
 )

$( genMSWith
    defGenOpts{goSel = ''SelRow}
    "ms11"
    msW
    ( const
        [st|
  select 'abc' as XYZ, 'def' as [    @$  aA], 'ghi', 'jkl' as XYZ, 'mno' as field1
  |]
    )
 )

-- ms treats bit as bool but is Maybe cos looks like numbers are treated not null by default but strings seem fine
$( genMSWith
    defGenOpts{goSel = ''SelRow}
    "ms12"
    msW
    ( const
        [st|
  select cast(0 as bit),cast (1 as bit),2,3
  |]
    )
 )

-- a trick to make bool not null by wrapping in isnull (same with datetime): dont need to worry about strings
$( genMSWith
    defGenOpts{goSel = ''SelRow, goDBParam = ''ReadOnly}
    "ms13"
    msW
    ( const
        [st|
  select isnull(cast(0 as bit),0),isnull(cast (1 as bit),1),isnull(cast('2009-01-20' as datetime),0),'abc'
  |]
    )
 )

-- convert vs cast makes no diff
$( genMSWith
    defGenOpts{goSel = ''SelRow, goDBParam = ''Writeable}
    "ms14"
    msW
    ( const
        [st|
  select convert(bit,0), convert(bit,1), cast('2009-01-20' as datetime)
  |]
    )
 )
