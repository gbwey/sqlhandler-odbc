{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE KindSignatures #-}
module TestSqlite_TH where
import DBConn
import GConn
import Sql_TH
import DBSqlite
import Sql
import Logging
import Text.Shakespeare.Text
import Data.Vinyl
import TablePrinter
import TestConnections
import Data.Time
import Predicate
import Refined
import Refined3
import Refined3Helper
import GHC.TypeLits (Nat)
import Data.Kind (Type)

$(genSqlWith defGenOpts { _goEnc = [t| '[Int,Integer,Int] |], _goDBParam = ''Writeable } "s3_1" s3W (\f -> [st|select * from mixed where id in (?,?,?) #{f ""}|]))

$(genSqlWith defGenOpts { _goEnc = [t| '[Int,Int] |] } "s3_2" s3W (\f -> [st|select * from mixed where id between ? and ? #{f ""}|]))

-- cos of limit 0 on count it says it is a string so this dont work unless we drop the limit
$(genSql "s3_3" s3W (\f -> [st|select * from mixed #{f "limit 10"}|]))

-- skip the limit but will be slower: but will give the right type
-- else will treat as a string!
$(genSql "s3_4" s3W (const "select count(*) as cnt1 from mixed"))

-- s3.db
s3_3TEST :: Sql (DBSqlite a) '[] '[Sel (Int,Double,UTCTime,String)]
s3_3TEST = mkSql' "select * from mixed"

s3_3TEST1 :: Sql (DBSqlite a) '[] '[Sel (Int, Refined (Ceiling Int Id >> Between 0 500) Double, UTCTime, String)]
s3_3TEST1 = mkSql' "select * from mixed"

s3_CARD :: Sql (DBSqlite a) '[] '[Sel (Int, String, MakeR3 (Ccn '[4,4,3]), MakeR3 (BaseN 16), MakeR3 DateTimeN)]
s3_CARD = mkSql' "select * from cardinfo where id < 5"

s3_CARD1 :: Sql (DBSqlite a) '[] '[Sel (Int, String, MakeR3 (Ccn '[4,4,3]), MakeR3 (BaseN 16), MakeR3 DateTimeN)]
s3_CARD1 = mkSql' "select * from cardinfo where id <> 6"

s3_CARD2 :: Sql (DBSqlite a) '[] '[Sel (Int, String, MakeR3 (Ccn '[4,4,3]), MakeR3 (BaseN 16), MakeR3 DateTimeN)]
s3_CARD2 = mkSql' "select * from cardinfo where id <> 7"

type DateTimeN1 =
  '(ParseTimeP UTCTime "%Y-%m-%d %H:%M:%S" Id, 'True,
    FormatTimeP "%Y-%m-%d %H:%M:%S" Id, String)

s3_test1 :: Sql (DBSqlite a) '[] '[Sel (MakeR3 (FromSeconds UTCTime), MakeR3 Ssn)]
s3_test1 = mkSql' "select 1546304461 as seconds, '123-34-2224' as ssn union all select 0, '123-12-2222'"

s3_test2 :: Sql (DBSqlite a) '[] '[Sel (MakeR3 (FromSeconds UTCTime), MakeR3 Ssn)]
s3_test2 = mkSql' "select 1546304461 as seconds, '123-34-2224' as ssn union all select 0, '666-12-2222'"

s3_test3 :: Sql (DBSqlite a) '[] '[Sel (MakeR3 (Ccn '[4,4,3]), MakeR3 (BaseN 16), MakeR3 DateTimeN1)]
s3_test3 = mkSql' [st|select '1234-5678-903' as cardnumber, 'ff' as hexvalue, '2001-07-04 12:13:14' as targetdate
            -- union all select '1234-5678-903', 'aa0g45', '2001-07-04 12:13:14'
            -- union all select '1234-5678-904', 'aa045', '2001-07-04 12:13:14'
            -- union all select '1234-5678-903', 'aa045', '2001-07-32 12:13:14'
            union all select '3333-1111-709', '4822e', '2019-09-02 23:04:59'
          |]

s3_test4 :: Sql (DBSqlite a) '[] '[Sel (MakeR3 (Ccn '[4,4,3]), MakeR3 (BaseN 16), MakeR3 DateTimeN, MakeR3 (BaseN 2))]
s3_test4 = mkSql' [st|select '1234-5678-903' as cardnumber, 'ff' as hexvalue, '2019-12-22 12:13:14' as targetdate, '10000111' as binvalue
union all select '1111-2841-991', '128ab', 'June 21 2009 12:13:14', '0000011'
union all select '6433-1000-006', '278fec', '11/30/09 12:13:29', '111'
union all select '1234-5678-903', '100', '2001-07-04 12:13:07', '1101010'
union all select '1111-2841-991', '999', 'January 21 2009 12:13:28', '11000111'
union all select '6433-1000-006', '278fed', '27/12/09 12:13:14', '1111'
union all select '6433-1000-006', 'ffff', 'September 18 2012 23:01:03', '11110101011'
  |]


type FromSeconds (t :: Type) = '(ShowP >> ParseTimeP t "%s" Id, 'True, FormatTimeP "%s" Id >> ReadP Integer, Integer)
{-
:l test\integration\TestSqlite_TH.hs test\integration\TestConnections.hs
a <- fd $ runSql s3W RNil s3_CARD

>formatTime defaultTimeLocale "%s %S" (read @UTCTime "2019-01-01 01:01:01.0")
"1546304461 01"
it :: String
>parseTimeM @Maybe @UTCTime True defaultTimeLocale "%s" "10"
Just 1970-01-01 00:00:10 UTC
it :: Maybe UTCTime


>wprintWith (poC (+100) $ poAscii defT) a

resultset 1 Sel
+----+-------------+----------------------------------------------+
| id |    name     |                     card                     |
+----+-------------+----------------------------------------------+
|  1 | abel tasman | R3:([1,2,3,4,5,6,7,8,9,0,3],"1234-5678-903") |
+----+-------------+----------------------------------------------+
|  2 | james cook  | R3:([1,1,1,1,2,8,4,1,9,9,1],"1111-2841-991") |
+----+-------------+----------------------------------------------+
|  3 | van dieman  | R3:([6,4,3,3,1,0,0,0,0,0,6],"6433-1000-006") |
+----+-------------+----------------------------------------------+

>wprintWith (poWrap $ poAscii defT) a

resultset 1 Sel
+----+-------------+------------------------------------------+
| id |    name     |                   card                   |
+----+-------------+------------------------------------------+
|  1 | abel tasman | R3:([1,2,3,4,5,6,7,8,9,0,3],"1234-5678-9 |
|    |             | 03")                                     |
+----+-------------+------------------------------------------+
|  2 | james cook  | R3:([1,1,1,1,2,8,4,1,9,9,1],"1111-2841-9 |
|    |             | 91")                                     |
+----+-------------+------------------------------------------+
|  3 | van dieman  | R3:([6,4,3,3,1,0,0,0,0,0,6],"6433-1000-0 |
|    |             | 06")                                     |
+----+-------------+------------------------------------------+

>wprintWith (poC (subtract 10) $ poWrap $ poAscii defT) a

resultset 1 Sel
+----+-------------+--------------------------------+
| id |    name     |              card              |
+----+-------------+--------------------------------+
|  1 | abel tasman | R3:([1,2,3,4,5,6,7,8,9,0,3],"1 |
|    |             | 234-5678-903")                 |
+----+-------------+--------------------------------+
|  2 | james cook  | R3:([1,1,1,1,2,8,4,1,9,9,1],"1 |
|    |             | 111-2841-991")                 |
+----+-------------+--------------------------------+
|  3 | van dieman  | R3:([6,4,3,3,1,0,0,0,0,0,6],"6 |
|    |             | 433-1000-006")                 |
+----+-------------+--------------------------------+

-}

