{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TestSqlite_TH where

import Data.Kind (Type)
import Data.Time
import Data.Vinyl
import Database.Sqlite
import GHC.TypeLits (Nat)
import HSql.Core.Sql
import HSql.Core.TablePrinter
import HSql.ODBC.DBConn
import HSql.ODBC.DBSqlite ()
import HSql.ODBC.Sql_TH
import Logging

{-
import Predicate
import qualified Predicate.Examples.Refined2 as R2
import qualified Predicate.Examples.Refined3 as R3
import Predicate.Refined
import qualified Predicate.Refined2 as R2
import qualified Predicate.Refined3 as R3
-}
import TestConnections
import Text.Shakespeare.Text

$(genSqlWith defGenOpts{goEnc = [t|'[Int, Integer, Int]|], goDBParam = ''Writeable} "s3_1" s3W (\f -> [st|select * from mixed where id in (?,?,?) #{f ""}|]))

$(genSqlWith defGenOpts{goEnc = [t|'[Int, Int]|]} "s3_2" s3W (\f -> [st|select * from mixed where id between ? and ? #{f ""}|]))

-- cos of limit 0 on count it says it is a string so this dont work unless we drop the limit
$(genSql "s3_3" s3W (\f -> [st|select * from mixed #{f "limit 10"}|]))

-- skip the limit but will be slower: but will give the right type
-- else will treat as a string!
$(genSql "s3_4" s3W (const "select count(*) as cnt1 from mixed"))

-- s3.db
s3_3TEST :: Sql (DBSqlite a) '[] '[Sel (Int, Double, UTCTime, String)]
s3_3TEST = mkSql' "select * from mixed"

{-
s3_3TEST1 :: Sql (DBSqlite a) '[] '[Sel (Int, Refined OA (Between 0 500 (Ceiling Int)) Double, UTCTime, String)]
s3_3TEST1 = mkSql' "select * from mixed"

s3_CARD' :: Sql (DBSqlite a) '[] '[Sel (Int, String, R2.MakeR2 (R2.Luhn OA 11), R2.MakeR2 (R2.BaseN OA 16 'True), R2.MakeR2 (R2.DateTimeN OA))]
s3_CARD' = mkSql' "select * from cardinfo where id < 5"

s3_CARD1' :: Sql (DBSqlite a) '[] '[Sel (Int, String, R2.MakeR2 (R2.Luhn OA 11), R2.MakeR2 (R2.BaseN OA 16 'True), R2.MakeR2 (R2.DateTimeN OA))]
s3_CARD1' = mkSql' "select * from cardinfo where id <> 6"

s3_CARD2' :: Sql (DBSqlite a) '[] '[Sel (Int, String, R2.MakeR2 (R2.Luhn OA 11), R2.MakeR2 (R2.BaseN OA 16 'True), R2.MakeR2 (R2.DateTimeN OA))]
s3_CARD2' = mkSql' "select * from cardinfo where id <> 7"

s3_CARD :: Sql (DBSqlite a) '[] '[Sel (Int, String, R3.MakeR3 (R3.Luhn OA '[4, 4, 3]), R3.MakeR3 (R3.BaseN OA 16 'True), R3.MakeR3 (R3.DateTimeN OA))]
s3_CARD = mkSql' "select * from cardinfo where id < 5"

s3_CARD1 :: Sql (DBSqlite a) '[] '[Sel (Int, String, R3.MakeR3 (R3.Luhn OA '[4, 4, 3]), R3.MakeR3 (R3.BaseN OA 16 'True), R3.MakeR3 (R3.DateTimeN OA))]
s3_CARD1 = mkSql' "select * from cardinfo where id <> 6"

s3_CARD2 :: Sql (DBSqlite a) '[] '[Sel (Int, String, R3.MakeR3 (R3.Luhn OA '[4, 4, 3]), R3.MakeR3 (R3.BaseN OA 16 'True), R3.MakeR3 (R3.DateTimeN OA))]
s3_CARD2 = mkSql' "select * from cardinfo where id <> 7"

type DateTimeN1 opts =
  '( opts
   , ParseTimeP UTCTime "%Y-%m-%d %H:%M:%S"
   , 'True
   , FormatTimeP "%Y-%m-%d %H:%M:%S"
   , String
   )
-}
s3_test0 :: Sql (DBSqlite a) '[] '[Sel (Int, String)]
s3_test0 = mkSql' "select 1234 as intvalue, 'some text' as txtvalue"

{-
s3_test1 :: Sql (DBSqlite a) '[] '[Sel (R3.MakeR3 (FromSeconds OA UTCTime), R3.MakeR3 (R3.Ssn OA))]
s3_test1 = mkSql' "select 1546304461 as seconds, '123-34-2224' as ssn union all select 0, '123-12-2222'"

s3_test2 :: Sql (DBSqlite a) '[] '[Sel (R3.MakeR3 (FromSeconds OA UTCTime), R3.MakeR3 (R3.Ssn OA))]
s3_test2 = mkSql' "select 1546304461 as seconds, '123-34-2224' as ssn union all select 0, '666-12-2222'"

s3_test3 :: Sql (DBSqlite a) '[] '[Sel (R3.MakeR3 (R3.Luhn OA '[4, 4, 3]), R3.MakeR3 (R3.BaseN OA 16 'True), R3.MakeR3 (DateTimeN1 OA))]
s3_test3 =
  mkSql'
    [st|select '1234-5678-903' as cardnumber, 'ff' as hexvalue, '2001-07-04 12:13:14' as targetdate
            -- union all select '1234-5678-903', 'aa0g45', '2001-07-04 12:13:14'
            -- union all select '1234-5678-904', 'aa045', '2001-07-04 12:13:14'
            -- union all select '1234-5678-903', 'aa045', '2001-07-32 12:13:14'
            union all select '3333-1111-709', '4822e', '2019-09-02 23:04:59'
          |]

s3_test4 :: Sql (DBSqlite a) '[] '[Sel (R3.MakeR3 (R3.Luhn OA '[4, 4, 3]), R3.MakeR3 (R3.BaseN OA 16 'True), R3.MakeR3 (R3.DateTimeN OA), R3.MakeR3 (R3.BaseN OA 2 'True))]
s3_test4 =
  mkSql'
    [st|select '1234-5678-903' as cardnumber, 'ff' as hexvalue, '2019-12-22 12:13:14' as targetdate, '10000111' as binvalue
union all select '1111-2841-991', '128ab', 'June 21 2009 12:13:14', '0000011'
union all select '6433-1000-006', '278fec', '11/30/09 12:13:29', '111'
union all select '1234-5678-903', '100', '2001-07-04 12:13:07', '1101010'
union all select '1111-2841-991', '999', 'January 21 2009 12:13:28', '11000111'
union all select '6433-1000-006', '278fed', '27/12/09 12:13:14', '1111'
union all select '6433-1000-006', 'ffff', 'September 18 2012 23:01:03', '11110101011'
  |]

type FromSeconds (opts :: Opt) (t :: Type) = '(opts, ShowP Id >> ParseTimeP t "%s", 'True, FormatTimeP "%s" >> ReadP Integer Id, Integer)
-}
