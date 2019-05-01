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
import DBFrame
import Sql
import Util
import Control.Monad.IO.Class
import Text.Shakespeare.Text
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vinyl
import TablePrinter
--import Data.Functor
import Data.Time
import Control.Lens
import TestConnections
--import EasyTest
--import Test.Hspec
--import qualified Frames as F
--import Frames (Frame(..),(:->))

$(genSqlWith defGenOpts { _goEnc = [t| '[Int,Integer,Int] |], _goDBParam = ''Writeable } "fn17" sqlite1 (\f -> [st|select * from orders where ord_num in (?,?,?) #{f ""}|]))

$(genSqlWith defGenOpts { _goEnc = [t| '[Int,Int] |] } "fn17b" sqlite1 (\f -> [st|select * from orders where ord_num between ? and ? #{f ""}|]))
-- cos of limit 0 on count it says it is a string so this dont work unless we drop the limit
$(genSql "fn18a" sqlite1 (\f -> [st|select * from orders #{f "limit 10"}|]))

-- skip the limit but will be slower: but will give the right type
-- else will treat as a string!
$(genSql "fn19" sqlite1 (const "select count(*) as znork from orders"))

