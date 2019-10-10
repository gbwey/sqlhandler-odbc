module Spec where
import qualified TestDBConn
import qualified TestSqlParser
import EasyTest

spec :: IO ()
spec = do
  run TestDBConn.suite
  run TestSqlParser.suite
