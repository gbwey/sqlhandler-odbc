module Spec where
import qualified TestDBConn
import qualified TestSqlParser

spec :: IO ()
spec = do
  TestDBConn.suite
  TestSqlParser.suite
