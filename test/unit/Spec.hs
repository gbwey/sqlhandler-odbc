module Main where
import qualified TestDBConn
import qualified TestSqlParser
import EasyTest

main :: IO ()
main = do
  run TestDBConn.suite
  run TestSqlParser.suite
