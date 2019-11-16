module Spec where
import qualified TestDBConn
import qualified TestSqlParser
import Test.Tasty

spec :: IO ()
spec = defaultMain $ testGroup "alltests"
  [ TestDBConn.suite
  , TestSqlParser.suite
  ]
