module Spec where

import Test.Tasty
import qualified TestDBConn

spec :: IO ()
spec =
  defaultMain $
    testGroup
      "alltests"
      [ TestDBConn.suite
      ]
