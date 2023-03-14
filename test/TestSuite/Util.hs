module TestSuite.Util
  ( fromAssertions,
  )
where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)

fromAssertions :: String -> [Assertion] -> [TestTree]
fromAssertions name =
  zipWith testCase [printf "[%2d] %s" n name | n <- [1 :: Int ..]]
