module Hello.Color.Tests
  ( tests,
  )
where

import qualified Color
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Hello.Color.Tests" $
    concat
      []
