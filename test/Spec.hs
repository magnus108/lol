module Main
  ( main,
  )
where

import qualified Hello.Color.Tests
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ Hello.Color.Tests.tests
      ]
