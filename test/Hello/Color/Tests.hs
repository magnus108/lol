{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hello.Color.Tests
  ( tests,
  )
where

import Color (colorNameVal)
import qualified Color
import qualified ColorX11
import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit
import TestSuite.Util

tests :: TestTree
tests =
  testGroup "Hello.Color.Tests" $
    concat
      [ fromAssertions
          "colorDemo"
          [ ("RGB {rgbRed = 255, rgbGreen = 0, rgbBlue = 0}", "RGB {rgbRed = 0, rgbGreen = 255, rgbBlue = 0}", "RGB {rgbRed = 0, rgbGreen = 0, rgbBlue = 255}")
              @=? (Color.colorDemo ColorX11.x11Theme)
          ],
        fromAssertions
          "ThemeInstance"
          [ Color.ThemeInstance {Color.getThemeInstance = Map.fromList [("Red", Color.someRGB 0 0 255)]}
              @=? (Color.ThemeInstance (Map.insert "Red" (Color.lookupColor @"Blue" ColorX11.x11Theme) Map.empty))
          ],
        fromAssertions
          "toRGB"
          [ Color.RGB 0 0 255 @=? Color.toRGB ColorX11.Blue,
            Color.toRGB (Color.RGBColor @0 @0 @255) @=? Color.toRGB ColorX11.Blue,
            Color.toRGB (Color.NamedRGB @"Blue" @0 @0 @255) @=? Color.toRGB ColorX11.Blue,
            Color.toRGB (Color.namedRGB @"Blue" @0 @0 @255) @=? Color.toRGB ColorX11.Blue
          ],
        fromAssertions
          "colorNameVal"
          [ "Blue" @=? Color.colorNameVal ColorX11.Blue,
            "#000091511" @=? Color.colorNameVal (Color.RGBColor @0 @0 @2555)
          ]
      ]
