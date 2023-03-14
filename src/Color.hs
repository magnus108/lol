{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Color where

import Data.Kind
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Word (Word8)
import GHC.TypeLits
import Text.Printf

infixr 6 :++:

type a :++: b = AppendSymbol a b

data RGB = RGB
  { rgbRed :: Word8,
    rgbGreen :: Word8,
    rgbBlue :: Word8
  }
  deriving (Eq, Show)

class IsColor a where
  toRGB :: a -> RGB

class IsColor a => NamedColor a where
  type ColorName a :: Symbol

colorNameVal :: forall a. KnownSymbol (ColorName a) => a -> String
colorNameVal _ = symbolVal $ Proxy @(ColorName a)

data RGBColor (r :: Nat) (g :: Nat) (b :: Nat) = RGBColor

instance ValidRGB r g b => IsColor (RGBColor r g b) where
  toRGB _ = RGB (natWord8 @r) (natWord8 @g) (natWord8 @b)
    where
      natWord8 :: forall n. (KnownNat n, n <= 255) => Word8
      natWord8 = fromIntegral $ natVal (Proxy @n)

data NamedRGB (name :: Symbol) (r :: Nat) (g :: Nat) (b :: Nat) = NamedRGB

namedRGB :: forall name r g b. (KnownSymbol name, ValidRGB r g b) => NamedRGB name r g b
namedRGB = NamedRGB

instance ValidRGB r g b => IsColor (NamedRGB name r g b) where
  toRGB _ = toRGB $ (RGBColor :: RGBColor r g b)

type family NatHex (n :: Nat) :: Symbol where
  NatHex 0 = "0"
  NatHex 1 = "1"
  NatHex 2 = "2"
  NatHex 3 = "3"
  NatHex 4 = "4"
  NatHex 5 = "5"
  NatHex 6 = "6"
  NatHex 7 = "7"
  NatHex 8 = "8"
  NatHex 9 = "9"
  NatHex 10 = "10"
  NatHex 11 = "11"
  NatHex 12 = "12"
  NatHex 13 = "13"
  NatHex 14 = "14"
  NatHex 15 = "15"
  NatHex n = NatHex (Div n 16) :++: NatHex (Mod n 16)

type family PadNatHex (n :: Nat) :: Symbol where
  PadNatHex n = IfThenElse (n <=? 15) ("0" :++: NatHex n) (NatHex n)

instance IsColor (RGBColor r g b) => NamedColor (RGBColor r g b) where
  type
    ColorName (RGBColor r g b) =
      "#" :++: PadNatHex r :++: PadNatHex g :++: PadNatHex b

instance IsColor (NamedRGB name r g b) => NamedColor (NamedRGB name r g b) where
  type ColorName (NamedRGB name r g b) = name

type ValidRGB r g b =
  (KnownNat r, KnownNat g, KnownNat b, r <= 255, g <= 255, b <= 255)

instance IsColor RGB where
  toRGB = id

data SomeColor = forall color. IsColor color => SomeColor color

instance Show SomeColor where
  show = show . toRGB

someRGB :: Word8 -> Word8 -> Word8 -> SomeColor
someRGB r g b = SomeColor $ RGB r g b

instance IsColor SomeColor where
  toRGB (SomeColor color) = toRGB color

type Theme = [Symbol]

newtype ThemeInstance (a :: Theme) = ThemeInstance
  {getThemeInstance :: Map.Map String SomeColor}
  deriving (Show)

class HasColor (color :: Symbol) (container :: Theme)

instance HasColor color (color ': colors)

instance {-# OVERLAPPABLE #-} (HasColor color colors) => HasColor color (currentColor ': colors)

type family IfThenElse (p :: Bool) (t :: a) (f :: a) where
  IfThenElse True t f = t
  IfThenElse False t f = f

lookupColor ::
  forall
    colorName
    theme.
  ( KnownSymbol colorName,
    HasColor colorName theme
  ) =>
  ThemeInstance theme ->
  SomeColor
lookupColor (ThemeInstance colors) =
  let targetname = symbolVal $ Proxy @colorName
   in colors Map.! targetname

colorDemo ::
  ( HasColor "red" theme,
    HasColor "green" theme,
    HasColor "blue" theme
  ) =>
  ThemeInstance theme ->
  String
colorDemo theme =
  let r = lookupColor @"red" theme
      g = lookupColor @"green" theme
      b = lookupColor @"blue" theme
   in show (r, g, b)

data MkTheme theme where
  NewTheme :: MkTheme '[]
  AddColor ::
    (KnownSymbol (ColorName color), NamedColor color) =>
    color ->
    MkTheme theme ->
    MkTheme (ColorName color : theme)

instantiateTheme :: MkTheme theme -> ThemeInstance theme
instantiateTheme NewTheme = ThemeInstance Map.empty
instantiateTheme (AddColor color mkTheme') =
  let (ThemeInstance t) = instantiateTheme mkTheme'
      colorName = colorNameVal color
      colorVal = SomeColor $ toRGB color
   in ThemeInstance $ Map.insert colorName colorVal t

class ValidateThemeInstance (theme :: Theme) (a :: Theme -> Type) where
  validateThemeInstance :: Map.Map String SomeColor -> Either String (a theme)

instance ValidateThemeInstance '[] ThemeInstance where
  validateThemeInstance theme = Right (ThemeInstance theme)

instance
  ( KnownSymbol currentColor,
    ValidateThemeInstance rest ThemeInstance
  ) =>
  ValidateThemeInstance (currentColor : rest) ThemeInstance
  where
  validateThemeInstance theme =
    let targetColor = symbolVal $ Proxy @currentColor
     in case Map.lookup targetColor theme of
          Nothing ->
            let colorName = symbolVal $ Proxy @currentColor
             in Left $ "missing color: " <> colorName
          Just _ -> do
            (ThemeInstance m) <- validateThemeInstance @rest theme
            pure $ ThemeInstance m
