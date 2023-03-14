{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ColorTheme where

import Color
import ColorX11
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Kind
import qualified Data.Map as M
import Data.Proxy
import qualified Data.Text as Text
import Data.Word
import GHC.TypeLits
import Text.Read

newtype ColorReference r a = ColorReference {unColorReference :: ExceptT String (Reader r) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader r, MonadError String)

type family HKD (wrapper :: Type -> Type) (value :: Type) :: Type where
  HKD Identity value = value
  HKD wrapper value = wrapper value

data ColorValue w
  = RGBValue RGB
  | X11Value SomeColor
  | OtherColor (HKD w (ColorValue w))

instance IsColor (ColorValue Identity) where
  toRGB (RGBValue rgb) = rgb
  toRGB (X11Value c) = toRGB c
  toRGB (OtherColor ref) = toRGB ref

newtype ThemeConfig' w = ThemeConfig' {getThemeConfig :: M.Map String (ColorValue w)}

type ThemeConfig = ThemeConfig' Identity

newtype RawThemeConfig = RawThemeConfig {getRawThemeConfig :: ThemeConfig' (ColorReference RawThemeConfig)}
  deriving newtype (FromJSON)

instance FromJSON (ThemeConfig' (ColorReference RawThemeConfig)) where
  parseJSON = fmap ThemeConfig' . parseJSON

type RuntimeTheme = ["blue", "green", "red"]

validateThemeConfig ::
  forall (theme :: Theme).
  ValidateThemeInstance theme ThemeInstance =>
  ThemeConfig ->
  Either String (ThemeInstance theme)
validateThemeConfig = validateThemeInstance . M.map SomeColor . getThemeConfig

evalConfig :: RawThemeConfig -> Either String ThemeConfig
evalConfig rawConfig =
  fmap ThemeConfig'
    . traverse (dereferenceColorValue rawConfig)
    . getThemeConfig
    . getRawThemeConfig
    $ rawConfig

loadRuntimeTheme :: FilePath -> IO ThemeConfig
loadRuntimeTheme p = do
  contents <- BS.readFile p
  case eitherDecode' contents >>= evalConfig of
    Left err -> ioError $ userError err
    Right val -> pure val

testQuery :: FilePath -> IO ()
testQuery p = do
  cfg <- loadRuntimeTheme p
  let sampleQuery t = (lookupColor @"red" t, lookupColor @"blue" t)
      r = sampleQuery <$> validateThemeConfig @RuntimeTheme cfg
  print r

dereferenceColorValue :: RawThemeConfig -> ColorValue (ColorReference RawThemeConfig) -> Either String (ColorValue Identity)
dereferenceColorValue env colorValue =
  case colorValue of
    RGBValue r -> pure $ RGBValue r
    X11Value c -> pure $ X11Value c
    OtherColor r -> do
      referencedColor <- dereferenceColorValue env =<< evalColorReference env r
      case referencedColor of
        OtherColor _ -> pure referencedColor
        _ -> pure $ OtherColor referencedColor

evalColorReference env =
  flip runReader env . runExceptT . unColorReference

instance FromJSON (ColorValue (ColorReference RawThemeConfig)) where
  parseJSON = withObject "color" $ \val ->
    parseRGBElement val <|> parseX11Element val <|> parseRefElement val
    where
      parseRGBElement val = do
        t <- val .: "rgb"
        RGBValue <$> parseRGB t

      parseX11Element val = do
        t <- val .: "x11"
        X11Value <$> parseX11Color t

      parseRefElement val = do
        refName <- val .: "same-as"
        pure . OtherColor $ generateRef refName

      generateRef name = do
        colors <- asks (getThemeConfig . getRawThemeConfig)
        case M.lookup name colors of
          Nothing -> throwError $ "Referenced color not found: " <> name
          Just color' -> pure color'

parseRGB :: MonadFail m => String -> m RGB
parseRGB s =
  case s of
    '#' : s' -> parseRGB s'
    [r, r', g, g', b, b'] -> do
      RGB
        <$> parseHex r r'
        <*> parseHex g g'
        <*> parseHex b b'
    _ ->
      fail $
        "invalid RGB hex string "
          <> s
          <> " expected a string in the form of #1a2b3c"
  where
    parseHex a b =
      case readEither ['0', 'x', a, b] of
        Left err -> fail err
        Right val -> pure val

parseX11Color :: MonadFail m => String -> m SomeColor
parseX11Color colorName =
  let (ThemeInstance t) = x11Theme
   in case M.lookup colorName t of
        Nothing -> fail $ "no x11 color " <> colorName
        Just color -> pure color
