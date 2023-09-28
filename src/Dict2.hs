{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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

module Dict2 where

import qualified Data.Aeson as Aeson
import Data.Kind
import qualified Data.Map.Strict as Map
import Data.Proxy
import GHC.TypeLits

type Dict = [Symbol]

class HasTranslation (translation :: Symbol) (container :: Dict)

instance HasTranslation translation (translation ': translations)

instance {-# OVERLAPPABLE #-} (HasTranslation translation translations) => HasTranslation translation (currentTranslation ': translations)

newtype Translation = Translation {translation :: String}
  deriving (Eq, Show, Aeson.FromJSON)

data TranslationTypeLevel (translation :: Symbol) = TranslationTypeLevel

class TranslationTypeLevelToTranslation a where
  toTranslation :: a -> Translation

instance TranslationTypeLevelToTranslation Translation where
  toTranslation = id

-- This Strange
type ValidTranslationTypeLevel translation = KnownSymbol translation

instance ValidTranslationTypeLevel translation => TranslationTypeLevelToTranslation (TranslationTypeLevel translation) where
  toTranslation _ = Translation (symbolVal (Proxy @translation))

-- IsColor == TranslationTypeLevelToTranslation
-- NamedColor == KeyTranslatable
-- ColorName == Key
-- KeyedTranslatable == NamedRGB

class TranslationTypeLevelToTranslation a => KeyTranslatable a where
  type Key a :: Symbol

translatableKey :: forall a. KnownSymbol (Key a) => a -> String
translatableKey _ = symbolVal $ Proxy @(Key a)

data KeyedTranslatable (key :: Symbol) (translation :: Symbol) = KeyedTranslatable

instance ValidTranslationTypeLevel translation => TranslationTypeLevelToTranslation (KeyedTranslatable key translation) where
  toTranslation _ = toTranslation $ (TranslationTypeLevel :: TranslationTypeLevel translation)

instance TranslationTypeLevelToTranslation (KeyedTranslatable key translation) => KeyTranslatable (KeyedTranslatable key translation) where
  type Key (KeyedTranslatable key translation) = key

keyedTranslatable :: forall key translation. (KnownSymbol key, ValidTranslationTypeLevel translation) => KeyedTranslatable key translation
keyedTranslatable = KeyedTranslatable

data MkDict theme where
  EmptyDict :: MkDict '[]
  AddDict ::
    (KnownSymbol (Key translation), KeyTranslatable translation) =>
    translation ->
    MkDict dict ->
    MkDict (Key translation : dict)

instantiateDict :: MkDict dict -> DictInstance dict
instantiateDict EmptyDict = DictInstance Map.empty
instantiateDict (AddDict translation mkDict') =
  let (DictInstance t) = instantiateDict mkDict'
      translationKey = translatableKey translation
      translationVal = SomeTranslation $ toTranslation translation
   in DictInstance $ Map.insert translationKey translationVal t

sampleDict =
  instantiateDict $
    AddDict (keyedTranslatable @"red" @"Rød") $
      AddDict (keyedTranslatable @"green" @"Grøn") $
        AddDict (keyedTranslatable @"blue" @"Blå") $
          EmptyDict

sampleLookup dict =
  ( lookupTranslation @"red" dict,
    lookupTranslation @"green" dict,
    lookupTranslation @"blue" dict
  )

{-
data GG a b c = GG
  { red :: a,
    green :: b,
    blue :: c
  }

myDict =
  GG
    { red = TranslationTypeLevel :: TranslationTypeLevel "rød",
      green = TranslationTypeLevel :: TranslationTypeLevel "grøn",
      blue = TranslationTypeLevel :: TranslationTypeLevel "blå"
    }

lol = toTranslation (red myDict)
-}

newtype DictInstance (dict :: Dict) = DictInstance {getDictInstance :: Map.Map String SomeTranslation}
  deriving (Eq, Show)

class ValidateDictInstance (dict :: Dict) (a :: Dict -> Type) where
  validateDictInstance :: Map.Map String SomeTranslation -> Either String (a dict)

instance ValidateDictInstance '[] DictInstance where
  validateDictInstance dict = Right (DictInstance dict)

instance
  ( KnownSymbol currentTranslation,
    ValidateDictInstance rest DictInstance
  ) =>
  ValidateDictInstance (currentTranslation : rest) DictInstance
  where
  validateDictInstance dict = do
    let targetTranslation = symbolVal $ Proxy @currentTranslation
     in case Map.lookup targetTranslation dict of
          Nothing ->
            let key = symbolVal $ Proxy @currentTranslation
             in Left $ "missing translation: " <> key
          Just _ -> do
            (DictInstance m) <- validateDictInstance @rest dict
            pure $ DictInstance m

dictInstance :: ValidateDictInstance dict DictInstance => Map.Map String SomeTranslation -> Either String (DictInstance dict)
dictInstance = validateDictInstance

lookupTranslation ::
  forall translationName dict.
  ( KnownSymbol translationName,
    HasTranslation translationName dict
  ) =>
  DictInstance dict ->
  SomeTranslation
lookupTranslation (DictInstance translations) =
  let targetName = symbolVal $ Proxy @translationName
   in translations Map.! targetName

demoDictInstance :: DictInstance ["red", "green", "blue"]
demoDictInstance =
  DictInstance . Map.fromList $
    [ ("red", someTranslation "Rød"),
      ("green", someTranslation "Grøn"),
      ("blue", someTranslation "Blå")
    ]

dictDemo ::
  ( HasTranslation "red" dict,
    HasTranslation "green" dict,
    HasTranslation "blue" dict
  ) =>
  DictInstance dict ->
  String
dictDemo dict =
  let r = lookupTranslation @"red" dict
      g = lookupTranslation @"green" dict
      b = lookupTranslation @"blue" dict
   in show (r, g, b)

type DemoDict = '["red", "green", "blue"]

type YellowDict = '["yellow"]

data SomeTranslation = forall translation. TranslationTypeLevelToTranslation translation => SomeTranslation translation

instance Show SomeTranslation where
  show = show . toTranslation

instance Eq SomeTranslation where
  x == y = (toTranslation x) == (toTranslation y)

someTranslation :: String -> SomeTranslation
someTranslation translation = SomeTranslation $ Translation translation

instance TranslationTypeLevelToTranslation SomeTranslation where
  toTranslation (SomeTranslation translation) = toTranslation translation
