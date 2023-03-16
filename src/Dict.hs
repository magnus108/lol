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

module Dict where

import Data.Kind
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Word (Word8)
import GHC.TypeLits

data Translation = Translation {unTranslation :: String}
  deriving (Eq, Show)

class IsTranslatable a where
  toTranslation :: a -> Translation

class IsTranslatable a => Translated a where
  type TranslationKey a :: Symbol

translationKeyVal :: forall a. KnownSymbol (TranslationKey a) => a -> String
translationKeyVal _ = symbolVal $ Proxy @(TranslationKey a)

data TranslatedTranslation (key :: Symbol) (translation :: Symbol) = TranslatedTranslation

instance ValidTranslation translation => IsTranslatable (TranslatedTranslation key translation) where
  toTranslation _ = Translation (symbolVal $ Proxy @translation)

type ValidTranslation translation = (KnownSymbol translation)

translatedTranslation :: forall key translation. (KnownSymbol key, ValidTranslation translation) => TranslatedTranslation key translation
translatedTranslation = translatedTranslation

instance IsTranslatable Translation where
  toTranslation = id

data SomeTranslatable = forall translation. IsTranslatable translation => SomeTranslatable translation

instance Show SomeTranslatable where
  show = show . toTranslation

instance IsTranslatable SomeTranslatable where
  toTranslation (SomeTranslatable translation) = toTranslation translation

someTranslatable :: String -> SomeTranslatable
someTranslatable x = SomeTranslatable $ Translation x

type Dict = [Symbol]

newtype DictInstance (a :: Dict) = DictInstance
  {getDictInstance :: Map.Map String SomeTranslatable}
  deriving (Show)

class HasTranslation (translation :: Symbol) (container :: Dict)

instance HasTranslation translation (translation ': translations)

instance {-# OVERLAPPABLE #-} (HasTranslation translation translations) => HasTranslation translation (currentTranslation ': translations)

lookupTranslation ::
  forall
    translation
    dict.
  ( KnownSymbol translation,
    HasTranslation translation dict
  ) =>
  DictInstance dict ->
  SomeTranslatable
lookupTranslation (DictInstance dict) =
  let targetname = symbolVal $ Proxy @translation
   in dict Map.! targetname

translationDemo ::
  ( HasTranslation "Car" dict,
    HasTranslation "House" dict
  ) =>
  DictInstance dict ->
  (String, String)
translationDemo dict =
  let x = lookupTranslation @"Car" dict
      y = lookupTranslation @"House" dict
   in (show x, show y)

data MkDict theme where
  EmptyDict :: MkDict '[]
  AddDict ::
    (KnownSymbol (TranslationKey translation), Translated translation) =>
    translation ->
    MkDict dict ->
    MkDict (TranslationKey translation : dict)

instantiateDict :: MkDict theme -> DictInstance theme
instantiateDict EmptyDict = DictInstance Map.empty
instantiateDict (AddDict translation mkDict') =
  let (DictInstance t) = instantiateDict mkDict'
      translationKey = translationKeyVal translation
      translationVal = SomeTranslatable $ toTranslation translation
   in DictInstance $ Map.insert translationKey translationVal t

{-sampleDict :: DictInstance '["Car", "House"]
sampleDict =
  instantiateDict $
    AddDict (translatedTranslation @"Car" @"bil") $
      AddDict (translatedTranslation @"House" @"hus") $
        EmptyDict
        -}
