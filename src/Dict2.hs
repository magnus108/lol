{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Dict2 where

import Data.Kind
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Word
import GHC.TypeLits

type Dict = [Symbol]

class HasTranslation (translation :: Symbol) (container :: Dict)

instance HasTranslation translation (translation ': translations)

instance {-# OVERLAPPABLE #-} (HasTranslation translation translations) => HasTranslation translation (currentTranslation ': translations)

data Translation = Translation {translation :: String}
  deriving (Eq, Show)

newtype DictInstance (dict :: Dict) = DictInstance {getDictInstance :: Map.Map String Translation}
  deriving (Eq, Show)

class ValidateDictInstance (dict :: Dict) (a :: Dict -> Type) where
  validateDictInstance :: Map.Map String Translation -> Maybe (a dict)

instance ValidateDictInstance '[] DictInstance where
  validateDictInstance dict = Just (DictInstance dict)

instance
  ( KnownSymbol currentTranslation,
    ValidateDictInstance rest DictInstance
  ) =>
  ValidateDictInstance (currentTranslation : rest) DictInstance
  where
  validateDictInstance dict = do
    let targetTranslation = symbolVal $ Proxy @currentTranslation
    Map.lookup targetTranslation dict
    (DictInstance m) <- validateDictInstance @rest dict
    pure $ DictInstance m

dictInstance :: ValidateDictInstance dict DictInstance => Map.Map String Translation -> Maybe (DictInstance dict)
dictInstance = validateDictInstance

lookupTranslation ::
  forall translationName dict.
  ( KnownSymbol translationName,
    HasTranslation translationName dict
  ) =>
  DictInstance dict ->
  Translation
lookupTranslation (DictInstance translations) =
  let targetName = symbolVal $ Proxy @translationName
   in translations Map.! targetName

demoDictInstance :: DictInstance ["red", "green", "blue"]
demoDictInstance =
  DictInstance . Map.fromList $
    [ ("red", Translation "Rød"),
      ("green", Translation "Grøn"),
      ("blue", Translation "Blå")
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
