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
{-# LANGUAGE UndecidableInstances #-}

module Dict2Read where

import Color
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Kind
import qualified Data.Map as M
import Dict2
import Text.Read
import Variadic

newtype TranslationReference r a = TranslationReference {unTranslationReference :: ExceptT String (Reader r) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader r, MonadError String)

type family HKD (wrapper :: Type -> Type) (value :: Type) :: Type where
  HKD Identity value = value
  HKD wrapper value = wrapper value

data TranslationValue w
  = Direct Translation
  | OtherTranslation (HKD w (TranslationValue w))

instance TranslationTypeLevelToTranslation (TranslationValue Identity) where
  toTranslation (Direct translation) = translation
  toTranslation (OtherTranslation ref) = toTranslation ref

newtype DictConfig' w = DictConfig' {getDictConfig :: M.Map String (TranslationValue w)}

type DictConfig = DictConfig' Identity

newtype RawDictConfig = RawDictConfig {getRawDictConfig :: DictConfig' (TranslationReference RawDictConfig)}
  deriving newtype (FromJSON)

instance FromJSON (DictConfig' (TranslationReference RawDictConfig)) where
  parseJSON = fmap DictConfig' . parseJSON

type RuntimeDict = ["blue", "green", "red"]

validateDictConfig ::
  forall (dict :: Dict).
  ValidateDictInstance dict DictInstance =>
  DictConfig ->
  Either String (DictInstance dict)
validateDictConfig = validateDictInstance . M.map SomeTranslation . getDictConfig

evalConfig :: RawDictConfig -> Either String DictConfig
evalConfig rawConfig =
  fmap DictConfig'
    . traverse (dereferenceTranslationValue rawConfig)
    . getDictConfig
    . getRawDictConfig
    $ rawConfig

loadRuntimeDict :: FilePath -> IO DictConfig
loadRuntimeDict p = do
  contents <- BS.readFile p
  case eitherDecode' contents >>= evalConfig of
    Left err -> ioError $ userError err
    Right val -> pure val

testQuery :: FilePath -> IO ()
testQuery p = do
  cfg <- loadRuntimeDict p
  let sampleQuery t = (lookupTranslation @"red" t, lookupTranslation @"blue" t)
      r = sampleQuery <$> validateDictConfig @RuntimeDict cfg
  print r

dereferenceTranslationValue :: RawDictConfig -> TranslationValue (TranslationReference RawDictConfig) -> Either String (TranslationValue Identity)
dereferenceTranslationValue env translationValue =
  case translationValue of
    Direct r -> pure $ Direct r
    OtherTranslation r -> do
      referencedKey <- dereferenceTranslationValue env =<< evalDictReference env r
      case referencedKey of
        OtherTranslation _ -> pure referencedKey
        _ -> pure $ OtherTranslation referencedKey

evalDictReference ::
  RawDictConfig ->
  TranslationReference RawDictConfig (TranslationValue (TranslationReference RawDictConfig)) ->
  Either String (TranslationValue (TranslationReference RawDictConfig))
evalDictReference env =
  flip runReader env . runExceptT . unTranslationReference

instance FromJSON (TranslationValue (TranslationReference RawDictConfig)) where
  parseJSON = withObject "translation" $ \val ->
    parseDirectElement val <|> parseRefElement val
    where
      parseDirectElement val = do
        t <- val .: "direct"
        Direct <$> parseDirect t

      parseRefElement val = do
        refKey <- val .: "same-as"
        pure . OtherTranslation $ generateRef refKey

      generateRef key = do
        translations <- asks (getDictConfig . getRawDictConfig)
        case M.lookup key translations of
          Nothing -> throwError $ "Referenced translation not found: " <> key
          Just translation' -> pure translation'

parseDirect :: MonadFail m => String -> m Translation
parseDirect s = pure $ Translation s
