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
import ColorX11
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

newtype TranslatedValue = TranslatedValue Translation
  deriving newtype (FromJSON)

unTranslatedValue :: TranslatedValue -> Translation
unTranslatedValue (TranslatedValue x) = x

newtype TranslationConfig = TranslationConfig {getTranslationConfig :: M.Map String TranslatedValue}
  deriving newtype (FromJSON)

type RuntimeDict = ["red", "green", "blue"]

validateDictConfig ::
  forall (dict :: Dict).
  ValidateDictInstance dict DictInstance =>
  TranslationConfig ->
  Either String (DictInstance dict)
validateDictConfig = validateDictInstance . M.map unTranslatedValue . getTranslationConfig

evalConfig :: TranslationConfig -> Either String TranslationConfig
evalConfig rawConfig = Right rawConfig

loadRuntimeDict :: FilePath -> IO TranslationConfig
loadRuntimeDict p = do
  contents <- BS.readFile p
  case eitherDecode' contents >>= evalConfig of
    Left err -> ioError $ userError err
    Right val -> pure val

testQuery :: FilePath -> IO ()
testQuery p = do
  cfg <- loadRuntimeDict p
  let sampleQuery t = (lookupTranslation @"red" t, lookupTranslation @"blue" t, lookupTranslation @"green" t)
      r = sampleQuery <$> validateDictConfig @RuntimeDict cfg
  print r
