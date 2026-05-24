{-# LANGUAGE NoFieldSelectors #-}

module JsonConfig
  ( JsonConfig (..),
    lookupSection,
    lookupSectionWith,
    MissingSection (..),
    UnparseableSection (..),
    Key,
  )
where

import Control.Exception
import Data.Aeson
import Data.Aeson.Types (Parser)

data JsonConfig = MakeJsonConfig
  { lookupSectionWith_ :: forall conf. (Value -> Parser conf) -> Key -> IO conf
  }

lookupSection :: forall conf. (FromJSON conf) => Key -> JsonConfig -> IO conf
lookupSection key (MakeJsonConfig {lookupSectionWith_}) = lookupSectionWith_ parseJSON key

lookupSectionWith :: forall conf. (Value -> Parser conf) -> Key -> JsonConfig -> IO conf
lookupSectionWith parser key (MakeJsonConfig {lookupSectionWith_}) = lookupSectionWith_ parser key

data MissingSection = MissingSection Key deriving (Show)

instance Exception MissingSection

data UnparseableSection = UnparseableSection Key String deriving (Show)

instance Exception UnparseableSection
