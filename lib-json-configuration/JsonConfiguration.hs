{-# LANGUAGE NoFieldSelectors #-}

module JsonConfiguration
  ( JsonConfiguration (..),
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

data JsonConfiguration = MakeJsonConfiguration
  { lookupSectionWith_ :: forall conf. (Value -> Parser conf) -> Key -> IO conf
  }

lookupSection :: forall conf. (FromJSON conf) => Key -> JsonConfiguration -> IO conf
lookupSection key (MakeJsonConfiguration {lookupSectionWith_}) = lookupSectionWith_ parseJSON key

lookupSectionWith :: forall conf. (Value -> Parser conf) -> Key -> JsonConfiguration -> IO conf
lookupSectionWith parser key (MakeJsonConfiguration {lookupSectionWith_}) = lookupSectionWith_ parser key

data MissingSection = MissingSection Key deriving (Show)

instance Exception MissingSection

data UnparseableSection = UnparseableSection Key String deriving (Show)

instance Exception UnparseableSection
