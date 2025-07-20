{-# LANGUAGE NoFieldSelectors #-}

module JsonConf
  ( JsonConf (..),
    lookupSection,
    lookupSectionWith,
    JsonConfMissingSection (..),
    JsonConfUnparseableSection (..),
    Key,
  )
where

import Control.Exception
import Data.Aeson
import Data.Aeson.Types (Parser)

data JsonConf = JsonConf
  { lookupSectionWith_ :: forall conf. (Value -> Parser conf) -> Key -> IO conf
  }

lookupSection :: forall conf. (FromJSON conf) => Key -> JsonConf -> IO conf
lookupSection key (JsonConf {lookupSectionWith_}) = lookupSectionWith_ parseJSON key

lookupSectionWith :: forall conf. (Value -> Parser conf) -> Key -> JsonConf -> IO conf
lookupSectionWith parser key (JsonConf {lookupSectionWith_}) = lookupSectionWith_ parser key

data JsonConfMissingSection = JsonConfMissingSection Key deriving (Show)

instance Exception JsonConfMissingSection

data JsonConfUnparseableSection = JsonConfUnparseableSection Key String deriving (Show)

instance Exception JsonConfUnparseableSection