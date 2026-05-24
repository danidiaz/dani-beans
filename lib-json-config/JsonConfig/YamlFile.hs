{-# LANGUAGE BlockArguments #-}

module JsonConfig.YamlFile
  ( make,
    module Data.Yaml.Config,
  )
where

import JsonConfig
import Control.Exception
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified
import Data.Aeson.Types
import Data.Yaml.Config

make ::
  -- | Usually pass the result of 'Data.Yaml.Config.loadYamlSettings' here.
  IO (KeyMap Value) ->
  IO JsonConfig
make action = do
  keyMap :: KeyMap Value <- action
  pure
    MakeJsonConfig
      { lookupSectionWith_ = \parser sectionKey -> do
          case Data.Aeson.KeyMap.lookup sectionKey keyMap of
            Nothing -> throwIO do MissingSection sectionKey
            Just foo -> case parse parser foo of
              Error message -> throwIO do UnparseableSection sectionKey message
              Success confSection -> pure confSection
      }
