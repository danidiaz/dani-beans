{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.Pool.Introspection.Bean
  ( PoolConfig (..),
    Pool.Pool,
    Pool.Resource (..),
    make,
  )
where

import Control.Exception
import Data.Aeson
import Data.Pool.Introspection qualified as Pool
import Data.String (IsString (..))
import Data.Text
import GHC.Generics (Generic)

-- | Different from 'Pool.PoolConfig'. This is intended to be deserialized from
-- the configuration file.
data PoolConfig = MakePoolConfig
  { poolSize :: Int,
    unusedResourceTtlSeconds :: Double,
    numStripes :: Maybe Int,
    poolLabel :: Maybe Text
  }
  deriving stock (Generic)

data PoolConfigJsonKeys = MakePoolConfigJsonKeys
  { poolSize :: Key,
    unusedResourceTtlSeconds :: Key,
    numStripes :: Key,
    poolLabel :: Key
  }

poolConfigJsonKeys :: PoolConfigJsonKeys
poolConfigJsonKeys =
  MakePoolConfigJsonKeys
    { poolSize = fromString "pool_size",
      unusedResourceTtlSeconds = fromString "unused_resource_ttl_seconds",
      numStripes = fromString "num_stripes",
      poolLabel = fromString "pool_label"
    }

instance FromJSON PoolConfig where
  parseJSON = withObject "PoolConfig" \o -> do
    poolSize <- o .: keys.poolSize
    unusedResourceTtlSeconds <- o .: keys.unusedResourceTtlSeconds
    numStripes <- o .:? keys.numStripes
    poolLabel <- o .:? keys.poolLabel
    pure MakePoolConfig {poolSize, unusedResourceTtlSeconds, numStripes, poolLabel}
    where
      keys = poolConfigJsonKeys

instance ToJSON PoolConfig where
  toJSON MakePoolConfig {poolSize, unusedResourceTtlSeconds, numStripes, poolLabel} =
    object
      [ keys.poolSize .= poolSize,
        keys.unusedResourceTtlSeconds .= unusedResourceTtlSeconds,
        keys.numStripes .= numStripes,
        keys.poolLabel .= poolLabel
      ]
    where
      keys = poolConfigJsonKeys

make :: forall r. IO r -> (r -> IO ()) -> PoolConfig -> forall x. (Pool.Pool r -> IO x) -> IO x
make alloc dealloc MakePoolConfig {unusedResourceTtlSeconds, poolSize, numStripes, poolLabel} continuation = do
  let poolConfig =
        maybe id Pool.setPoolLabel poolLabel $
          maybe id (Pool.setNumStripes . Just) numStripes $
            Pool.defaultPoolConfig alloc dealloc unusedResourceTtlSeconds poolSize
  bracket (Pool.newPool poolConfig) Pool.destroyAllResources continuation
