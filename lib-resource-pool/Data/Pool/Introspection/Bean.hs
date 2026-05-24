{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumDecimals #-}
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
    unusedResourceTtlSeconds :: Double
  }
  deriving stock (Generic)

instance FromJSON PoolConfig where
  parseJSON = withObject "PoolConfig" \o -> do
    poolSize <- o .: fromString "pool_size"
    unusedResourceTtlSeconds <- o .: fromString "unused_resource_ttl_seconds"
    pure MakePoolConfig {poolSize, unusedResourceTtlSeconds}

instance ToJSON PoolConfig where
  toJSON MakePoolConfig {poolSize, unusedResourceTtlSeconds} =
    object
      [ fromString "pool_size" .= poolSize,
        fromString "unused_resource_ttl_seconds" .= unusedResourceTtlSeconds
      ]

make :: forall r. IO r -> (r -> IO ()) -> PoolConfig -> forall x. (Pool.Pool r -> IO x) -> IO x
make alloc dealloc MakePoolConfig {unusedResourceTtlSeconds, poolSize} continuation = do
  let poolConfig = Pool.defaultPoolConfig alloc dealloc unusedResourceTtlSeconds poolSize
  bracket (Pool.newPool poolConfig) Pool.destroyAllResources continuation
