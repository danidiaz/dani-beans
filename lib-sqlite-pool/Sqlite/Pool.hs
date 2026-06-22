{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Allocating connections that might me used down the hierarchy of calls.
module Sqlite.Pool
  ( makeWith,
    runWith,
    SqlitePoolConfig (..),
    SqlitePool,
    make,
  )
where

import Data.Aeson
import Data.Pool.Introspection qualified
import Data.Pool.Introspection.Bean (Pool, PoolConfig, Resource(..))
import Data.Pool.Introspection.Bean qualified
import Data.Text
import GHC.Generics (Generic)
import Sqlite
import Sqlite.Query (execute_)
import ThreadLocal

type SqlitePool = Pool Connection

data SqlitePoolConfig = SqlitePoolConfig
  {databaseFile :: Text}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

make :: SqlitePoolConfig -> PoolConfig -> forall x. (SqlitePool -> IO x) -> IO x
make
  SqlitePoolConfig {databaseFile}
  poolConf =
    do
      Data.Pool.Introspection.Bean.make (Sqlite.openV2NoMutexReadWrite databaseFile) Sqlite.close poolConf

newtype WithConnection =
  MakeWithConnection (forall x. IO x -> IO x)

runWith :: WithConnection -> forall x. IO x -> IO x
runWith (MakeWithConnection r) = r

-- | Takes connection from the pool and sets up transactionality.
makeWith ::
  SqlitePool ->
  ThreadLocal (IO Connection) ->
  WithConnection
makeWith pool threadLocalConnection =
  MakeWithConnection \action -> 
    Data.Pool.Introspection.withResource pool \Resource {resource} -> do
      execute_ resource "BEGIN IMMEDIATE"
      r <- withThreadLocal threadLocalConnection (pure resource) action
      execute_ resource "COMMIT"
      pure r
