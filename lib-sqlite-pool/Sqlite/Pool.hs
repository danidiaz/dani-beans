{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Allocating connections that might me used down the hierarchy of calls.
module Sqlite.Pool
  ( withConnection,
    hoistWithConnection,
    withLazilyAllocatedConnection,
    hoistWithLazilyAllocatedConnection,
    SqlitePoolConfig (..),
    SqlitePool,
    makeSqlitePool,
  )
where

import Data.Aeson
import Data.Pool.Introspection qualified
import Data.Pool.Introspection.Bean
import Data.Text
import GHC.Generics (Generic)
import LazyBracket (ExitCase (..), lazyGeneralBracket_)
import LazyBracket qualified
import Sqlite
import Sqlite.Query (execute_)
import ThreadLocal

type SqlitePool = Pool Connection

data SqlitePoolConfig = SqlitePoolConfig
  {databaseFile :: Text}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

makeSqlitePool :: SqlitePoolConfig -> PoolConfig -> forall x. (SqlitePool -> IO x) -> IO x
makeSqlitePool
  SqlitePoolConfig {databaseFile}
  poolConf =
    do
      Data.Pool.Introspection.Bean.make (Sqlite.openV2NoMutexReadWrite databaseFile) Sqlite.close poolConf

-- | Takes connection from the pool and sets up transactionality.
withConnection ::
  SqlitePool ->
  ThreadLocal (IO Connection) ->
  (forall x. IO x -> IO x)
withConnection pool threadLocalConnection action =
  Data.Pool.Introspection.withResource pool \Resource {resource} -> do
    execute_ resource "BEGIN IMMEDIATE"
    r <- withThreadLocal threadLocalConnection (pure resource) action
    execute_ resource "COMMIT"
    pure r

-- | Like 'withConnection', but more in the shape of a decorator.
hoistWithConnection ::
  forall bean.
  ((forall x. IO x -> IO x) -> bean -> bean) ->
  SqlitePool ->
  ThreadLocal (IO Connection) ->
  bean ->
  bean
hoistWithConnection hoistBean pool tlocal =
  hoistBean (withConnection pool tlocal)

withLazilyAllocatedConnection ::
  SqlitePool ->
  ThreadLocal (IO Connection) ->
  (forall x. IO x -> IO x)
withLazilyAllocatedConnection pool threadLocalConnection action =
  lazyGeneralBracket_
    (Data.Pool.Introspection.takeResource pool)
    ( \(Data.Pool.Introspection.Resource {resource}, localPool) -> \case
        ExitCaseSuccess {} -> Data.Pool.Introspection.putResource localPool resource
        ExitCaseException {} -> Data.Pool.Introspection.destroyResource pool localPool resource
        ExitCaseAbort -> Data.Pool.Introspection.destroyResource pool localPool resource
    )
    ( \LazyBracket.Resource {LazyBracket.accessResource, LazyBracket.controlResource} -> do
        controlResource
          ( \(Data.Pool.Introspection.Resource {resource}, _) -> do
              execute_ resource "BEGIN IMMEDIATE"
          )
        r <-
          withThreadLocal
            threadLocalConnection
            ( do
                (Data.Pool.Introspection.Resource {resource}, _) <- accessResource
                pure resource
            )
            action
        controlResource
          ( \(Data.Pool.Introspection.Resource {resource}, _) -> do
              execute_ resource "COMMIT"
          )
        pure r
    )

-- | Like 'withConnection', but more in the shape of a decorator.
hoistWithLazilyAllocatedConnection ::
  forall bean.
  ((forall x. IO x -> IO x) -> bean -> bean) ->
  SqlitePool ->
  ThreadLocal (IO Connection) ->
  bean ->
  bean
hoistWithLazilyAllocatedConnection hoistBean pool tlocal =
  hoistBean (withLazilyAllocatedConnection pool tlocal)
