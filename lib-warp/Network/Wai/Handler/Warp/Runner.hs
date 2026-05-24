{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Network.Wai.Handler.Warp.Runner
  ( RunnerConfig (..),
    makeSettings,
    Settings,
    Runner (..),
    make,
    decorate,
    run,
  )
where

import Data.Aeson
import Data.String (fromString)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Network.Wai.Application
import Network.Wai.Handler.Warp (Settings, defaultSettings, 
          setPort, 
          setHost,
          HostPreference)
import Network.Wai.Handler.Warp qualified
import Data.Function ((&))

data RunnerConfig = MakeRunnerConfig
  { port :: Maybe Int,
    host :: Maybe HostPreference
  }
  deriving stock (Generic)

instance FromJSON RunnerConfig where
  parseJSON = withObject "RunnerConfig" \o -> do
    port <- o .:? fromString "port"
    hostText <- o .:? fromString "host"
    pure MakeRunnerConfig
      { port,
        host = fromString . Text.unpack <$> hostText
      }

instance ToJSON RunnerConfig where
  toJSON MakeRunnerConfig {port, host} =
    object
      [ fromString "port" .= port,
        fromString "host" .= (Text.pack . show <$> host)
      ]

makeSettings :: RunnerConfig -> Settings
makeSettings MakeRunnerConfig { port, host } = 
  defaultSettings 
  & maybe id setPort port
  & maybe id setHost host

newtype Runner = MakeRunner {_run :: IO ()}

make ::
  Settings ->
  Application ->
  Runner
make
  settings
  MakeApplication {application} = MakeRunner {_run}
    where
      _run = Network.Wai.Handler.Warp.runSettings settings application

run :: Runner -> IO ()
run MakeRunner {_run} = _run

-- | Likely useless, better decorate the 'Application' instead.
decorate :: (forall x. IO x -> IO x) -> Runner -> Runner
decorate f MakeRunner {_run} = MakeRunner {_run = f _run}
