{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoFieldSelectors #-}

module Network.Wai.Application
  ( Application (..),
    Middleware (..),
    applyMiddleware,
  )
where

import Data.Monoid (Dual (..), Endo (..))
import Network.Wai qualified

newtype Application = MakeApplication {application :: Network.Wai.Application}

-- | In the 'Semigroup' instance, the left 'Middleware' will be applied /first/ to the 'Application'!
newtype Middleware = MakeMiddleware {middleware :: Network.Wai.Middleware}
  deriving (Semigroup, Monoid) via (Dual (Endo Application))

applyMiddleware :: Middleware -> Application -> Application
applyMiddleware (MakeMiddleware {middleware}) (MakeApplication {application}) =
  MakeApplication {application = middleware application}
