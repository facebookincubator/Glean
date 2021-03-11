{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Util.Service
  ( -- * Service specification
    Service(Tier, HostPort)
  , serviceToString, serviceTier
    -- ** Aliases
  , HostName, PortNumber
  ) where

import qualified Glean.Service.Types as Service
import Glean.Service.Types hiding (HostPort)

import Data.Int
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text

type HostName = Text
type PortNumber = Int

-- The Thrift-generated types are a bit ugly, so let's make synonyms
pattern Tier :: Text -> Service
pattern Tier x = Service_tier x

pattern HostPort :: Text -> Int32 -> Service
pattern HostPort h p = Service_hostPort (Service.HostPort h p)

-- could move this to be attached to the Thrift file to avoid the orphan
instance IsString Service where
  fromString s
    | (rev_port, ':':rev_host) <- break (==':') $ reverse s
    , [(port,"")] <- reads $ reverse rev_port =
      HostPort (Text.pack (reverse rev_host)) port
    | otherwise = Tier (Text.pack s)

serviceToString :: Service -> String
serviceToString (HostPort h p) = Text.unpack h ++ ":" ++ show p
serviceToString (Tier s) = Text.unpack s
serviceToString _ = error "serviceToString" -- pattern synonyms :(

serviceTier :: Service -> Text
serviceTier HostPort{} = ""
serviceTier (Tier s) = s
serviceTier _ = error "serviceToString" -- pattern synonyms :(
