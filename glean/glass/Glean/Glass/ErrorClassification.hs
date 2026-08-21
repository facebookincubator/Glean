{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE OverloadedStrings #-}

module Glean.Glass.ErrorClassification
  ( srErrorName
  ) where

import Control.Exception
  ( AllocationLimitExceeded(..), SomeException, fromException )
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text

import Thrift.Channel (ChannelException(..))
import Thrift.Protocol.ApplicationException.Types
  ( ApplicationException(..), ApplicationExceptionType(..) )

-- | The @uex@ name ServiceRouter should classify this failure under, or
-- 'Nothing' to leave its default undeclared-exception handling (fatal plus
-- host markdown) in place.
--
-- A name only has an effect if glean.glass's routing config gives it an
-- @appErrorsMap@ entry; without one ServiceRouter falls back to that same
-- default, so adding a name here is never worse than not classifying at all.
srErrorName :: SomeException -> Maybe ByteString
srErrorName e
  | Just (ChannelException msg) <- fromException e =
      flip lookup downstreamErrorNames =<< serviceRouterErrorReason msg
  | Just AllocationLimitExceeded <- fromException e =
      Just "GlassAllocationLimit"
  | Just ApplicationException
      { applicationException_type = ApplicationExceptionType_Timeout }
      <- fromException e =
      Just "GlassTimeout"
  | otherwise = Nothing

-- | The @ErrorReason@ enum name that ServiceRouter reported a downstream failure
-- under, e.g. APP_QUEUE_TIMEOUT, THROTTLING_CLIENT_ID_REQUEST, etc.
-- hsthrift's cpp-channel flattens the whole @TServiceRouterException@
-- to its @what()@ string, so this is the only way to recover it. Match the
-- enum name only: @TServiceRouterException::getExceptionMsg@ formats it as
-- @(REASON) @, while the message that follows is prose, not contract.
serviceRouterErrorReason :: Text -> Maybe Text
serviceRouterErrorReason msg
  | Text.null afterReason = Nothing
  | otherwise = Just reason
  where
    (_, atMarker) = Text.breakOn marker msg
    (reason, afterReason) =
      Text.breakOn ")" (Text.drop (Text.length marker) atMarker)
    marker = "TServiceRouterException: ("

-- | ServiceRouter @ErrorReason@ names (@servicerouter/common/error.thrift@)
-- that Glass reports under a dedicated @uex@ name. Anything absent keeps
-- ServiceRouter's undeclared-exception default.
downstreamErrorNames :: [(Text, ByteString)]
downstreamErrorNames =
  [ ("THROTTLING_CLIENT_ID_REQUEST", "GleanClientThrottled")
  , ("THROTTLING_ERROR", "GleanErrorRateShed")
  , ("APP_QUEUE_TIMEOUT", "GleanQueueTimeout")
  , ("HOST_OVERLOAD", "GleanOverload")
  , ("APP_OVERLOAD", "GleanOverload")
  ]
