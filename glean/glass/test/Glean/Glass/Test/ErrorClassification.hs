{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE OverloadedStrings #-}

module Glean.Glass.Test.ErrorClassification (main) where

import Control.Exception
  ( AllocationLimitExceeded(..), SomeException, toException )
import Data.ByteString (ByteString)
import Data.Text (Text)
import Test.HUnit (Test(..), assertEqual)

import Glean.Init (withUnitTest)
import TestRunner (testRunner)
import Thrift.Channel (ChannelException(..))
import Thrift.Protocol.ApplicationException.Types
  ( ApplicationException(..), ApplicationExceptionType(..) )

import Glean.Glass.ErrorClassification (srErrorName)
import Glean.Glass.Types
  ( GlassException(..), GlassExceptionReason(..), Revision(..) )

main :: IO ()
main = withUnitTest $ testRunner $ TestList unitTests

unitTests :: [Test]
unitTests =
  [ TestLabel label $ TestCase $ assertEqual label expected (srErrorName e)
  | (label, e, expected) <- classifications
  ]

-- | The downstream messages are shaped after real @error_reason_what@ values
-- from the @service_router@ dataset for @service = 'glean.glass'@.
classifications :: [(String, SomeException, Maybe ByteString)]
classifications =
  [ ( "per-client Glean throttle"
    , downstream
        "(THROTTLING_CLIENT_ID_REQUEST) Throttling request to tier \
        \'glean.query.prod' with client_id \
        \'glean.glass~useGleanGlassNavigationForDiffTool:phabricator' \
        \because global request rate is higher than max configured 100"
    , Just "GleanClientThrottled"
    )
  , ( "Glean error-rate throttle"
    , downstream
        "(THROTTLING_ERROR) This request to service 'glean.query.prod' was \
        \throttled because the error rate exceeded the configured threshold"
    , Just "GleanErrorRateShed"
    )
  , ( "Glean queue timeout, with the trailing help text"
    , downstream
        "(APP_QUEUE_TIMEOUT) Server Queue Timeout -- For help debugging \
        \visit https://fburl.com/wiki/sr_app_queue_timeout"
    , Just "GleanQueueTimeout"
    )
  , ( "Glean host overload"
    , downstream
        "(HOST_OVERLOAD) Host is overloaded -- For help debugging visit \
        \https://fburl.com/wiki/sr_host_overload"
    , Just "GleanOverload"
    )
  , ( "Glean app overload shares the overload name"
    , downstream
        "(APP_OVERLOAD) Application is overloaded -- For help debugging \
        \visit https://fburl.com/wiki/sr_app_overload"
    , Just "GleanOverload"
    )
  , ( "unclassified reason falls through to ServiceRouter's default"
    , downstream
        "(RECV_TIMEOUT) Receive timeout after 30000ms -- For help debugging \
        \visit https://fburl.com/wiki/sr_recv_timeout"
    , Nothing
    )
  , ( "channel failure that is not a ServiceRouter exception"
    , toException (ChannelException "recvCob: connection closed")
    , Nothing
    )
  , ( "GHC allocation limit"
    , toException AllocationLimitExceeded
    , Just "GlassAllocationLimit"
    )
  , ( "Glass's own request timeout"
    , toException $ ApplicationException
        "glass server timeout" ApplicationExceptionType_Timeout
    , Just "GlassTimeout"
    )
  , ( "other ApplicationExceptions are not Glass timeouts"
    , toException $ ApplicationException
        "something else" ApplicationExceptionType_InternalError
    , Nothing
    )
  , ( "declared exceptions keep the classification hsthrift gives them"
    , toException $ GlassException
        { glassException_reasons =
            [GlassExceptionReason_exactRevisionNotAvailable "deadbeef"]
        , glassException_revisions = [Revision "deadbeef"]
        }
    , Nothing
    )
  ]

downstream :: Text -> SomeException
downstream what = toException $ ChannelException $
  "recvCob: facebook::servicerouter::TServiceRouterException: " <> what
