{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Open-source client-side authentication hook for outbound Glean RPCs.
--
-- 'Glean.Remote' wraps every outbound Glean RPC in 'withGleanCATHeaders'.
-- This open-source build provides a no-op (identity) implementation: no
-- transport headers are attached.
--
-- The Meta-internal build replaces this module an internal authentication
-- mechanism.
module Glean.Auth
  ( withGleanCATHeaders
  ) where

import Thrift.Api (Thrift)
import Glean.GleanService.Client (GleanService)

-- | Attach client-side auth headers to a Glean RPC. No-op in OSS builds.
withGleanCATHeaders :: Thrift GleanService a -> Thrift GleanService a
withGleanCATHeaders = id
