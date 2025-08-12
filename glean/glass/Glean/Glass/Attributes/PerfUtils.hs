{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Attributes.PerfUtils (
    parseGitDiff,
    applyLineMappingToDefs,
    applyLineMappingToRefs
  ) where

import Data.Text (Text)

type Mapping = ()

parseGitDiff :: Maybe Text -> Mapping
parseGitDiff = const ()

applyLineMappingToDefs :: Mapping -> a -> a
applyLineMappingToDefs _ = id

applyLineMappingToRefs :: Mapping -> a -> a
applyLineMappingToRefs _ = id
