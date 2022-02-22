{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.RepoMapping where

import qualified Data.Map.Strict as Map

import Glean.Glass.Base ( GleanDBAttrName, GleanDBName(..) )
import Glean.Glass.Types ( Language(..), RepoName(..) )

-- example: the open source react repo.
gleanIndices :: Map.Map RepoName [(GleanDBName, Language)]
gleanIndices = Map.fromList
    [ (RepoName "react",
        [ ("react", Language_JavaScript) ])
    ]

-- repos that contain symbol attributes
gleanAttrIndices :: Map.Map GleanDBName [GleanDBAttrName]
gleanAttrIndices = Map.empty
