{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Glean.Util.Same
  ( queryDeclFamily
  ) where

import Glean
import Glean.Angle
import qualified Glean.Angle as Angle
import qualified Glean.Schema.Cxx1.Types as Cxx
import Glean.Util.ToAngle

-- | Use 'Cxx.DeclToFamily' to find the same facts (shallow query)
--
-- Always returns non-empty list, in same order as in 'Cxx.DeclFamily' fact
queryDeclFamily :: Cxx.Declaration -> Haxl w [Cxx.Declaration]
queryDeclFamily decl = do
  fams <- search_ $ Angle.query $
    predicate @Cxx.DeclToFamily $
      rec $ field @"decl" (toAngle decl) end
  case fams of
    [] -> return [decl]
    (Cxx.DeclToFamily _ (Just key):_) ->
      getKey (Cxx.declToFamily_key_family key)
    _ -> error ("queryDeclFamily: " ++ show decl)
