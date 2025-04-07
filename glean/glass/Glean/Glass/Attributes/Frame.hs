{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- Copyright 2004-present Facebook. All Rights Reserved.

module Glean.Glass.Attributes.Frame
  ( {- instances -}
    FrameAttr(..)

  ) where

--
-- Strobelight frames for entities
--


import qualified Data.Map as Map


import Glean.Glass.Attributes.Class
import Glean.Glass.Types as Glass
    ( Attribute(Attribute_aString), Attributes(Attributes) )


import qualified Glean.Schema.Code.Types as Code

import Data.Text ( Text )

data FrameAttr = FrameAttr

type EntityFrameLabel = (Code.Entity, Text)

instance ToAttributes FrameAttr where
  type AttrRep FrameAttr = EntityFrameLabel
  type AttrLog FrameAttr = ()
  type FileAttrRep FrameAttr = ()

  fileAttrsToAttributeList _ _ = undefined


  queryForFile _ _ _ _ = undefined

  augmentSymbols _ frames refs defs _ =
    (refs_result, defs_result, ())
     where
     (refs_result, defs_result) =
      extendAttributes (\_ ent -> ent) attrMap refs defs
      where
      attrMap = Map.fromList
        [ (entity, toAttributes frame)
        | (entity, frame) <- frames
        ]

      toAttributes frame =
        Attributes $ Map.singleton "frame" $ Attribute_aString frame


  queryMetadataForFile _ _ _ _ = pure []
