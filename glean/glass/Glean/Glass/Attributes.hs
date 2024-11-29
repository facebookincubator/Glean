{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Attributes
  ( -- * export known attribute types
    SymbolKind.SymbolKindAttr(..)
    -- * class
  , ToAttributes(..)
  , RefEntitySymbol
  , DefEntitySymbol

    -- * operating with attributes
  , extendAttributes
  , attrListToMap
  , attrMapToList
  ) where

import Glean.Glass.Attributes.Class
import Glean.Glass.Attributes.SymbolKind as SymbolKind
