{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Display (
    displayDefault,
    displayVerbose,
    Display(..),
    DisplayOpts(..),
    PredicateStyle(..),
    defaultDisplayOpts,
    verboseDisplayOpts,
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc

data PredicateStyle = PredicateWithHash | PredicateWithoutHash

newtype DisplayOpts = DisplayOpts
  { predicateStyle :: PredicateStyle
  -- more later
  }

-- | A "human readable" style, for things like error messages
defaultDisplayOpts :: DisplayOpts
defaultDisplayOpts = DisplayOpts
  { predicateStyle = PredicateWithoutHash
  }

-- | All the details, for debugging
verboseDisplayOpts :: DisplayOpts
verboseDisplayOpts = DisplayOpts
  { predicateStyle = PredicateWithHash
  }

displayDefault :: Display a => a -> Doc ann
displayDefault = display defaultDisplayOpts

displayVerbose :: Display a => a -> Doc ann
displayVerbose = display verboseDisplayOpts

-- | A class for paramterising pretty-printers with some options to
-- control what gets printed, for example to select different levels
-- of verbosity.
class Display a where
  display :: DisplayOpts -> a -> Doc ann

  -- | If necessary, parenthesise the output to make it suitable for
  -- use in an "atomic" position such as a function argument.
  displayAtom :: DisplayOpts -> a -> Doc ann
  displayAtom opts = display opts

instance Display () where
  display _ = pretty

instance Display String where
  display _ = pretty

instance Display Text where
  display _ = pretty
