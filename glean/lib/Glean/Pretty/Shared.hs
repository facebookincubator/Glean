{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


-- | Helper combinator for writing pretty printers
module Glean.Pretty.Shared
  ( txt, (</>), (<//>), prepend, before, after
  ) where

import Data.Text.Prettyprint.Doc
import Data.Text (Text)

import Glean.Util.AnnMaker (Ann, anStyle)

import Glean.Pretty.Style (Style)

{-# INLINE txt #-}
-- | This helper should be the only use of 'pretty' in this module
txt :: Style -> Text -> Doc (Ann r)
txt style p = anStyle style (pretty p)

(</>) :: Doc ann -> Doc ann -> Doc ann
(</>) a b = a <> softline <> b  -- space or newline

(<//>) :: Doc ann -> Doc ann -> Doc ann
(<//>) a b = a <> softline' <> b  -- nothing or newline

prepend :: Maybe (Doc ann) -> [Doc ann] -> [Doc ann]
prepend = maybe id (:)

before :: Maybe (Doc ann) -> Doc ann -> Doc ann
before = maybe id (</>)

after :: Maybe (Doc ann) -> Doc ann -> Doc ann
after = maybe id (flip (</>))
