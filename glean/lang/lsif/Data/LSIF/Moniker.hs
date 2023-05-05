{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-

Processing moniker strings. These are indexer-specific entity encodings,
often with a lot of structure. We can pull out some useful facts if we know
the encoding.

-}

module Data.LSIF.Moniker ( processMoniker ) where

import Data.Text ( Text )
import Data.LSIF.Gen

type Scheme = Text

processMoniker :: MonikerKind -> Scheme -> Text -> Maybe (Text, SymbolKind)
processMoniker _kind scheme ident = case scheme of
  -- lsif-java or scip-java indexer
  "semanticdb" -> processSemanticDB ident
  _ -> Nothing

-- From lsif-java, SemanticDB Symbols, the kind of a symbol is encoded
-- in its final char. https://tinyurl.com/3xwarf74
--
-- In Scip, this is the final 'descriptor' of the symbol
--
processSemanticDB :: Text -> Maybe (Text, SymbolKind)
processSemanticDB "" = Nothing
processSemanticDB ident = case parseSuffix ident of
    (ident', suffix) -> Just (ident', kindFromSuffix suffix)
