{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE StrictData #-}

module HieDBIndexer.Types where

import Data.Array.Unboxed (UArray)
import Data.Binary (Binary)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Glean (Nat (..))
import qualified Glean.Schema.Hs.Types as Hs
import qualified Glean.Schema.Src.Types as Src

-- TODO(T101444496): use record syntax
type Vertex = (String, String, String, String, Int, Int, Int, Int)

data Range = Range
  { fp :: FilePath
  , lineBegin :: Int
  , columnBegin :: Int
  , lineEnd :: Int
  , columnEnd :: Int
  }
  deriving (Eq, Show, Ord)

data ByteSpan = ByteSpan
  { start :: Int
  , bsLength :: Int
  }
  deriving (Eq, Show, Ord, Generic)

instance Binary ByteSpan

instance Hashable ByteSpan

data FileLocation = FileLocation
  { fileName :: FilePath
  , locSpan :: ByteSpan
  }
  deriving (Eq, Show, Ord, Generic)

instance Hashable FileLocation

instance Binary FileLocation

data Definition = Definition
  { -- | Fully qualified name?
    qualName :: Text
  , name :: Text
  , symName :: Text
  , modName :: Text
  , loc :: FileLocation
  }
  deriving (Eq, Show, Ord, Generic)

instance Hashable Definition

instance Binary Definition

-- | List of xreferences to the same symbol in a file.
data XReferences = XReferences
  { targetName :: Text
  , spans :: [ByteSpan]
  }
  deriving (Eq, Show, Ord, Generic)

instance Binary XReferences

{- | All the XRefs in a file. XRefs to the same symbol are all grouped in
 TargetInfo.
-}
type FileXRefMap = HashMap FilePath [XReferences]

type LineLengthArray = UArray Int Int

-- Mapping of each file to a list of its line lengths
type FileLineMap = HashMap FilePath LineLengthArray

data NodeDefinition
  = Name Definition
  | Type Definition
  | Constructor Definition
  deriving (Eq, Show, Ord, Generic)

instance Binary NodeDefinition

instance Hashable NodeDefinition

data IndexerBatchOutput = IndexerBatchOutput
  { nodeDefs :: [NodeDefinition]
  , xrefList :: [(FilePath, [XReferences])]
  }
  deriving (Eq, Show, Ord, Generic)

instance Binary IndexerBatchOutput

-- Helpers

mkGleanByteSpan :: ByteSpan -> Src.ByteSpan
mkGleanByteSpan ByteSpan {..} = Src.ByteSpan st len
  where
    st = Nat $ fromIntegral start
    len = Nat $ fromIntegral bsLength

mkGleanXRefTarget :: Hs.DefinitionName -> Hs.XRefTarget
mkGleanXRefTarget = Hs.XRefTarget_definition

mkGleanXReference :: Hs.DefinitionName -> [ByteSpan] -> Hs.XReference
mkGleanXReference defName spans =
  Hs.XReference (mkGleanXRefTarget defName) $ map mkGleanByteSpan spans
