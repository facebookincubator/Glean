{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Glean.Util.CxxXRef
  ( transformXRefs
  ) where

import Control.Monad
import Data.List
import Text.JSON

data XRef = XRef !Int !Int JSValue

order :: XRef -> XRef -> Ordering
order (XRef o1 n1 v1) (XRef o2 n2 v2) =
  compare o1 o2 <> compare n2 n1 <> compare v1 v2

-- | Process a `cxx1.FileXRefs` (as JSON) into a flat list of absolute
-- offsets, which is more useful for inspecting and comparing.  This
-- is used by regression tests and the special "xref" query in the
-- shell.
transformXRefs :: JSValue -> Result JSValue
transformXRefs v = do
  [ ("xmap", xmap), ("targets", JSArray targets) ] <- keyVals v
  [ ("file", file),
    ("fixed", JSArray fixed),
    ("froms", JSArray froms) ] <- keyVals xmap
  fixed_xrefs <- forM fixed $ \x -> do
    [ ("target", target), ("from", from) ] <- objVals x
    spans <- fromToByteSpans from
    return [ XRef o n target | (o, n) <- spans ]
  when (length froms /= length targets) $ fail "invalid xrefs"
  variable_xrefs <- forM (zip targets froms) $ \(t, from) -> do
    JSObject xrefs <- return t
    JSArray ts <- valFromObj "key" xrefs
    spans <- fromToByteSpans from
    return [ XRef o n t | (o, n) <- spans, t <- ts ]
  return $ makeObj
    [ ("key", JSObject $ toJSObject
        [ ("file", file)
        , ("xrefs", JSArray
            $ map toXRef
            $ sortBy order
            $ concat
            $ variable_xrefs ++ fixed_xrefs)
        ])
    ]
  where
    fromToByteSpans from = do
      [ ("spans", JSArray packedSpans),
        ("expansions", JSArray packedExpansions),
        ("spellings", JSArray packedSpellings) ] <- objVals from
      spans <- unpackByteSpans packedSpans
      expansions <- unpackByteSpans packedExpansions
      spellings <- unpackByteSpans packedSpellings
      return $ sort $ spans ++ expansions ++ spellings

    unpackByteSpans packedByteSpans = do
      relByteSpans <- mapM toRelByteSpans packedByteSpans
      return $ reoffset 0 (concat relByteSpans)
      where
      toRelByteSpans x = do
        [("length", JSRational _ n), ("offsets", JSArray offsets)] <- objVals x
        return $ map ((, floor n) . (\(JSRational _ o) -> floor o)) offsets

    toXRef (XRef o n t) = makeObj
      [ ("range", makeObj
          [ ("begin", showJSON o)
          , ("end", showJSON n)
          ]),
        ("target", t)
      ]

    reoffset !_ [] = []
    reoffset !k ((o, n) : xs) = (o+k, o+k+n) : reoffset (o+k) xs

    objVals :: JSValue -> Result [(String, JSValue)]
    objVals (JSObject obj) = return $ fromJSObject obj
    objVals _ = fail "not an object"

    keyVals :: JSValue -> Result [(String, JSValue)]
    keyVals (JSObject obj) = objVals =<< valFromObj "key" obj
    keyVals _ = fail "not an object"
