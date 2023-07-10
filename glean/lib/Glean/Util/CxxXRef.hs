{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

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
  [ ("xmap", xmap),
    ("externals", JSArray externals),
    ("targets", JSArray _) ] <- keyVals v
  [ ("file", file),
    ("fixed", JSArray fixed),
    ("variable", JSArray variable),
    ("froms", JSArray _) ] <- keyVals xmap
  fixed_xrefs <- forM fixed $ \x -> do
    [ ("target", target),
      ("ranges", JSArray ranges),
      ("from", JSObject _) ] <- objVals x
    reoffset 0 <$> mapM (xref target) ranges
  when (length variable /= length externals) $ fail "invalid xrefs"
  variable_xrefs <- forM (zip externals variable) $ \(target,v) -> do
    JSArray ranges <- return v
    reoffset 0 <$> mapM (xref target) ranges
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
    xref target x = do
      [("offset", JSRational _ o), ("length", JSRational _ n)] <- objVals x
      return $ XRef (floor o) (floor n) target

    toXRef (XRef o n t) = makeObj
      [ ("range", makeObj
          [ ("begin", showJSON o)
          , ("end", showJSON n)
          ]),
        ("target", t)
      ]

    reoffset !_ [] = []
    reoffset !k (XRef o n t : xs) = XRef (o+k) (o+k+n) t : reoffset (o+k) xs

    objVals :: JSValue -> Result [(String, JSValue)]
    objVals (JSObject obj) = return $ fromJSObject obj
    objVals _ = fail "not an object"

    keyVals :: JSValue -> Result [(String, JSValue)]
    keyVals (JSObject obj) = objVals =<< valFromObj "key" obj
    keyVals _ = fail "not an object"
