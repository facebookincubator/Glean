{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Angle.RecExpansionTest (main) where

import Test.HUnit

import TestRunner
import Util.String.Quasi

import Glean.Init
import Glean.Query.Thrift as Thrift
import qualified Glean.Schema.GleanTest.Types as Glean.Test

import TestDB

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "recExpansion" $ angleRecExpansion id
  , TestLabel "recExpansion/page" $ angleRecExpansion (limit 1)
  ]

angleRecExpansion :: (forall a . Query a -> Query a) -> Test
angleRecExpansion modify = dbTestCase $ \env repo -> do
  results <- runQuery_ env repo $ modify $ recursive $
    angle @Glean.Test.TreeToTree
    [s|
      glean.test.TreeToTree { node = { label = "a" } }
    |]
  assertBool "recursive expand key" $ case results of
    [ Glean.Test.TreeToTree
      { treeToTree_key =
          Just Glean.Test.Tree
            { tree_key =
              Just Glean.Test.Tree_key
                { tree_key_node = Glean.Test.Node {
                    node_key = Just (Glean.Test.Node_key "a")
                  }
                , tree_key_left =
                    Just Glean.Test.Tree
                      { tree_key =
                          Just Glean.Test.Tree_key
                            { tree_key_node = Glean.Test.Node {
                                node_key = Just (Glean.Test.Node_key "b")
                              }
                            , tree_key_right =
                                Just Glean.Test.Tree
                                  { tree_key =
                                      Just Glean.Test.Tree_key
                                        { tree_key_node = Glean.Test.Node {
                                            node_key = Just (Glean.Test.Node_key "d")
                                        }}
                                  }
                            }
                      }
                }
            }
      , treeToTree_value =
          Just Glean.Test.Tree
            { tree_key =
              Just Glean.Test.Tree_key
                { tree_key_node = Glean.Test.Node {
                    node_key = Just (Glean.Test.Node_key "e")
                  }
                , tree_key_left =
                    Just Glean.Test.Tree
                      { tree_key =
                          Just Glean.Test.Tree_key
                            { tree_key_node = Glean.Test.Node {
                                node_key = Just (Glean.Test.Node_key "f")
                            }
                            , tree_key_right =
                                Just Glean.Test.Tree
                                  { tree_key =
                                      Just Glean.Test.Tree_key
                                        { tree_key_node = Glean.Test.Node {
                                            node_key = Just (Glean.Test.Node_key "g")
                                        }}
                                  }
                            }
                      }
                }
            }
      }] -> True
    _ -> False

  results <- runQuery_ env repo $ modify $ recursive $
    angle @Glean.Test.FooToFoo
    [s|
      glean.test.FooToFoo "foo1"
    |]
  assertBool "recursive expand value" $ case results of
    [ Glean.Test.FooToFoo
        { fooToFoo_key =
            Just Glean.Test.Foo
              { foo_key = Just "foo1"
              , foo_value =
                  Just Glean.Test.Bar
                    { bar_key = Just "bar1"
                    , bar_value =
                      Just Glean.Test.Qux  { qux_key = Just "qux1"}
                    }
              }
        , fooToFoo_value =
            Just Glean.Test.Foo
              { foo_key = Just "foo2"
              , foo_value =
                  Just Glean.Test.Bar
                    { bar_key = Just  "bar2"
                    , bar_value =
                      Just Glean.Test.Qux  { qux_key = Just "qux2"}
                    }
              }
        }] -> True
    _ -> False
