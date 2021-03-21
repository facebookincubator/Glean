{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
-- Copyright 2004-present Facebook. All Rights Reserved.

-- | Check comment block removal logic
module TrimLogicTest
  ( main
  ) where

import Util.String.Quasi

import Glean.Init
import Glean.DocBlock.TrimLogic (toLines, fromLines, trimArtAndIndent)

import Data.ByteString.Char8 (ByteString)
import Test.HUnit
import TestRunner

process :: ByteString -> ByteString
process = fromLines . trimArtAndIndent . toLines

data TrimTest = TrimTest { raw, trim :: ByteString }

assertTrim :: String -> TrimTest -> Assertion
assertTrim name tt = assertEqual name (trim tt) (process (raw tt))

trimTest :: ByteString -> ByteString -> Test
trimTest a b =
  let name = show a
  in TestLabel name (TestCase (assertTrim name (TrimTest a b)))

emptys :: Test
emptys = TestLabel "emptys" $ TestList $ map tt
  [ ""
  , "\n"
  , "\n\n"
  , "    \n    \n    "
  , "\n    \n    \n   "
  , "    \n    \n   \n"
  , "\n    \n    \n   \n"
  ]
  where
    tt a =
      let name = "emptys:" ++ show a
      in TestLabel name (TestCase (assertTrim name (TrimTest a "")))

-- | One comment line
slashSlash1 :: Test
slashSlash1 = TestLabel "slashSlash1" $ TestList
  [ trimTest "// Two slashes1 " "Two slashes1"
  , trimTest "  // Two slashes2" "Two slashes2"
  , trimTest "// Two slashes3\n" "Two slashes3"
  , trimTest "  \n \n\n   //    Three slashes4  \n\n  \n " "Three slashes4"
  , trimTest "/// Three slashes1" "Three slashes1"
  , trimTest "  /// Three slashes2" "Three slashes2"
  , trimTest "/// Three slashes3\n" "Three slashes3"
  , trimTest "///Three slashes4" "Three slashes4"
  , trimTest "  \n \n\n   ///    Three slashes4  \n\n  \n " "Three slashes4"
  , trimTest "//! Two! slashes1" "Two! slashes1"
  , trimTest "  //! Two! slashes2" "Two! slashes2"
  , trimTest "//! Two! slashes3\n" "Two! slashes3"
  , trimTest "  \n \n\n   //!    Two! slashes4  \n\n  \n " "Two! slashes4"
  , trimTest "///< Three< slashes1  " "Three< slashes1"
  , trimTest "//!< Two!< slashes1  \n  " "Two!< slashes1"
  -- https://clang.llvm.org/doxygen/classclang_1_1RawComment.html#a9e317c95ddd8bc0629a24180c0d46630
  -- RawComment::isAlmostTrailingComment looks for this probably typo:
  , trimTest "//< Two slashes1" "Two slashes1"
  ]

-- | Multiple comment lines
slashSlash2 :: Test
slashSlash2 = TestLabel "slashSlash2" $ TestList
  [ trimTest [s|
  ///                weird
    //   multi
     //!  line
  |] [s|weird
 multi
line|]
  , trimTest [s|
  ///                weird

    //   multi

     //!  line
  |] [s|weird

 multi

line|]
  ]

-- | Leading and trailing /* */ on same line
slashStar1 :: Test
slashStar1 = TestLabel "slashStar1" $ TestList
  [ trimTest "/* foo bar */" "foo bar"
  , trimTest "   /*   foo bar    */   " "foo bar"
  , trimTest " \n \n\n   /*   foo bar   */  \n\n  \n " "foo bar"
  , trimTest "/** foo bar */" "foo bar"
  , trimTest "/******foo bar */" "foo bar"
  , trimTest "   /**   foo bar    */   " "foo bar"
  , trimTest " \n \n\n   /**   foo bar   */  \n\n  \n " "foo bar"
  , trimTest "/*! foo bar */" "foo bar"
  , trimTest "   /*!   foo bar    */   " "foo bar"
  , trimTest " \n \n\n   /*!   foo bar   */  \n\n  \n " "foo bar"
  , trimTest "/*< foo bar */" "foo bar"
  , trimTest "/**< foo bar */" "foo bar"
  , trimTest "/*!< foo bar */" "foo bar"
  ]

-- | Multi-line /* and */ comments
slashStar2 :: Test
slashStar2 = TestLabel "slashStar2" $ TestList
  -- single * both middle lines have leading start, so remove all of them
  [ trimTest [s|
  /* Hello
   *  World
   * How Are You?
   */
  |] [s|Hello
 World
How Are You?|]
  -- double * both middle lines have leading start, so remove all of them
  , trimTest [s|
  /** Hello
   *  World
   * How Are You?
   */
  |] [s|Hello
 World
How Are You?|]
  -- many leading *, no space after
  , trimTest [s|
  /*****************Hello
   *  World
   * How Are You?
   */
  |] [s|Hello
 World
How Are You?|]
  -- with ! both middle lines have leading start, so remove all of them
  , trimTest [s|
  /*! Hello
   *  World
   * How Are You?
   */
  |] [s|Hello
 World
How Are You?|]
  -- with < both middle lines have leading start, so remove all of them
  , trimTest [s|
  /**< Hello
   *  World
   * How Are You?
   */
  |] [s|Hello
 World
How Are You?|]
  -- with !< both middle lines have leading start, so remove all of them
  , trimTest [s|
  /*!< Hello
   *  World
   * How Are You?
   */
  |] [s|Hello
 World
How Are You?|]
  -- only one middle lines has leading star, so remove none of them
  , trimTest [s|
  /** Hello
   *  World
      How Are You?
   */
  |] [s|Hello
*  World
   How Are You?|]
  -- all middle lines have *, so remove it. Last line has leading star, removed
  , trimTest [s|
  /** Hello
   *  World
   * How Are You?   */
  |] [s|Hello
 World
How Are You?|]
  -- all middle lines have *, so remove it. Last line has no leading star.
  , trimTest [s|
  /** Hello
   *  World
      How Are You?  */
  |] [s|Hello
World
    How Are You?|]
    -- last line has leading star to remove
  , trimTest [s|
  /** Hello
      World
   *  How Are You?  */
  |] [s|Hello
    World
How Are You?|]
  -- https://clang.llvm.org/doxygen/classclang_1_1RawComment.html#a9e317c95ddd8bc0629a24180c0d46630
  -- RawComment::isAlmostTrailingComment looks for this probably typo:
  , trimTest [s|
  /*< Hello
   *  World
   * How Are You?
   */
  |] [s|Hello
 World
How Are You?|]

  ]

main :: IO ()
main = withUnitTest $ do
  testRunner $ TestList
    [ emptys, slashSlash1, slashSlash2, slashStar1, slashStar2  ]
