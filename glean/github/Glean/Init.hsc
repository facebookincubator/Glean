{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP, TemplateHaskell #-}
module Glean.Init (
    withUnitTest,
    withOptions,
    withOptionsGen,
    withUnitTestOptions,
    InitOptions,
    parserInfo,
    setArgs,
    setPrefs,
    setTransformGflags,
  ) where

import Control.Exception
import Foreign
import Foreign.C
import Options.Applicative
import qualified System.Environment as Sys

import Util.OptParse
import Util.Text (withCStrings)
import Util.Control.Exception (tryAll)

import TestRunner

import Mangle.TH

$(mangle
  "void folly::init(int*, char***, bool)"
  [d|
    foreign import ccall unsafe
      c_follyInit
        :: Ptr CInt
        -> Ptr (Ptr (Ptr CChar))
        -> CBool
        -> IO ()
  |])

follyInit :: [String] -> IO ()
follyInit args = do
  name <- Sys.getExecutablePath
  let allArgs = defaultGflags <> args
  withCStrings (name : allArgs) $ \argv ->
    with (fromIntegral (1 + length allArgs)) $ \pargc ->
    with argv $ \pargv ->
      c_follyInit pargc pargv 0

-- Tell glog to log to stderr by default, this can be overridden by
-- command-line flags.
defaultGflags :: [String]
defaultGflags = ["--logtostderr"]

follyUninit :: IO ()
follyUninit = return ()

withUnitTest :: IO () -> IO ()
withUnitTest = id

withOptions :: ParserInfo a -> (a -> IO b) -> IO b
withOptions p f = withOptionsGen (parserInfo p) f

data InitOptions a = InitOptions
  { initArgs :: Maybe [String] -- otherwise use getArgs
  , initPrefs :: PrefsMod
  , initParser :: ParserInfo a
  , initTransformGflags :: a -> [String] -> [String]
  }

parserInfo :: ParserInfo a -> InitOptions a
parserInfo p =
  InitOptions
    { initArgs = Nothing
    , initPrefs = idm
    , initParser = p
    , initTransformGflags = \_ x -> x
    }

type InitSpec a = InitOptions a -> InitOptions a

setPrefs :: PrefsMod -> InitSpec a
setPrefs m opts = opts { initPrefs = m }

setArgs :: [String] -> InitSpec a
setArgs a opts = opts { initArgs = Just a }

setTransformGflags :: (a -> [String] -> [String]) -> InitSpec a
setTransformGflags tr opts = opts { initTransformGflags = tr }

withUnitTestOptions :: ParserInfo a -> (TestAction -> a -> IO b) -> IO b
withUnitTestOptions opts f = withOptions opts $ f TestAction

withOptionsGen :: InitOptions a -> (a -> IO b) -> IO b
withOptionsGen InitOptions{..} act = do
  args <- maybe Sys.getArgs pure initArgs
  r <- tryAll $ partialParse (prefs initPrefs) p' args
  case r of
    Left e -> run (fixHelp args) $ throwIO e
    Right (opts, fbArgs) ->
      run (initTransformGflags opts (fixHelp fbArgs)) $
        act opts
  where
    p' = initParser { infoParser = fbhelper <*> infoParser initParser }
    run args act = bracket_ (follyInit args) follyUninit act
    -- Because the C++ flag help is usually a huge amount of spew, we want to
    -- reserve --help for the Haskell options helper, and provide a --help-all
    -- flag to show the help for the C++ flags.
    fixHelp :: [String] -> [String]
    fixHelp [] = []
    fixHelp ("--help":r) = fixHelp r
    fixHelp ("--help-all":r) = "--help" : fixHelp r
    fixHelp (f:r) = f : fixHelp r

fbhelper :: Parser (a -> a)
fbhelper = abortOption showHelpText $ mconcat
  [ long "help-all"
  , help "Show all possible options."
  , hidden
  ]
  where
#if MIN_VERSION_optparse_applicative(0,16,0)
  showHelpText = ShowHelpText Nothing
#else
  showHelpText = ShowHelpText
#endif
