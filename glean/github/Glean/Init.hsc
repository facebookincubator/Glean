-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP, TemplateHaskell #-}
module Glean.Init (
    withUnitTest,
    withGflags,
    withOptions,
  ) where

import Control.Exception
import Foreign
import Foreign.C
import Options.Applicative
import qualified System.Environment as Sys

import Util.Encoding (setDefaultEncodingToUTF8)
import Util.OptParse
import Util.Text (withCStrings)
import Util.Control.Exception (tryAll)

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

withGflags :: [String] -> IO a -> IO a
withGflags args = bracket_ (follyInit args) follyUninit

withOptions :: ParserInfo a -> (a -> IO b) -> IO b
withOptions = withParser Sys.getArgs

withParser :: IO [String] -> ParserInfo a -> (a -> IO b) -> IO b
withParser argsIO p act = do
  args <- argsIO
  r <- tryAll $ partialParse defaultPrefs p' args
  case r of
    Left e -> run (fixHelp args) $ throwIO e
    Right (opts, fbArgs) -> run (fixHelp fbArgs) $ act opts
  where
    p' = p { infoParser = fbhelper <*> infoParser p }
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
