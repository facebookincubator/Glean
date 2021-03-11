{-# LANGUAGE CPP #-}
module Glean.Init (module Glean.Init) where

import Control.Exception
import Options.Applicative
import qualified System.Environment as Sys
import Util.Encoding (setDefaultEncodingToUTF8)
import Util.OptParse
import Util.Text (withCStrings)
import Util.Control.Exception (tryAll)

withUnitTest :: IO () -> IO ()
withUnitTest = id

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
    run _args = id
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
