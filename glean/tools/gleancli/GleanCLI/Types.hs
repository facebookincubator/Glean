module GleanCLI.Types
  ( Plugin(..)
  ) where

import Options.Applicative
import qualified Glean.LocalOrRemote as Glean

class Plugin c where
  parseCommand :: Parser c
  runCommand :: Glean.LocalOrRemote b => b -> c -> IO ()
