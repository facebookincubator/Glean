{-# LANGUAGE NamedFieldPuns #-}
-- | Tiny demonstration of calling the SyntaxHighlighter service (Pygments)
--
-- Code and options taken from Hack in
--
-- > flib/intern/remarkup/syntax/highlighter/FBPygmentsSyntaxHighlighter.php
--
-- Build with
--
-- > buck build  @mode/opt //glean/demo:call-pygments
--
-- Example command
--
-- > buck-out/opt/gen/glean/demo/call-pygments --service syntaxhighlighter
-- >     --format cpp --file glean/rts/id.h
module CallPygments (main) where

import qualified
  SyntaxHighlighter.Syntaxhighlighter.SyntaxHighlighterService.Client
    as SH -- generated
import qualified SyntaxHighlighter.Syntaxhighlighter.Types as SH  -- generated

import Glean.Init (withOptions)
import Glean.Util.ThriftService
import Glean.Impl.ThriftService

import Thrift.Api

import Control.Monad.Trans
import Data.Default
import Data.Map as Map (Map)
import qualified Data.Map as Map
import Data.String (fromString)
import Data.Text as Text (Text, pack)
import qualified Data.Text.IO as Text
import qualified Options.Applicative as O

data SynHighQuery = SH_Highlight
    { shFormat :: String
    , shFile :: Maybe FilePath }
  deriving Show

data Config = Config
    { cfgService :: ThriftService SH.SyntaxHighlighterService
    , cfgQuery :: SynHighQuery
    }

options :: O.ParserInfo Config
options = O.info (O.helper <*> parser) O.fullDesc
  where
    mkTS s = mkThriftService s def { processingTimeout = Just 10.0 }
    parser = Config
      <$> fmap mkTS (O.option (fromString <$> O.str)
            (O.long "service" <> O.metavar "<TIER or HOST:PORT>"))
      <*> sh_highlight
    sh_highlight = SH_Highlight
      <$> O.strOption (O.long "format" <> O.metavar "<pygments format name>")
      <*> O.optional (O.strOption
        (O.long "file" <> O.metavar "<local path to file>"))

runThrift' :: Config -> Thrift SH.SyntaxHighlighterService a -> IO a
runThrift' c act = runThriftInefficiently (cfgService c) act

main :: IO ()
main = do
 putStrLn "\n\nRunning"
 withOptions options $ \cfg -> do
  putStrLn "\n\ncase cfgQuery config"
  case cfgQuery cfg of
    SH_Highlight{ shFormat, shFile } -> do
      let format = Text.pack shFormat
          lex_options :: Map Text Text
          lex_options = Map.fromList [("stripnl", "false")]
          format_options :: Map Text Text
          format_options = Map.fromList [("nowrap", "true")]
      contents <- maybe Text.getContents Text.readFile shFile
      liftIO $ putStrLn "\n\ncalling runThrift"
      hr <- runThrift' cfg $ do
        liftIO $ putStrLn ("\n\ncalling SH.highlight "
          ++ show (format, "<contents>" :: String, lex_options, format_options))
        SH.highlight format contents lex_options format_options
      putStrLn "\n\nhighlightResult_result : "
      Text.putStrLn (SH.highlightResult_result hr)
      putStrLn "\n\nhighlightResult_css : "
      Text.putStrLn (SH.highlightResult_css hr)
      let n = length (SH.highlightResult_tokens hr)
      putStrLn ("\n\nhighlightResult_tokens : " <> show n)
      mapM_ print (SH.highlightResult_tokens hr)
