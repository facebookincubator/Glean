-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TypeApplications #-}

module Glean.Util.ShellPrint
  ( ShellFormat(..)
  , ShellPrintFormat(..)
  , PrintOpts(..)
  , getTerminalWidth
  , hPutShellPrintLn
  , putShellPrintLn
  , shellFormatOpt
  , shellPrint
  ) where

import Prelude hiding ((<>))

import Control.Monad (join)
import Data.Aeson
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Data.HashMap.Strict as HashMap
import Data.Time.Clock.POSIX
import Data.List hiding (span)
import Data.Void
import Options.Applicative as O
import Data.Text.Prettyprint.Doc
import System.Exit
import System.IO
import System.Process
import System.Timeout
import Prettyprinter.Render.Terminal
import Util.Control.Exception (catchAll)
import Util.TimeSec


import qualified Glean.Types as Thrift
import Glean.Backend.Remote (dbShard)
import Glean.Repo.Text (showRepo)

data ShellPrintFormat
  = TTY
  | PlainText
  | Json
  deriving (Eq,Show)

data Context = Context
  { ctxVerbose :: Bool
  , ctxFormat :: ShellPrintFormat
  , ctxNow :: Time
  }

parseFormat :: String -> Maybe ShellPrintFormat
parseFormat "tty" = Just TTY
parseFormat "plain" = Just PlainText
parseFormat "json" = Just Json
parseFormat _ = Nothing

shellFormatOpt :: Parser (Maybe ShellPrintFormat)
shellFormatOpt =
  O.optional $ O.option (maybeReader parseFormat) $ mconcat
    [ O.long "format"
    , O.metavar "(tty|plain|json)"
    , O.help "Output format"
    ]

data PrintOpts = PrintOpts
  { poVerbose :: Bool
  , poFormat :: ShellPrintFormat
  , poNow :: Time
  , poWidth :: Maybe PageWidth
  }

putShellPrintLn :: ShellFormat a => Maybe ShellPrintFormat -> a -> IO ()
putShellPrintLn = hPutShellPrintLn stdout

hPutShellPrintLn
  :: ShellFormat a
  => Handle
  -> Maybe ShellPrintFormat
  -> a
  -> IO ()
hPutShellPrintLn outh opt x = do
  tty <- hIsTerminalDevice stdout
  now <- utcTimeToPOSIXSeconds <$> getCurrentTime
  width <- fromMaybe 80 <$> getTerminalWidth
  let
    t0 = Time (round now)
    format = fromMaybe (if tty then TTY else PlainText) opt
    opts = PrintOpts
      { poVerbose = False
      , poFormat = format
      , poNow = t0
      , poWidth = Just $ AvailablePerLine width 1
      }
  shellPrint outh opts x

shellPrint :: ShellFormat a => Handle -> PrintOpts -> a -> IO ()
shellPrint handle PrintOpts{..} x =
  Pretty.renderIO handle sds
  where
    sds =
      layoutPretty layout $ doc <> hardline
    doc = case poFormat of
      Json ->
        pretty $
        BS.unpack $
        J.encodePretty $
        shellFormatJson context x
      TTY -> shellFormatText context x
      PlainText -> unAnnotate $ shellFormatText context x
    context = Context
      { ctxFormat = poFormat
      , ctxNow = poNow
      , ctxVerbose = poVerbose
      }
    layout = LayoutOptions
      { layoutPageWidth = fromMaybe Unbounded poWidth
      }

type Ann = AnsiStyle

class ShellFormat a where
    shellFormatText :: Context -> a -> Doc Ann
    shellFormatJson :: Context -> a -> Value

instance ShellFormat Void where
  shellFormatText _ctx v = case v of
  shellFormatJson _ctx v = case v of

instance ShellFormat String where
  shellFormatText _ctx s = pretty s
  shellFormatJson _ctx s = J.toJSON s

instance ShellFormat Thrift.DatabaseStatus where
  shellFormatText _ctx status =
    case status of
      Thrift.DatabaseStatus_Complete -> parens "complete"
      Thrift.DatabaseStatus_Finalizing -> parens "finalizing"
      Thrift.DatabaseStatus_Incomplete -> parens "incomplete"
      Thrift.DatabaseStatus_Restoring -> parens "restoring"
      Thrift.DatabaseStatus_Broken -> parens "broken"
      Thrift.DatabaseStatus_Restorable -> parens "restorable"
      Thrift.DatabaseStatus_Missing -> parens "missing deps"
  shellFormatJson _ctx status = J.toJSON @String $
      case status of
        Thrift.DatabaseStatus_Complete -> "COMPLETE"
        Thrift.DatabaseStatus_Finalizing -> "FINALIZING"
        Thrift.DatabaseStatus_Incomplete -> "INCOMPLETE"
        Thrift.DatabaseStatus_Restoring -> "RESTORING"
        Thrift.DatabaseStatus_Broken -> "BROKEN"
        Thrift.DatabaseStatus_Restorable -> "RESTORABLE"
        Thrift.DatabaseStatus_Missing -> "MISSING"

instance ShellFormat Thrift.Repo where
  shellFormatText _ctx repo = pretty (showRepo repo)
  shellFormatJson _ctx repo = J.toJSON (showRepo repo)

statusColour :: Thrift.DatabaseStatus -> Color
statusColour status = case status of
  Thrift.DatabaseStatus_Complete -> Green
  Thrift.DatabaseStatus_Finalizing -> Green
  Thrift.DatabaseStatus_Incomplete -> Blue
  Thrift.DatabaseStatus_Restoring -> Black
  Thrift.DatabaseStatus_Broken -> Red
  Thrift.DatabaseStatus_Restorable -> Black
  Thrift.DatabaseStatus_Missing -> Black

statusStyle :: Thrift.DatabaseStatus -> AnsiStyle
statusStyle = color . statusColour

instance ShellFormat Thrift.Database where
  shellFormatText ctx db = shellFormatText ctx (db, []::[(String, Void)])
  shellFormatJson ctx db = shellFormatJson ctx (db, []::[(String, Void)])

instance ShellFormat v => ShellFormat (Thrift.Database, [(String, v)]) where
  shellFormatText ctx@Context{..} (db, extras) = nest 2 $ vsep $
    [ annotate (statusStyle status) (shellFormatText ctx repo)
      <+> shellFormatText ctx status]
    ++
    [ "Created:" <+> showWhen (Thrift.database_created_since_epoch db) ]
    ++
    [ "Completed:" <+> showWhen t
    | Just t <- [Thrift.database_completed db]
    ]
    ++
    [ pretty key <> ":" <+> shellFormatText ctx value
    | (key, value) <- extras]
    ++
    [ "Backup:" <+> pretty loc
    | ctxVerbose ||
      Thrift.database_status db == Thrift.DatabaseStatus_Restorable
    , Just loc <- [Thrift.database_location db]
    ]
    ++
    [ "Expires in:" <+> pretty (ppTimeSpan timeSpan)
    | ctxVerbose
    , Just expiresEpochTime <-
        [Thrift.unPosixEpochTime <$> Thrift.database_expire_time db]
    , let expires = Time $ fromIntegral expiresEpochTime
    , let timeSpan = expires `timeDiff` ctxNow
    ]
    ++
    [ "Shard:" <+> pretty (dbShard repo)
    | ctxVerbose
    ]
    ++
    [ nest 2 $ pretty name <> ":" <+> pretty value
    | ctxVerbose
    , (name,value) <- sortOn fst $
        HashMap.toList (Thrift.database_properties db)
    ]
    where
      showWhen (Thrift.PosixEpochTime t) =
        pretty (show (posixSecondsToUTCTime (fromIntegral t))) <+>
          parens (pretty (Text.unpack age) <+> "ago")
        where
          age = ppTimeSpanWithGranularity Hour $
            ctxNow `timeDiff` Time (fromIntegral t)

      status = Thrift.database_status db
      repo = Thrift.database_repo db
  shellFormatJson ctx (db, extras) = J.object $
    [ "repo" .= shellFormatJson ctx repo
    , "status" .= shellFormatJson ctx status
    , "created" .= jsonTime (Thrift.database_created_since_epoch db)
    , "completed" .= jsonMaybeTime (Thrift.database_completed db)
    , "backup" .= maybe J.Null J.toJSON (Thrift.database_location db)
    , "expires" .= jsonMaybeTime (Thrift.database_expire_time db)
    , "shard" .= J.toJSON (dbShard $ Thrift.database_repo db)
    ] ++
    [ jsonKeyFrom key .= shellFormatJson ctx value
    | (key, value) <- extras]
    where
      status = Thrift.database_status db
      repo = Thrift.database_repo db
      jsonMaybeTime = maybe J.Null jsonTime
      jsonTime (Thrift.PosixEpochTime t) =
        J.toJSON $ posixSecondsToUTCTime (fromIntegral t)
      jsonKeyFrom s = Text.pack $ map f s
        where
          f ' ' = '_'
          f c = toLower c

instance ShellFormat [Thrift.Database] where
  shellFormatText ctx dbs =
    vsep $ concatMap f $ sortOn Thrift.database_created_since_epoch dbs
    where f db = [shellFormatText ctx db, pretty ("    "::Text)]
  shellFormatJson ctx dbs = J.toJSON $ map (shellFormatJson ctx) dbs

instance ShellFormat v => ShellFormat [(Thrift.Database, [(String, v)])] where
  shellFormatText ctx dbs =
    vsep $ concatMap f $ sortOn (Thrift.database_created_since_epoch . fst) dbs
    where f x = [shellFormatText ctx x, pretty ("    "::Text)]
  shellFormatJson ctx dbs = J.toJSON $ map (shellFormatJson ctx) dbs

-- | Get the terminal width
getTerminalWidth :: IO (Maybe Int)
getTerminalWidth = fmap join $
  -- FIXME: This is a terrible way to get the terminal size but we don't
  -- seem to have any packages which can do this.
  System.Timeout.timeout 100000
    (withCreateProcess
      (proc "stty" ["size"]){std_out = CreatePipe, std_err = CreatePipe}
      (\_ (Just outh) (Just errh) ph -> do
          out <- hGetContents outh
          err <- hGetContents errh
          length out `seq` length err `seq` return ()
          hClose outh
          hClose errh
          ex <- waitForProcess ph
          return $ case ex of
            ExitSuccess
              | [[(_,"")],[(w,"")]] <- map reads $ words out -> Just w
            _ -> Nothing
      ))
  `catchAll` \_ -> return Nothing
