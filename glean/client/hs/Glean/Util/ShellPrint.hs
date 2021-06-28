{-# LANGUAGE TypeApplications #-}
-- Copyright 2004-present Facebook. All Rights Reserved.

module Glean.Util.ShellPrint
  ( ShellFormat(..)
  , ShellPrintFormat(..)
  , shellFormatOpt
  , shellPrint
  ) where

import Prelude hiding ((<>))

import Data.Aeson
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Data.Time.Clock.POSIX
import Data.List hiding (span)
import Data.Void
import Options.Applicative as O
import System.Console.ANSI
import Text.PrettyPrint.Annotated.HughesPJ
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

shellPrint :: ShellFormat a => Bool -> ShellPrintFormat -> Time -> a -> String
shellPrint ctxVerbose ctxFormat ctxNow x =
  case ctxFormat of
    Json ->
      BS.unpack $
      J.encodePretty $
      shellFormatJson Context{..} x
    TTY ->
      renderText
        (\c -> setSGRCode [SetColor Foreground Vivid c])
        (const $ setSGRCode [Reset])
    PlainText -> renderText mempty mempty
  where
    renderText colourStart colourEnd =
      renderDecorated
        colourStart
        colourEnd
        (shellFormatText Context{..} x)

class ShellFormat a where
    shellFormatText :: Context -> a -> Doc Color
    shellFormatJson :: Context -> a -> Value

instance ShellFormat Void where
  shellFormatText _ctx v = case v of
  shellFormatJson _ctx v = case v of

instance ShellFormat String where
  shellFormatText _ctx s = text s
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

instance ShellFormat (Maybe Thrift.DatabaseStatus) where
  shellFormatText ctx status =
    maybe
      (parens "unknown")
      (shellFormatText ctx)
      status
  shellFormatJson ctx status =
    maybe
      (J.toJSON @String "UNKNOWN")
      (shellFormatJson ctx)
      status

instance ShellFormat Thrift.Repo where
  shellFormatText _ctx repo = text (showRepo repo)
  shellFormatJson _ctx repo = J.toJSON (showRepo repo)

statusColour :: Maybe Thrift.DatabaseStatus -> Color
statusColour status = case status of
  Just Thrift.DatabaseStatus_Complete -> Green
  Just Thrift.DatabaseStatus_Finalizing -> Green
  Just Thrift.DatabaseStatus_Incomplete -> Blue
  Just Thrift.DatabaseStatus_Restoring -> Black
  Just Thrift.DatabaseStatus_Broken -> Red
  Just Thrift.DatabaseStatus_Restorable -> Black
  Just Thrift.DatabaseStatus_Missing -> Black
  Nothing -> Red

instance ShellFormat Thrift.Database where
  shellFormatText ctx db = shellFormatText ctx (db, []::[(String, Void)])
  shellFormatJson ctx db = shellFormatJson ctx (db, []::[(String, Void)])

instance ShellFormat v => ShellFormat (Thrift.Database, [(String, v)]) where
  shellFormatText ctx@Context{..} (db, extras) = vcat
    [ annotate (statusColour status) (shellFormatText ctx repo)
        <+> shellFormatText ctx status
      , nest 2 $ vcat $
        [ "Created:" <+> showWhen t
        | Just t <- [Thrift.database_created_since_epoch db]
        ]
        ++
        [ "Completed:" <+> showWhen t
        | Just t <- [Thrift.database_completed db]
        ]
        ++
        [ text key <> ":" <+> shellFormatText ctx value
        | (key, value) <- extras]
        ++
        [ "Backup:" <+> text (Text.unpack loc)
        | ctxVerbose ||
          Thrift.database_status db == Just Thrift.DatabaseStatus_Restorable
        , Just loc <- [Thrift.database_location db]
        ]
        ++
        [ "Expires in:" <+> text (Text.unpack (ppTimeSpan timeSpan))
        | ctxVerbose
        , Just expiresEpochTime <-
            [Thrift.unPosixEpochTime <$> Thrift.database_expire_time db]
        , let expires = Time $ fromIntegral expiresEpochTime
        , let timeSpan = expires `timeDiff` ctxNow
        ]
        ++
        [ "Shard:" <+> text (Text.unpack (dbShard repo))
        | ctxVerbose
        ]
        ++
        [ nest 2 $ text (Text.unpack name) <> ":" <+> text (Text.unpack value)
        | ctxVerbose
        , (name,value) <- sortOn fst $
            HashMap.toList (Thrift.database_properties db)
        ]
    ]
    where
      showWhen (Thrift.PosixEpochTime t) =
        text (show (posixSecondsToUTCTime (fromIntegral t))) <+>
          parens (text (Text.unpack age) <+> "ago")
        where
          age = ppTimeSpanWithGranularity Hour $
            ctxNow `timeDiff` Time (fromIntegral t)

      status = Thrift.database_status db
      repo = Thrift.database_repo db
  shellFormatJson ctx (db, extras) = J.object $
    [ "repo" .= shellFormatJson ctx repo
    , "status" .= shellFormatJson ctx status
    , "created" .= jsonMaybeTime (Thrift.database_created_since_epoch db)
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
    vcat $ concatMap f $ sortOn Thrift.database_created_since_epoch dbs
    where f db = [shellFormatText ctx db, text "    "]
  shellFormatJson ctx dbs = J.toJSON $ map (shellFormatJson ctx) dbs
