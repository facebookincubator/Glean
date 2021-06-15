-- Copyright 2004-present Facebook. All Rights Reserved.

module Glean.Util.ShellPrint
  ( ShellFormat(..)
  , shellPrint
  ) where

import Prelude hiding ((<>))

import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Time.Format as Time
import Data.List hiding (span)
import Util.TimeSec
import Text.PrettyPrint.Annotated.HughesPJ
import System.Console.ANSI
import System.IO

import qualified Glean.Types as Thrift
import qualified Glean

data Context = Context
  { ctxVerbose :: Bool
  , ctxIsTTY :: Bool
  , ctxNow :: Time
  }

shellPrint :: ShellFormat a => Handle -> Bool -> Bool -> Time -> a -> IO ()
shellPrint outh ctxVerbose ctxIsTTY ctxNow x =
  hPutStrLn outh s
  where
    s =
      renderDecorated
        colourStart
        colourEnd
        (shellFormat Context{..} x)
    colourStart c
      | ctxIsTTY =
        setSGRCode [SetColor Foreground Vivid c]
      | otherwise = mempty
    colourEnd _
      | ctxIsTTY =
        setSGRCode [Reset]
      | otherwise = mempty

class ShellFormat a where
    shellFormat :: Context -> a -> Doc Color

instance ShellFormat Thrift.DatabaseStatus where
  shellFormat _ctx status =
    case status of
      Thrift.DatabaseStatus_Complete -> parens "complete"
      Thrift.DatabaseStatus_Finalizing -> parens "finalizing"
      Thrift.DatabaseStatus_Incomplete -> parens "incomplete"
      Thrift.DatabaseStatus_Restoring -> parens "restoring"
      Thrift.DatabaseStatus_Broken -> parens "broken"
      Thrift.DatabaseStatus_Restorable -> parens "restorable"
      Thrift.DatabaseStatus_Missing -> parens "missing deps"

instance ShellFormat (Maybe Thrift.DatabaseStatus) where
  shellFormat ctx status =
    maybe
      (parens "unknown")
      (shellFormat ctx)
      status

instance ShellFormat Thrift.Repo where
  shellFormat _ctx repo = hcat
    [ text $ Text.unpack (Thrift.repo_name repo)
    , text $ "/"
    , text $ Text.unpack (Thrift.repo_hash repo)
    ]

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
  shellFormat ctx@Context{..} db = cat
    [ annotate (statusColour status) (shellFormat ctx repo)
        <+> shellFormat ctx status
      , nest 2 $ vcat $
        [ "Created:" <+> text t <+>
          let age = ctxNow `timeDiff` fromUTCTime utc in parens $
          text (Text.unpack (ppTimeSpanWithGranularity Hour age)) <> " ago"
        | Just t <- [Text.unpack <$> Thrift.database_created db]
        , not (null t)
        , Just utc <-
          [Time.parseTimeM False Time.defaultTimeLocale iso8601 t]
        ]
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
        [ "Shard:" <+> text (Text.unpack (Glean.dbShard db))
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
      status = Thrift.database_status db
      repo = Thrift.database_repo db
      iso8601 = "%Y-%m-%dT%H:%M:%SZ"
