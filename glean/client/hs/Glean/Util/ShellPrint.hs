{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications, ConstraintKinds, UndecidableInstances #-}

module Glean.Util.ShellPrint
  ( DbVerbosity(..)
  , StatsFormatOpts(..)
  , ShellFormat(..)
  , ShellPrint
  , ShellPrintFormat(..)
  , PrintOpts(..)
  , getTerminalWidth
  , hPutShellPrintLn
  , putShellPrintLn
  , shellFormatOpt
  , shellPrint
  , withFormatOpts
  ) where

import Prelude hiding ((<>))

import Control.Monad
import Data.Aeson
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char
import Data.Int
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
import Text.Printf hiding (parseFormat)
import System.Exit
import System.IO
import System.Process
import System.Timeout
import Prettyprinter.Render.Terminal
import Util.Control.Exception (catchAll)
import Util.TimeSec
import Util.Timing

import Glean.Backend.Types (dbShard)
import qualified Glean.Types as Thrift
import Glean.Repo.Text (showRepo)

data ShellPrintFormat
  = TTY
  | PlainText
  | Json
  | CompactJson
  deriving (Eq,Show)

data Context = Context
  { ctxFormat :: ShellPrintFormat
  , ctxNow :: Time
  }

parseFormat :: String -> Maybe ShellPrintFormat
parseFormat "tty" = Just TTY
parseFormat "plain" = Just PlainText
parseFormat "json" = Just Json
parseFormat "compact-json" = Just CompactJson
parseFormat _ = Nothing

shellFormatOpt :: Parser (Maybe ShellPrintFormat)
shellFormatOpt =
  O.optional $ O.option (maybeReader parseFormat) $ mconcat
    [ O.long "format"
    , O.metavar "(tty|plain|json|compact-json)"
    , O.help "Output format"
    ]

data PrintOpts = PrintOpts
  { poFormat :: ShellPrintFormat
  , poNow :: Time
  , poWidth :: Maybe PageWidth
  }

type ShellPrint a = ShellFormat () a
  -- we might prefer this to be "forall o . ShellFormat o a" but that
  -- requires QuantifiedConstraints which isn't available until GHC 8.6.x
  -- and we're keeping GHC 8.4.x compatibility for now.

putShellPrintLn
  :: ShellPrint a
  => Maybe ShellPrintFormat -> a -> IO ()
putShellPrintLn = hPutShellPrintLn stdout

hPutShellPrintLn
  :: ShellPrint a
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
      { poFormat = format
      , poNow = t0
      , poWidth = Just $ AvailablePerLine width 1
      }
  shellPrint outh opts x

shellPrint
  :: forall a. ShellFormat () a
  => Handle -> PrintOpts -> a -> IO ()
shellPrint handle PrintOpts{..} x =
  Pretty.renderIO handle sds
  where
    sds =
      layoutPretty layout $ doc <> hardline
    doc = case poFormat of
      CompactJson ->
        pretty $
        BS.unpack $
        J.encode $
        shellFormatJson context () x
      Json ->
        pretty $
        BS.unpack $
        J.encodePretty $
        shellFormatJson context () x
      TTY ->
        shellFormatText context () x
      PlainText ->
        unAnnotate $ shellFormatText context () x
    context = Context
      { ctxFormat = poFormat
      , ctxNow = poNow
      }
    layout = LayoutOptions
      { layoutPageWidth = fromMaybe Unbounded poWidth
      }

type Ann = AnsiStyle

-- | Format data of type 'a' with format options of type 'o'
class ShellFormat o a where
    shellFormatText
      :: Context -> o -> a -> Doc Ann
    shellFormatJson
      :: Context -> o -> a -> Value

data WithFormatOpts o a = WithFormatOpts a o
instance ShellFormat o a => ShellFormat d (WithFormatOpts o a) where
  shellFormatText ctx _ (WithFormatOpts x opts) = shellFormatText ctx opts x
  shellFormatJson ctx _ (WithFormatOpts x opts) = shellFormatJson ctx opts x

withFormatOpts :: ShellFormat o a => a -> o -> WithFormatOpts o a
withFormatOpts = WithFormatOpts

instance ShellFormat o Void where
  shellFormatText _ctx _ v = case v of {}
  shellFormatJson _ctx _ v = case v of {}

instance ShellFormat o String where
  shellFormatText _ctx _ s = pretty s
  shellFormatJson _ctx _ s = J.toJSON s

instance ShellFormat o Thrift.DatabaseProperties where
  shellFormatText _ctx _ props = vsep
    [ pretty name <> ":" <+> pretty value
    | (name,value) <- sortOn fst $
        HashMap.toList props
    ]
  shellFormatJson _ctx _ props = J.toJSON props

instance ShellFormat o Thrift.DatabaseStatus where
  shellFormatText _ctx _ status =
    case status of
      Thrift.DatabaseStatus_Complete -> parens "complete"
      Thrift.DatabaseStatus_Available -> parens "available elsewhere"
      Thrift.DatabaseStatus_Finalizing -> parens "finalizing"
      Thrift.DatabaseStatus_Incomplete -> parens "incomplete"
      Thrift.DatabaseStatus_Restoring -> parens "restoring"
      Thrift.DatabaseStatus_Broken -> parens "broken"
      Thrift.DatabaseStatus_Restorable -> parens "restorable"
      Thrift.DatabaseStatus_Missing -> parens "missing deps"
  shellFormatJson _ctx _ status = J.toJSON @String $
      case status of
        Thrift.DatabaseStatus_Complete -> "COMPLETE"
        Thrift.DatabaseStatus_Available-> "AVAILABLE"
        Thrift.DatabaseStatus_Finalizing -> "FINALIZING"
        Thrift.DatabaseStatus_Incomplete -> "INCOMPLETE"
        Thrift.DatabaseStatus_Restoring -> "RESTORING"
        Thrift.DatabaseStatus_Broken -> "BROKEN"
        Thrift.DatabaseStatus_Restorable -> "RESTORABLE"
        Thrift.DatabaseStatus_Missing -> "MISSING"

instance ShellFormat o Thrift.Dependencies where
  shellFormatText _ctx _ dependency =
    case dependency of
      Thrift.Dependencies_stacked Thrift.Stacked{..} ->
        pretty (showRepo $ Thrift.Repo stacked_name stacked_hash)
      Thrift.Dependencies_pruned (Thrift.Pruned baseRepo _ _ _)
        -> pretty (showRepo baseRepo)
  shellFormatJson _ctx _ dependency =
    case dependency of
      Thrift.Dependencies_stacked Thrift.Stacked{..} ->
        J.toJSON (showRepo $ Thrift.Repo stacked_name stacked_hash)
      Thrift.Dependencies_pruned (Thrift.Pruned baseRepo _ _ _)
        -> J.toJSON (showRepo baseRepo)

instance ShellFormat o Thrift.Repo where
  shellFormatText _ctx _ repo = pretty (showRepo repo)
  shellFormatJson _ctx _ repo = J.toJSON (showRepo repo)

statusColour :: Thrift.DatabaseStatus -> Color
statusColour status = case status of
  Thrift.DatabaseStatus_Complete -> Green
  Thrift.DatabaseStatus_Available -> Green
  Thrift.DatabaseStatus_Finalizing -> Green
  Thrift.DatabaseStatus_Incomplete -> Blue
  Thrift.DatabaseStatus_Restoring -> Black
  Thrift.DatabaseStatus_Broken -> Red
  Thrift.DatabaseStatus_Restorable -> Black
  Thrift.DatabaseStatus_Missing -> Black

statusStyle :: Thrift.DatabaseStatus -> AnsiStyle
statusStyle = color . statusColour

data DbVerbosity
  = DbSummarise
  | DbDescribe
  deriving (Eq)

instance ShellFormat DbVerbosity Thrift.Database where
  shellFormatText ctx opts db =
    shellFormatText ctx opts (db, []::[(String, Void)])
  shellFormatJson ctx opts db =
    shellFormatJson ctx opts (db, []::[(String, Void)])

instance (ShellFormat DbVerbosity v)
  => ShellFormat DbVerbosity (Thrift.Database, [(String, v)]) where
    shellFormatText ctx@Context{..} opts (db, extras) = nest 2 $ vsep $
      [ annotate (statusStyle status) (shellFormatText ctx () repo)
        <+> shellFormatText ctx () status]
      ++
      [ "Source:" <+> showWhen t
      | Just t <- [Thrift.database_repo_hash_time db]
      ]
      ++
      [ "Created:" <+> showWhen (Thrift.database_created_since_epoch db) ]
      ++
      [ "Completed:" <+> showWhen databaseComplete_time
      | Just Thrift.DatabaseComplete{..} <- [Thrift.database_complete db]
      ]
      ++
      [ "Size:" <+> pretty(showSizeBytes (fromIntegral s))
      | Just Thrift.DatabaseComplete{..} <- [Thrift.database_complete db]
      , Just s <- [databaseComplete_bytes]
      ]
      ++
      [ "Broken:"
          <+> (if Text.null task then emptyDoc else pretty task <> ": ")
          <> pretty reason
      | Just (Thrift.DatabaseBroken task reason) <- [Thrift.database_broken db]
      ]
      ++
      [ pretty key <> ":" <+> shellFormatText ctx opts value
      | (key, value) <- extras]
      ++
      [ "Dependencies:" <+> shellFormatText ctx () dependency
      | Just dependency <- [Thrift.database_dependencies db]
      ]
      ++
      [ "Backup:" <+> pretty loc
      | verbosity == DbDescribe ||
        Thrift.database_status db == Thrift.DatabaseStatus_Restorable
      , Just loc <- [Thrift.database_location db]
      ]
      ++
      [ "Expires in:" <+> pretty (ppTimeSpan timeSpan)
      | verbosity == DbDescribe
      , Just expiresEpochTime <-
          [Thrift.unPosixEpochTime <$> Thrift.database_expire_time db]
      , let expires = Time $ fromIntegral expiresEpochTime
      , let timeSpan = expires `timeDiff` ctxNow
      ]
      ++
      [ "Shard:" <+> pretty (dbShard repo)
      | verbosity == DbDescribe
      ]
      ++
      [ nest 2 $ vsep
        [ "Properties:"
        , shellFormatText ctx () (Thrift.database_properties db)
        ]
      | verbosity == DbDescribe
      ]
      where
        showWhen (Thrift.PosixEpochTime t) =
          pretty (show (posixSecondsToUTCTime (fromIntegral t))) <+>
            parens (pretty (Text.unpack age) <+> "ago")
          where
            age = ppTimeSpanWithGranularity Hour $
              ctxNow `timeDiff` Time (fromIntegral t)

        showSizeBytes :: Double -> String
        showSizeBytes b
          | b > 1e9 = printf "%.2f GB" (b / 1e9)
          | otherwise = printf "%.2f MB" (b / 1e6)

        status = Thrift.database_status db
        repo = Thrift.database_repo db
        verbosity = opts
    shellFormatJson ctx opts (db, extras) = J.object $
      [ "repo" .= shellFormatJson ctx () repo
      , "status" .= shellFormatJson ctx () status
      , "created" .= jsonTime (Thrift.database_created_since_epoch db)
      , "completed" .= jsonMaybeTime
          (Thrift.databaseComplete_time <$> Thrift.database_complete db)
      , "backup" .= maybe J.Null J.toJSON (Thrift.database_location db)
      , "expires" .= jsonMaybeTime (Thrift.database_expire_time db)
      , "shard" .= J.toJSON (dbShard $ Thrift.database_repo db)
      , "source" .= maybe J.Null jsonTime (Thrift.database_repo_hash_time db)
      , "properties" .= shellFormatJson ctx () (Thrift.database_properties db)
      , "dependencies" .= maybe J.Null J.toJSON
        (Thrift.database_dependencies db)
      , "size" .= maybe J.Null J.toJSON
          (Thrift.databaseComplete_bytes <$> Thrift.database_complete db)
      ] ++
      [ jsonKeyFrom key .= shellFormatJson ctx opts value
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

instance ShellFormat DbVerbosity [Thrift.Database] where
  shellFormatText ctx opts dbs =
    vsep $ concatMap f $ sortOn Thrift.database_created_since_epoch dbs
    where f db = [shellFormatText ctx opts db, pretty ("    "::Text)]
  shellFormatJson ctx v dbs = J.toJSON $ map (shellFormatJson ctx v) dbs

instance (ShellFormat DbVerbosity v)
  => ShellFormat DbVerbosity [(Thrift.Database, [(String, v)])] where
    shellFormatText ctx opts dbs = vsep $ concatMap f $
      sortOn (Thrift.database_created_since_epoch . fst) dbs
      where f x = [shellFormatText ctx opts x, pretty ("    "::Text)]
    shellFormatJson ctx v dbs = J.toJSON $ map (shellFormatJson ctx v) dbs

type PredStatsList =
  [(Either Thrift.Id Thrift.PredicateRef, Thrift.PredicateStats)]
type PredStatsFilter =
  Either Thrift.Id Thrift.PredicateRef -> Bool

data StatsFormatOpts = StatsFormatOpts
  { showTotal :: Bool
  , sortBySize :: Bool
  }
  deriving (Eq)

instance ShellFormat StatsFormatOpts (PredStatsFilter, PredStatsList) where
    shellFormatText _context opts (filterPred, preds) = vsep $
        [ nest 2 $ vsep
            [ case ref of
                Left id -> pretty id
                Right pref -> pretty pref
            , "count:" <+> pretty (Thrift.predicateStats_count stats)
            , "size: " <+> pretty (getSizeInfo
                (Thrift.predicateStats_size stats) totalSizeBytes)
            ]
        | (ref, stats) <- sort $ filter (filterPred . fst) preds
        ] ++
        if showTotal opts then
          [ ""
          , "Total: " <> pretty totalFacts
              <+> "facts" <+> parens (pretty (showAllocs totalSizeBytes))
          ]
        else
          []
      where
        predicate_count = Thrift.predicateStats_count . snd
        predicate_size = Thrift.predicateStats_size . snd
        totalFacts = foldl' (+) 0 $ map predicate_count preds
        totalSizeBytes =
          foldl' (+) 0 $ map predicate_size preds
        sort = if sortBySize opts then
          sortOn $ negate . predicate_size
        else
          sortOn fst

    shellFormatJson _ _ (filterPred, preds) =
      J.toJSON $ filter (filterPred . fst) preds

getSizeInfo :: Int64 -> Int64 -> String
getSizeInfo bytes total =
  printf "%d (%s) %.4f%%" bytes humanReadableSize percentage_x
    where
      percentage_x :: Double
      percentage_x = 100 * fromIntegral bytes / fromIntegral total
      humanReadableSize = showAllocs bytes

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
