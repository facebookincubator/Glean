{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Logging
  (
  -- * classes for convenient typed logging
    LogRepo(..)
  , LogResult(..)
  , LogRequest(..)
  , LogError(..)

  -- * some types
  , ErrorTy(..)
  , ErrorText(..)
  , ErrorLogger
  , errorText

  ) where

import Logger.GleanGlass ( GleanGlassLogger )
import Logger.GleanGlassErrors ( GleanGlassErrorsLogger )
import Util.Logger ( ActionLog(..) )
import qualified Logger.GleanGlass as Logger
import qualified Logger.GleanGlassErrors as Errors

import Data.Text ( Text )
import Util.Text ( textShow )

import Glean ( Repo(..) )

import Glean.Glass.Types
import Glean.Glass.Query (RepoSearchResult(..), FeelingLuckyResult(..))
import qualified Data.Text as Text
import Data.List.NonEmpty ( NonEmpty(..), toList )
import qualified Data.List.NonEmpty as NE

instance ActionLog GleanGlassLogger where
  successLog = Logger.setSuccess True
  failureLog ex = mconcat
    [ Logger.setSuccess False
    , Logger.setError (textShow ex)
    ]
  timeLog = Logger.setTimeElapsedUs . floor . (* 1000) . (* 1000)
  allocLog = Logger.setAllocatedBytes . fromIntegral

class LogRequest a where
  logRequest :: a -> GleanGlassLogger

instance LogRequest DocumentSymbolsRequest where
  logRequest = logDocumentSymbolsRequestSG Logger.setFilepath Logger.setRepo

instance LogRequest SymbolId where
  logRequest = logSymbolSG Logger.setSymbol

instance LogRequest USR where
  logRequest (USR hash) = logSymbolSG Logger.setSymbol (SymbolId hash)

instance LogRequest SymbolPath where
  logRequest = logSymbolPathSG Logger.setFilepath Logger.setRepo

instance LogRequest Location where
  logRequest = logLocationSG Logger.setFilepath Logger.setRepo

instance LogRequest SymbolSearchRequest where
  logRequest = logSymbolSearchRequestSG Logger.setSymbol Logger.setRepo

instance LogRequest FileIncludeLocationRequest where
  logRequest FileIncludeLocationRequest{..} =
    Logger.setFilepath (unPath fileIncludeLocationRequest_filepath) <>
      Logger.setRepo (unRepoName fileIncludeLocationRequest_repository)

class LogResult a where
  logResult :: (a, GleanGlassLogger) -> GleanGlassLogger

instance LogResult DocumentSymbolListXResult where
  logResult (DocumentSymbolListXResult{..}, log) =
    log <> Logger.setItemCount (length documentSymbolListXResult_references +
      length documentSymbolListXResult_definitions)

instance LogResult FileIncludeLocationResults where
  logResult (FileIncludeLocationResults{..}, log) =
    log <> Logger.setItemCount (sum
       (map (length . fileIncludeXRef_includes)
          (unXRefFileList fileIncludeLocationResults_references)
       ))

instance LogResult DocumentSymbolIndex where
  logResult (DocumentSymbolIndex{..}, log) =
    log <> Logger.setItemCount (fromIntegral documentSymbolIndex_size)

instance LogResult Range where
  logResult (_, log) = log

instance LogResult [Location] where
  logResult (xs,log) = log <> Logger.setItemCount (length xs)

instance LogResult [LocationRange] where
  logResult (xs,log) = log <> Logger.setItemCount (length xs)

instance LogResult Location where
  logResult (Location{..},log) = log <> mconcat
    [ Logger.setItemCount 1
    , Logger.setRepo $ unRepoName location_repository
    ]

instance LogResult LocationRange where
  logResult (LocationRange{..},log) = log <> mconcat
    [ Logger.setItemCount 1
    , Logger.setRepo $ unRepoName locationRange_repository
    ]

instance LogResult SymbolDescription where
  logResult (SymbolDescription{..}, log) = log <>
    logResult (symbolDescription_location, log) <>
    logResult (symbolDescription_sym, log) <>
    Logger.setItemCount 1

instance LogResult SymbolPath where
  logResult (SymbolPath{..}, log) = log <>
    Logger.setRepo (unRepoName symbolPath_repository)

instance LogResult SymbolId where
  logResult (sym, log) = log <> logSymbolSG Logger.setSymbol sym

instance LogResult [SymbolId] where
  logResult (xs, log) = log <> Logger.setItemCount (length xs)

instance LogResult SymbolSearchResult where
  logResult (SymbolSearchResult{..}, log) =
    log <> Logger.setItemCount (length symbolSearchResult_symbols)

instance LogResult RepoSearchResult where
  logResult (RepoSearchResult rs, log) =
    log <> Logger.setItemCount (length rs)

instance LogResult FeelingLuckyResult where
  logResult (FeelingLuckyResult rs, log) =
    log <> Logger.setItemCount
      (sum (map (sum . map (length . unRepoSearchResult)) rs))

instance LogResult SearchBySymbolIdResult where
  logResult (SearchBySymbolIdResult symids, log) = logResult (symids, log)

instance LogResult SearchRelatedResult where
  logResult (SearchRelatedResult{..}, log) =
    logResult (searchRelatedResult_edges, log)

instance LogResult RelatedNeighborhoodResult where
  logResult (RelatedNeighborhoodResult{..}, log) =
    log <> Logger.setItemCount
      (length relatedNeighborhoodResult_containsChildren +
        length relatedNeighborhoodResult_extendsChildren +
        length relatedNeighborhoodResult_containsParents +
        length relatedNeighborhoodResult_extendsParents +
        sum (map (\x -> 1 + length (inheritedSymbols_provides x))
              relatedNeighborhoodResult_inheritedSymbols
            )
      )

instance LogResult USRSymbolDefinition where
  logResult (USRSymbolDefinition{..}, log) = log <>
    logResult (uSRSymbolDefinition_location, log) <>
    Logger.setItemCount 1

instance LogResult [RelatedSymbols] where
  logResult (edges, log) = log <> Logger.setItemCount (length edges)

class LogRepo a where
  logRepo :: a -> GleanGlassLogger

instance LogRepo Glean.Repo where
  logRepo = logRepoSG Logger.setRepoName Logger.setRepoHash

instance LogRepo (Glean.Repo,a) where
  logRepo (repo,_) = logRepo repo

instance LogRepo (a,Glean.Repo) where
  logRepo (_,repo) = logRepo repo

instance {-# OVERLAPPABLE #-} LogRepo a => LogRepo (a,b) where
  logRepo (repo,_) = logRepo repo

instance {-# OVERLAPPABLE #-} LogRepo a => LogRepo (a,b,c) where
  logRepo (repo,_,_) = logRepo repo

-- For queries that search multiple repos, better log the set of dbs we touch
instance LogRepo (NonEmpty (a, Glean.Repo)) where
  logRepo = logRepo . NE.map snd

instance LogRepo (NonEmpty Glean.Repo) where
  logRepo (repo :| []) = logRepo repo
  logRepo rs@(_ :| _) =
    Logger.setRepoName (commas repo_name rs) <>
    Logger.setRepoHash (commas (Text.take 12 . repo_hash) rs)

commas :: (Glean.Repo -> Text) -> NonEmpty Glean.Repo -> Text
commas f = Text.intercalate "," . map f . toList

--
-- | Intern error logging
--

newtype ErrorText = ErrorText Text

-- | Types of missing data / internal logic errors
data ErrorTy
  = NoSrcFileFact !Text
  | NoSrcFileLinesFact !Text
  | EntitySearchFail !Text
  | EntityNotSupported !Text
  | AttributesError !Text
  | AggregateError [ErrorTy]

errorText :: ErrorTy -> Text
errorText e = case e of
  NoSrcFileFact t -> t
  NoSrcFileLinesFact t -> t
  EntitySearchFail t -> t
  EntityNotSupported t -> t
  AttributesError t -> t
  AggregateError errs ->
    Text.unlines $ "Multiple errors:": map (("  " <>) . errorText) errs

type ErrorLogger = GleanGlassErrorsLogger

class LogError a where
  logError :: a -> GleanGlassErrorsLogger

instance LogError ErrorTy where
  logError e =
    Errors.setError (errorText e) <>
    Errors.setErrorType (case e of
      NoSrcFileFact{} -> "NoSrcFileFact"
      NoSrcFileLinesFact{} -> "NoSrcFileLinesFact"
      EntitySearchFail{} -> "EntitySearchFail"
      EntityNotSupported{} -> "EntityNotSupported"
      AttributesError{} -> "AttributesError"
      AggregateError{} -> "AggregateError"
    )

instance LogError Glean.Repo where
  logError = logRepoSG Errors.setRepoName Errors.setRepoHash

instance LogError USR where
  logError (USR hash) = Errors.setSymbol hash

instance LogError (NonEmpty (a, Glean.Repo)) where
  logError = logError . NE.map snd

instance LogError (NonEmpty Glean.Repo) where
  logError (repo :| []) = logError repo
  logError rs =
    Errors.setRepoName (commas repo_name rs) <>
    Errors.setRepoHash (commas (Text.take 12 . repo_hash) rs)

-- sometimes we return more than just the repo result
instance LogError (a,Glean.Repo) where
  logError (_,repo) = logError repo

instance LogError (Glean.Repo,a) where
  logError (repo,_) = logError repo

instance {-# OVERLAPPABLE #-} LogError a => LogError (a,b) where
  logError (repo,_) = logError repo

instance {-# OVERLAPPABLE #-} LogError a => LogError (a,b,c) where
  logError (repo,_,_) = logError repo

instance LogError DocumentSymbolsRequest where
  logError = logDocumentSymbolsRequestSG Errors.setFilepath Errors.setRepo

instance LogError SymbolId where
  logError = logSymbolSG Errors.setSymbol

instance LogError SymbolPath where
  logError = logSymbolPathSG Errors.setFilepath Errors.setRepo

instance LogError Location where
  logError = logLocationSG Errors.setFilepath Errors.setRepo

instance LogError SymbolSearchRequest where
  logError = logSymbolSearchRequestSG Errors.setSymbol Errors.setRepo

instance LogError FileIncludeLocationRequest where
  logError FileIncludeLocationRequest{..} =
    Errors.setFilepath (unPath fileIncludeLocationRequest_filepath) <>
      Errors.setRepo (unRepoName fileIncludeLocationRequest_repository)

--
-- Lift log accessors generically over Glass types
--

logSymbolSG :: Semigroup a => (Text -> a)
  -> SymbolId -> a
logSymbolSG f (SymbolId s) = f s

logRepoSG :: Semigroup a => (Text -> a) -> (Text -> a)
  -> Repo -> a
logRepoSG f g Glean.Repo{..} = f repo_name <> g repo_hash

logSymbolPathSG :: Semigroup a => (Text -> a) -> (Text -> a)
  -> SymbolPath -> a
logSymbolPathSG f g SymbolPath{..} =
    f (unPath symbolPath_filepath) <>
      g (unRepoName symbolPath_repository)

logLocationSG :: Semigroup a => (Text -> a) -> (Text -> a) -> Location -> a
logLocationSG f g Location{..} =
     f (unPath location_filepath) <>
      g (unRepoName location_repository)

logDocumentSymbolsRequestSG :: Semigroup a => (Text -> a) -> (Text -> a)
  -> DocumentSymbolsRequest  -> a
logDocumentSymbolsRequestSG f g DocumentSymbolsRequest{..} =
    f (unPath documentSymbolsRequest_filepath) <>
    g (unRepoName documentSymbolsRequest_repository)

logSymbolSearchRequestSG :: Semigroup a => (Text -> a) -> (Text -> a)
  -> SymbolSearchRequest -> a
logSymbolSearchRequestSG logQuery logRepo SymbolSearchRequest{..} =
  case repo of
    Nothing -> logQuery symbolSearchRequest_name
    Just r -> logRepo r <> logQuery symbolSearchRequest_name
  where
    repo = unRepoName <$> symbolSearchRequest_repo_name
