-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Query (QueryCommand) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Default
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Options.Applicative
import System.IO

import Thrift.Protocol.JSON (serializeJSON)
import Util.IO
import Util.OptParse

import Glean hiding (options)
import Glean.Types as Thrift

import GleanCLI.Common
import GleanCLI.Types

data QueryCommand
  = Query
      { repoSpec :: Either Text Repo
      , query :: String
      , recurse :: Bool
      , queryPageOptions :: PageOptions
      , limitFacts :: Maybe Int
      , output :: Maybe FilePath
      , statsOutput :: Maybe FilePath
      , timeout :: Maybe Int64
      , omitResults :: Bool
      }

instance Plugin QueryCommand where
  parseCommand =
    commandParser "query" (progDesc "Execute an Angle query") $ do
      repoSpec <- Left <$> repoNameOpt <|> Right <$> repoSlash
      queryPageOptions <- pageOpts
      recurse <- switch $ long "recursive"
        <> help "fetch nested facts (slower)"
      limitFacts <- optional $ option auto
        ( long "limit"
        <> metavar "FACTS"
        <> help "maximum number of facts to query"
        )
      output <- optional $ strOption
        ( long "output"
        <> short 'o'
        <> metavar "FILE"
        <> help "output the facts to a file"
        )
      statsOutput <- optional $ strOption
        ( long "stats"
        <> metavar "FILE"
        <> help "output stats to a file ('-' for stdout)"
        )
      timeout <- optional $ option auto
        ( long "timeout"
        <> metavar "MILLISECONDS"
        <> help "Override the default query timeout"
        )
      omitResults <- switch $ long "omit-results"
        <> help (
          "don't print results; " <>
          "use with --stats to get a count of results")
      query <- strArgument
        ( metavar "QUERY"
        <> help "query to execute ('@file' to read from file, '-' for stdin)"
        )
      return Query{..}

  runCommand _ _ backend Query{..} = do
    query_bytes <- case query of
      "-" -> B.hGetContents stdin
      '@':path -> B.readFile path
      _ -> return $ Text.encodeUtf8 $ Text.pack query

    repo <- case repoSpec of
      Left name -> Glean.getLatestRepo backend name
      Right repo -> return repo

    let with_output f = case output of
          Just path -> withFile path WriteMode f
          Nothing -> f stdout

        with_stats_output f = case statsOutput of
          Just path ->
            (if path == "-" then ($ stdout) else withFile path WriteMode) $
            \out -> f $ B8.hPutStrLn out . maybe "{}" serializeJSON
          Nothing -> f $ const $ return ()

    with_output $ \h_out -> with_stats_output $ \print_stats -> do
    let subtract_limit Nothing _ = Just Nothing
        subtract_limit (Just m) n
          | m > n = Just $ Just (m-n)
          | otherwise = Nothing

        loop cont limit = do
          UserQueryResults{..} <- Glean.userQuery backend repo def
            { userQuery_query = query_bytes
            , userQuery_encodings = [UserQueryEncoding_json
                def{ userQueryEncodingJSON_expand_results = recurse }]
            , userQuery_options = Just def
                { userQueryOptions_max_results =
                    case (limit, fromIntegral <$> pageFacts queryPageOptions) of
                      (Just m, Just n) -> Just $ m `min` n
                      (x,y) -> x <|> y
                , userQueryOptions_max_bytes =
                    Just $ fromIntegral $ pageBytes queryPageOptions
                , userQueryOptions_max_time_ms = timeout
                , userQueryOptions_continuation = cont
                , userQueryOptions_syntax = QuerySyntax_ANGLE
                , userQueryOptions_recursive = recurse
                , userQueryOptions_omit_results = omitResults
                }
            }
          n <- case userQueryResults_results of
            UserQueryEncodedResults_json UserQueryResultsJSON{..} -> do
              mapM_ (B8.hPutStrLn h_out) userQueryResultsJSON_facts
              return $ length userQueryResultsJSON_facts
            _ -> die 1 "error: unexpected results encoding"
          print_stats userQueryResults_stats
          case userQueryResults_continuation of
            Just new_cont
              | Just new_limit <- subtract_limit limit $ fromIntegral n ->
                  loop (Just new_cont) new_limit
            _ -> return ()
    loop Nothing $ fromIntegral <$> limitFacts
