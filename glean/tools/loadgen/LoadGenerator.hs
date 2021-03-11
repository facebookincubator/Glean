{- |

A very simple load generator to test the Glean server's behaviour
under differing load scenarios. It fires identical requests at the
server with a given concurrency and measures the throughput.

Available options:
  --service TIER or HOST:PORT
                           Glean server to connect to
  --use-shards yes|no|fallback
                           Whether to specify a shard when connecting (default:
                           fallback)
  --tier TIER              specifies the server configuration to load from
                           configerator
  --enable-logging         Log requests to Scuba/Hive/...
  --query ARG              query to hit the server with
  --repo NAME/HASH         identifies the repository
  --concurrent N           number of concurrent requests (default 1)
  --time SECONDS           how long to stress the server (default 30)

For example

$ loadgen \
    --service localhost:25052 \
    --query 'search.cxx.SearchByNameAndScope { name = "SemiFuture" }' \
    --time 30 \
    --concurrent 200 \
    --repo fbsource/849a42650945d0220710a8875816aa59525fb348
157074 requests in 30.05s, 5226.66s QPS

-}

{-# LANGUAGE ApplicativeDo #-}
module LoadGenerator (main) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Default
import Data.IORef
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Options.Applicative
import System.Timeout
import Text.Printf

import Util.EventBase
import Util.Log
import Util.OptParse
import Util.Timing

import Glean.Backend (withBackend)
import Glean.Backend.Remote (setTimeout)
import Glean hiding (Config, options)
import qualified Glean
import Glean.Util.ConfigProvider
import Glean.Types

data WorkLimit
  = Time Int
    -- ^ continuously make requests for this many seconds
  | Requests Int
    -- ^ make exactly this many requests

data Config = Config
  { cfgService :: Glean.Service
  , cfgQuery :: Text
  , cfgRepo :: Repo
  , cfgConcurrent :: Int
    -- ^ Number of concurrent requests
  , cfgWorkLimit :: WorkLimit
    -- ^ workload: time or number of requests
  , cfgQueryTimeout :: Maybe Int
    -- ^ Query timeout, implemented in the query engine. When this timeout
    -- expires, we get partial results and a continuation.
  , cfgThriftTimeout :: Maybe Int
    -- ^ Thrift timeout. When this timeout expires, the whole requests
    -- fails.
  }

options :: ParserInfo Config
options = info (helper <*> parser) fullDesc
  where
    parser :: Parser Config
    parser = do
      cfgService <- Glean.options
      cfgQuery <- textOption
        (  long "query"
        <> help "query to hit the server with"
        )
      cfgRepo <- option (maybeReader parseRepo)
        (  long "repo"
        <> metavar "NAME/HASH"
        <> help "identifies the repository"
        )
      cfgConcurrent <- option auto
        (  long "concurrent"
        <> metavar "N"
        <> help "number of concurrent requests (default 1)"
        <> value 1
        )
      cfgWorkLimit <- requests <|> time
      cfgQueryTimeout <- optional $ option auto
        (  long "query-timeout"
        <> metavar "MILLISECONDS"
        <> help "sets the query timeout, implemented by Glean"
        )
      cfgThriftTimeout <- optional $ option auto
        (  long "thrift-timeout"
        <> metavar "MILLISECONDS"
        <> help "sets the thrift timeout"
        )
      return Config{..}

    time = Time <$> option auto
        (  long "time"
        <> metavar "SECONDS"
        <> help "how long to stress the server (default 30)"
        <> value 30
        )

    requests = Requests <$> option auto
        (  long "requests"
        <> metavar "N"
        <> help "how many requests to send"
        )

data Stats = Stats
  { numRequests :: {-# UNPACK #-} !(IORef Int)
  , numRequestsFailed :: {-# UNPACK #-} !(IORef Int)
  }

newStats :: IO Stats
newStats = do
  numRequests <- newIORef 0
  numRequestsFailed <- newIORef 0
  return Stats{..}

main :: IO ()
main =
  withConfigOptions options $ \(Config{..}, cfgOpts) ->
  withEventBaseDataplane $ \evb ->
  withConfigProvider cfgOpts $ \cfgAPI ->
  let
    settings = maybe id (setTimeout . fromIntegral) cfgThriftTimeout
  in
  withBackend evb cfgAPI cfgService settings $ \backend -> do
    Stats{..} <- newStats
    let
      doRequest =
        sendRequest `catch` \e -> do
          case e of
            _ | Just AsyncCancelled{} <- fromException e -> throwIO e
              | otherwise -> do
                logError (show (e :: SomeException))
                atomicModifyIORef' numRequestsFailed (\n -> (n+1,()))

      sendRequest = do
        _ <- Glean.userQuery backend cfgRepo def
          { userQuery_query = Text.encodeUtf8 cfgQuery
          , userQuery_encodings = [UserQueryEncoding_bin def]
          , userQuery_options = Just def
              { userQueryOptions_syntax = QuerySyntax_ANGLE
              , userQueryOptions_max_time_ms =
                  fmap fromIntegral cfgQueryTimeout
              }
          }
        return ()

      requestLoop = do
         let continue = doRequest >> requestLoop
         case cfgWorkLimit of
           Requests m -> do
             go <- atomicModifyIORef' numRequests $ \n ->
               (if n == m then n else n+1, n < m)
             when go continue
           _otherwise -> do
             atomicModifyIORef' numRequests $ \n -> (n+1, ())
             continue

      timeLimit = case cfgWorkLimit of
        Time secs -> timeout (fromIntegral secs * 1000000)
        _otherwise -> fmap Just

    (time, _alloc, _) <-
      timeIt $
      timeLimit $
      replicateConcurrently_ cfgConcurrent
      requestLoop

    f <- readIORef numRequestsFailed
    n <- readIORef numRequests
    let ok = n - f
    printf "%d requests failed\n" f
    printf "%d requests succeeded in %.2fs, %.2f QPS\n"
      ok time (fromIntegral ok / time)
