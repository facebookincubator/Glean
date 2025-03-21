{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Snapshot.Query (runQuery) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.UTF8 as UTF8
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Compat.Prettyprinter (pretty)
import qualified Data.Yaml as Yaml
import System.Exit
import qualified Text.JSON as JSON
import qualified Data.Vector as Vector

import Util.JSON.Pretty ()

import qualified Glean
import qualified Glean.Types as Thrift
import Glean.Regression.Snapshot.Transform

newtype TQ = TQ { fromTQ :: [(Text, Aeson.Value)] }

instance Aeson.FromJSON TQ where
  parseJSON = Aeson.withArray "transform" $ go [] . Vector.toList
    where
      go ts [] = return $ TQ $ reverse ts
      go ts (Aeson.String s : v : xs) = go ((s,v) : ts) xs
      go _ _ = fail "invalid transform"

data Query = Query
  { queryText :: UTF8.ByteString
  , queryRecursive :: Bool
  , queryPerf :: Bool
    -- ^ Also gather performance results, compare against test.perf
  , queryMaxResults :: Maybe Int
  , queryTransforms :: TQ
  , queryDerive :: Bool
  }

instance Aeson.FromJSON Query where
  parseJSON = Aeson.withObject "query" $ \v -> do
    queryText <- Text.encodeUtf8 <$> v Aeson..: "query"
    queryRecursive <- v Aeson..:! "recursive" Aeson..!= True
    queryPerf <- v Aeson..:! "perf" Aeson..!= False
    queryMaxResults <- v Aeson..:! "max_results"
    queryTransforms <- v Aeson..:! "transform" Aeson..!= TQ []
    queryDerive <- v Aeson..:! "derive" Aeson..!= False
    return Query{..}

runQuery
  :: Glean.Backend e => e
  -> Thrift.Repo
  -> Transforms
  -> FilePath
  -> IO (
    String, -- The results as transformed, pretty-printed JSON
    Maybe String -- Query performance stats, also pretty-printed JSON
  )
runQuery backend repo xforms qfile =
  handle (\(Glean.BadQuery b) -> throwIO $ Glean.BadQuery ("in " <> Text.pack qfile <> ": " <> b)) $ do
  r <- Yaml.decodeFileEither qfile
  case r of
    Left err -> die
      $ qfile ++ ": invalid query - " ++ Yaml.prettyPrintParseException err
    Right Query{..} -> do
      transform <-
        fmap (foldl' (.) id) $ mapM getTransform $ fromTQ queryTransforms
      let mkQuery k cont = def
            { Thrift.userQuery_query = queryText
            , Thrift.userQuery_options = Just def
                { Thrift.userQueryOptions_no_base64_binary = True
                , Thrift.userQueryOptions_syntax = Thrift.QuerySyntax_ANGLE
                , Thrift.userQueryOptions_expand_results = True
                , Thrift.userQueryOptions_recursive = queryRecursive
                , Thrift.userQueryOptions_max_results = fromIntegral <$> k
                , Thrift.userQueryOptions_continuation = cont
                , Thrift.userQueryOptions_collect_facts_searched = queryPerf
                , Thrift.userQueryOptions_store_derived_facts = queryDerive
                }
            }

          collect acc perf k cont = do
            res <- liftIO $ Glean.userQuery backend repo $ mkQuery k cont
            let facts = Thrift.userQueryResults_facts res
                remaining = subtract (length facts) <$> k
                perf' = perf <> Thrift.userQueryResults_stats res
            if isJust cont && maybe True (>0) remaining
              then collect (facts ++ acc) perf' remaining cont
              else return (facts ++ acc, perf')

      (facts, perf) <- collect [] Nothing queryMaxResults Nothing

      let generatedTag = '@':"generated"

      perfString <- if queryPerf
        then do
          Thrift.SchemaInfo{..} <- Glean.getSchemaInfo backend (Just repo)
            def { Thrift.getSchemaInfo_omit_source = True }
          return $ Just $ show $ pretty $ JSON.JSObject $ JSON.toJSObject
            [ (generatedTag, JSON.JSNull)
            , ("facts_searched", JSON.JSObject $ JSON.toJSObject $
              sortBy (comparing fst)
                [ (show (pretty pred), JSON.JSRational False (fromIntegral n))
                | Just facts <- [Thrift.userQueryStats_facts_searched =<< perf]
                , (pid,n) <- Map.toList facts
                , Just pred <- [Map.lookup pid schemaInfo_predicateIds]
                ])
            , ("full_scans", JSON.JSArray $ sort $
                [ JSON.JSString $ JSON.toJSString $ show (pretty pred)
                | Just scans <- [Thrift.userQueryStats_full_scans <$> perf]
                , pred <- scans
                ])
            ]
        else
          return Nothing

      resultString <- fmap
        (show
          . pretty
          . JSON.JSArray
          . (JSON.JSString (JSON.toJSString generatedTag) :)
          . transform
          . map nukeIds)
        $ forM facts $ \fact -> case JSON.decode (UTF8.toString fact) of
          JSON.Error err -> die $ "invalid fact in response: " ++ err
          JSON.Ok (value :: JSON.JSValue) -> return value

      return (resultString, perfString)

  where
    getTransform (name, arg)
      | Just (Transform f) <- HashMap.lookup name xforms =
          case Aeson.fromJSON arg of
            Aeson.Success x -> return $ f x
            Aeson.Error msg ->
              die $
                "invalid argument to transform '" ++ Text.unpack name ++ "': "
                ++ msg
      | otherwise = die $ "unknown transform '" ++ Text.unpack name ++ "'"
