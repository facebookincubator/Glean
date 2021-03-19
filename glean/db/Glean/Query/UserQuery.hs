module Glean.Query.UserQuery
  ( userQueryFacts
  , userQuery
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Except
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Coerce
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (hash)
import Data.Int
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Scientific
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Data.Word (Word64)
import TextShow

import ServiceData.GlobalStats as Stats
import qualified ServiceData.Types as Stats
import Thrift.Protocol.JSON.Base64
import Util.AllocLimit
import Util.Timing
import Util.Log

import qualified Glean.Angle.Parser as Angle
import Glean.Angle.Types hiding (Type, FieldDef)
import qualified Glean.Angle.Types as Angle
import Glean.Database.Schema.Types
import Glean.Database.Stuff
import Glean.Database.Types as Database
import Glean.Database.Writes
import Glean.FFI
import Glean.Query.Codegen hiding (Match(..))
import Glean.Query.Flatten
import Glean.Query.Opt
import Glean.Query.Reorder
import Glean.RTS as RTS
import Glean.RTS.Bytecode.Disassemble
import qualified Glean.RTS.Bytecode.Gen.Version as Bytecode
import qualified Glean.RTS.Foreign.Bytecode as Bytecode
import Glean.RTS.Foreign.FactSet (FactSet)
import qualified Glean.RTS.Foreign.FactSet as FactSet
import Glean.RTS.Foreign.Lookup
import Glean.RTS.Foreign.Query
import Glean.RTS.Foreign.Stacked (stacked)
import Glean.RTS.Types (Type, FieldDef, PidRef(..), ExpandedType(..))
import qualified Glean.RTS.Term as RTS
import Glean.RTS.Term (Term(Ref,Alt,Tuple,ByteArray), Match(..))
import qualified Glean.Types as Thrift
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Query.JSON
import Glean.Query.Nested
import Glean.Query.Nested.Compile
import Glean.Query.Nested.Types
import Glean.Schema.Resolve (resolveType)
import Glean.Schema.Util
import Glean.Util.Observed as Observed
import Glean.Query.Typecheck

-- NOTE: We keep the public interface monomorphic, at least for now.

--
-- | Perform a query using the JSON query syntax, and return the results
-- as JSON-encoded Thrift data.
--
userQuery
  :: Database.Env
  -> Thrift.Repo
  -> Thrift.UserQuery
  -> IO Thrift.UserQueryResults
userQuery env repo query = chooseEncoding
  (Thrift.userQuery_options query)
  (Thrift.userQuery_encodings query)
  $ genericUserQuery env repo query

--
-- | Query a list of facts and return the result as JSON-encoded Thrift data.
--
userQueryFacts
  :: Database.Env
  -> Thrift.Repo
  -> Thrift.UserQueryFacts
  -> IO Thrift.UserQueryResults
userQueryFacts env repo query = chooseEncoding
  (Thrift.userQueryFacts_options query)
  (Thrift.userQueryFacts_encodings query)
  $ genericUserQueryFacts env repo query

-- | A generic implementation of scan queries.
genericUserQuery
  :: Encoding e
  => Database.Env
  -> Thrift.Repo
  -> Thrift.UserQuery
  -> e
  -> IO Thrift.UserQueryResults
{-# INLINE genericUserQuery #-}
genericUserQuery env repo query enc = do
  config@ServerConfig.Config{..} <- Observed.get (envServerConfig env)
  readDatabase env repo $ \schema lookup ->
    maybe id withSavedAllocLimit config_query_alloc_limit
      $ performUserQuery enc schema
      $ userQueryImpl env schema config lookup repo query

-- | A generic implementation of lookup queries.
genericUserQueryFacts
  :: Encoding e
  => Database.Env
  -> Thrift.Repo
  -> Thrift.UserQueryFacts
  -> e
  -> IO Thrift.UserQueryResults
{-# INLINE genericUserQueryFacts #-}
genericUserQueryFacts env repo query enc = do
  config <- Observed.get (envServerConfig env)
  readDatabase env repo $ \schema lookup ->
    performUserQuery enc schema $
      userQueryFactsImpl schema config lookup query

-- | Results returned from a query, parametrised of the types of facts and
-- statistics
data Results stats fact = Results
  { -- | Facts matching the query
    resFacts :: [(Fid, fact)]

    -- | Type of the results, if known
  , resPredicate :: Maybe PredicateDetails

    -- | Additional facts satisfying nested parts of the query
  , resNestedFacts :: IntMap fact

    -- | Continuation for paging
  , resCont :: Maybe Thrift.UserQueryCont

    -- | Statistics
  , resStats :: stats

    -- | Diagnostics
  , resDiags :: [Text]

    -- | Write handle for derived facts
  , resWriteHandle :: Maybe Thrift.Handle

    -- | Count of the facts searched per Pid
  , resFactsSearched :: Maybe (Map Int64 Int64)

    -- | Inferred type of the query, for logging
  , resType :: Maybe Text

    -- | Size of compiled bytecode, for logging
  , resBytecodeSize :: Maybe Int

    -- | Query compile time, for logging, in seconds
  , resCompileTime :: Maybe Double

    -- | Query execution time after compilation, for logging, in nanoseconds
  , resExecutionTime :: Maybe Word64
  }

class Encoding e where
  type EncodedFact e

  shouldExpand :: e -> Bool
  serializeFact
    :: e
    -> IntMap (EncodedFact e)
    -> Fid
    -> PredicateDetails
    -> Thrift.Fact
    -> IO (EncodedFact e)
  setResults
    :: e
    -> Results stats (EncodedFact e)
    -> Thrift.UserQueryResults
    -> Thrift.UserQueryResults

withEncoding
  :: Thrift.UserQueryEncoding
  -> (forall e. Encoding e => e -> a)
  -> Maybe a
{-# INLINE withEncoding #-}
withEncoding (Thrift.UserQueryEncoding_bin x) f = Just $ f x
withEncoding (Thrift.UserQueryEncoding_json x) f = Just $ f x
withEncoding (Thrift.UserQueryEncoding_compact x) f = Just $ f x
withEncoding _ _ = Nothing

chooseEncoding
  :: Maybe Thrift.UserQueryOptions
  -> [Thrift.UserQueryEncoding]
  -> (forall e. Encoding e => e -> IO a)
  -> IO a
{-# INLINE chooseEncoding #-}
chooseEncoding mopts encs f = do
  forM_ (Thrift.userQueryOptions_continuation =<< mopts) checkUserQueryCont
  case encs of
    [] -> f $ LegacyJSONEncoding $ fromMaybe def mopts
    _ | act : _ <- mapMaybe (`withEncoding` f) encs -> act
      | otherwise -> throwIO $ Thrift.BadQuery
          "none of the requested encodings are supported"

newtype LegacyJSONEncoding = LegacyJSONEncoding Thrift.UserQueryOptions

instance Encoding LegacyJSONEncoding where
  type EncodedFact LegacyJSONEncoding = ByteString

  shouldExpand (LegacyJSONEncoding opts) =
    Thrift.userQueryOptions_expand_results opts

  serializeFact
    (LegacyJSONEncoding opts)
    serialized
    fid
    predicateDetails
    Thrift.Fact{..} = factToJSON
      (Thrift.userQueryOptions_no_base64_binary opts)
      serialized
      predicateDetails
      fid
      fact_key
      fact_value

  setResults _ res qres = qres
      { Thrift.userQueryResults_facts = snd <$> resFacts res
      , Thrift.userQueryResults_nestedFacts = Map.fromList
          [ (fromIntegral i, fact)
            | (i,fact) <- IntMap.toList $ resNestedFacts res ]
      }

instance Encoding Thrift.UserQueryEncodingJSON where
  type EncodedFact Thrift.UserQueryEncodingJSON = ByteString

  shouldExpand = Thrift.userQueryEncodingJSON_expand_results

  serializeFact enc serialized fid predicateDetails Thrift.Fact{..} =
    factToJSON
      (Thrift.userQueryEncodingJSON_no_base64_binary enc)
      serialized
      predicateDetails
      fid
      fact_key
      fact_value

  setResults enc res qres = qres
    { Thrift.userQueryResults_results = Thrift.UserQueryEncodedResults_json
        Thrift.UserQueryResultsJSON
          { userQueryResultsJSON_encoding = enc
          , userQueryResultsJSON_facts = snd <$> resFacts res
          , userQueryResultsJSON_nestedFacts = Map.fromList
              [ (fromIntegral i, fact)
                | (i,fact) <- IntMap.toList $ resNestedFacts res ]
          }
    }

instance Encoding Thrift.UserQueryEncodingCompact where
  type EncodedFact Thrift.UserQueryEncodingCompact = ByteString

  shouldExpand = Thrift.userQueryEncodingCompact_expand_results

  serializeFact _ serialized fid predicateDetails Thrift.Fact{..} =
    factToCompact
      serialized
      predicateDetails
      fid
      fact_key
      fact_value

  setResults enc res qres = qres
    { Thrift.userQueryResults_results = Thrift.UserQueryEncodedResults_compact
        Thrift.UserQueryResultsCompact
          { userQueryResultsCompact_encoding = enc
          , userQueryResultsCompact_facts = snd <$> resFacts res
          , userQueryResultsCompact_nestedFacts = Map.fromList
              [ (fromIntegral i, fact)
                | (i,fact) <- IntMap.toList $ resNestedFacts res ]
          }
    }

instance Encoding Thrift.UserQueryEncodingBin where
  type EncodedFact Thrift.UserQueryEncodingBin = Thrift.Fact

  shouldExpand _ = False

  serializeFact _ _ _ _ = return

  setResults enc res qres = qres
    { Thrift.userQueryResults_results = Thrift.UserQueryEncodedResults_bin
        Thrift.UserQueryResultsBin
          { userQueryResultsBin_encoding = enc
          , userQueryResultsBin_facts =
              Map.fromList
              $ coerce
              $ resFacts res
          , userQueryResultsBin_nestedFacts =
              Map.fromList
              $ map (first fromIntegral)
              $ IntMap.toList
              $ resNestedFacts res
          }
    }

-- | Perform a query and convert the 'Results' to a Thrift result
performUserQuery
  :: Encoding e
  => e
  -> DbSchema
  -> IO (Results Stats Thrift.Fact)
  -> IO Thrift.UserQueryResults
{-# INLINE performUserQuery #-}
performUserQuery encoding schema query = do
  !results <- withStats $ do
    res <- query

    -- Convert nested facts - we do it in the order of their fact ids
    -- which means we can only depend on facts converted earlier.
    nested <- foldM
      (\expanded (fid, fact@Thrift.Fact{..}) -> do
          details <- pidDetails schema fact_type
          encoded <- serializeFact
            encoding
            (if shouldExpand encoding then expanded else IntMap.empty)
            (Fid $ fromIntegral fid)
            details
            fact
          return $ IntMap.insert fid encoded expanded)
      IntMap.empty
      (IntMap.toList $ resNestedFacts res)

    let
      -- avoid lookupPid if we know the result type
      getDetails = case resPredicate res of
        Just d -> const (return d)
        Nothing -> pidDetails schema

    facts <- forM (resFacts res) $ \(i, fact) -> do
      details <- getDetails (Thrift.fact_type fact)
      enc <- serializeFact
        encoding
        (if shouldExpand encoding then nested else IntMap.empty)
        i
        details
        fact
      return (i,enc)

    return res
      { resFacts = facts
      , resNestedFacts = if shouldExpand encoding then IntMap.empty else nested
      }
  return $ setResults encoding results def
    { Thrift.userQueryResults_stats = Just $ resStats results
    , Thrift.userQueryResults_continuation = resCont results
    , Thrift.userQueryResults_diagnostics = resDiags results
    , Thrift.userQueryResults_handle = resWriteHandle results
    , Thrift.userQueryResults_type = resType results
    }

pidDetails :: DbSchema -> Int64 -> IO PredicateDetails
pidDetails schema ty = do
  case lookupPid (Pid (fromIntegral ty)) schema of
    Nothing ->  throwIO $ Thrift.Exception $
      "failed looking up fact type: " <> Text.pack (show ty)
    Just d -> return d

userQueryFactsImpl
  :: DbSchema
  -> ServerConfig.Config
  -> Lookup
  -> Thrift.UserQueryFacts
  -> IO (Results Stats Thrift.Fact)
     -- The length of the result list is guaranteed to be the same
     -- as the userQueryFacts_facts list in the input.
userQueryFactsImpl
    DbSchema{..}
    config
    lookup
    Thrift.UserQueryFacts{..} = do
  let opts = fromMaybe def userQueryFacts_options
      limits = mkQueryRuntimeOptions opts config

  vlog 2 $ "userQueryFactsImpl: " <> show (length userQueryFacts_facts)
  qResults@QueryResults{..} <- do
    nextId <- firstFreeId lookup
    -- executeCompiled needs a Define, even though we won't use it
    bracket (FactSet.new nextId) release $ \derived -> do
    let stack = stacked lookup derived
    bracket
      (compileQueryFacts userQueryFacts_facts)
      (release . compiledQuerySub) $ \sub ->
        executeCompiled schemaInventory stack sub limits

  stats <- getStats qResults

  let results = Results
        { resFacts = Vector.toList queryResultsFacts
        , resPredicate = Nothing
        , resNestedFacts = mkNestedFacts queryResultsNestedFacts
        , resCont = Nothing
        , resStats = stats
        , resDiags = []
        , resWriteHandle = Nothing
        , resFactsSearched = Nothing
        , resType = Nothing  -- could be facts of different predicates
        , resBytecodeSize = Nothing
        , resCompileTime = Nothing
        , resExecutionTime = Nothing
        }

  return $ if Thrift.userQueryOptions_omit_results opts
     then withoutFacts results
     else results

userQueryImpl
  :: Database.Env
  -> DbSchema
  -> ServerConfig.Config
  -> Lookup
  -> Thrift.Repo
  -> Thrift.UserQuery
  -> IO (Results Stats Thrift.Fact)

-- Angle queries:
userQueryImpl
  env
  schema@DbSchema{..}
  config@ServerConfig.Config{..}
  lookup
  repo
  Thrift.UserQuery{..}
  | let opts = fromMaybe def userQuery_options
  , Thrift.QuerySyntax_ANGLE <- Thrift.userQueryOptions_syntax opts = do
    let
      opts = fromMaybe def userQuery_options
      stored = Thrift.userQueryOptions_store_derived_facts opts
      debug = Thrift.userQueryOptions_debug opts

    (returnType, compileTime, irDiag, cont) <-
      case Thrift.userQueryOptions_continuation opts of
        Just ucont
          | Just retTy <- Thrift.userQueryCont_returnType ucont -> do
          (compileTime, _, returnType) <- timeIt $ compileType schema retTy
          return (returnType, compileTime, [], Right ucont)

        -- This is either a new query or the continuation of a query
        -- that returns a temporary predicate.
        _ -> do
          (compileTime, _, query@QueryWithInfo{..}) <- timeIt $
            compileAngleQuery schema userQuery_query stored
          let
            irDiag =
              [ "ir:\n" <> Text.pack (show (pretty qiQuery))
              | Thrift.queryDebugOptions_ir debug ]

            cont = case Thrift.userQueryOptions_continuation opts of
              Just c -> Right c
              Nothing -> Left query

          return (qiReturnType, compileTime, irDiag, cont)

    details@PredicateDetails{..} <- case returnType of
      Angle.Record
          [ Angle.FieldDef _ (Angle.Predicate (PidRef pid ref))
          , Angle.FieldDef _ keyTy
          , Angle.FieldDef _ valTy ] -> do
        let
          tmpDetails =
            -- This is a temporary Pid generated by Flatten.captureKey,
            -- because the query doesn't return facts. In this case we
            -- are pretending the query returns facts for a new predicate,
            -- so we need a PredicateDetails to pass to unfoldFacts below.
            -- (this is temporary, unfoldFacts wil go away in due course
            -- when we compile it to bytecode).
            PredicateDetails
              { predicatePid = pid
              , predicateKeyType = keyTy
              , predicateValueType = valTy
              , predicateRef = ref
              , predicateSchema = error "predicateSchema"
              , predicateTraversal = error "predicateTraversal"
              , predicateTypecheck = error "predicateTypecheck"
              , predicateDeriving = NoDeriving
              , predicateInStoredSchema = False
              }
        return $ fromMaybe tmpDetails
          $ IntMap.lookup (fromIntegral (fromPid pid)) predicatesById
      _ -> throwIO $ Thrift.BadQuery
        "only queries for facts are currently supported"

    unless (Text.null userQuery_predicate) $
      -- With Angle queries, setting userQuery_predicate is
      -- optional. If the client sets it, we will check it: this
      -- can be a useful way to catch errors in the client.
      -- If the query is not returning whole facts, then the
      -- client should set this field to "".
      checkPredicatesMatch schema
        details
        userQuery_predicate
        userQuery_predicate_version

    let limits = mkQueryRuntimeOptions opts config
    nextId <- case Thrift.userQueryOptions_continuation opts of
      Just Thrift.UserQueryCont{..}
        | userQueryCont_nextId > 0 -> return (Fid userQueryCont_nextId)
      _otherwise -> firstFreeId lookup
    derived <- FactSet.new nextId
    let stack = stacked lookup derived

    ( qResults@QueryResults{..}
      , queryDiag
      , bytecodeSize) <-
      case cont of
        Right ucont -> do
          let binaryCont = Thrift.userQueryCont_continuation ucont
          results <- restartCompiled schemaInventory stack
            (Just predicatePid) limits binaryCont
          return (results, [], B.length binaryCont)

        Left query -> do
          let
            bytecodeDiag sub =
              [ "bytecode:\n" <> Text.unlines
                (disassemble "Query" $ compiledQuerySub sub)
              | Thrift.queryDebugOptions_bytecode debug ]
          bracket (compileQuery query) (release . compiledQuerySub) $ \sub -> do
            results <- executeCompiled schemaInventory stack sub limits
            diags <- evaluate $ force (bytecodeDiag sub) -- don't keep sub alive
            sz <- evaluate $ Bytecode.size (compiledQuerySub sub)
            return (results, diags, sz)

    -- If we're storing derived facts, queue them for writing and
    -- return the handle.
    maybeWriteHandle <-
      if stored
        then writeDerivedFacts env repo derived
        else return Nothing

    userCont <- case queryResultsCont of
      Nothing -> return Nothing
      Just bs -> do
        nextId <- firstFreeId derived
        return $ Just $ mkUserQueryCont (Right returnType) bs nextId

    stats <- getStats qResults

    when (isJust userCont) $
      addStatValueType "glean.query.truncated" 1 Stats.Sum

    let pred = predicateRef_name predicateRef <> "." <>
          showt (predicateRef_version predicateRef)

    let results = Results
          { resFacts = Vector.toList queryResultsFacts
          , resPredicate = Just details
          , resNestedFacts = mkNestedFacts queryResultsNestedFacts
          , resCont = userCont
          , resStats = stats
          , resDiags = irDiag ++ queryDiag
          , resWriteHandle = maybeWriteHandle
          , resFactsSearched = queryResultsStats
          , resType = Just pred
          , resBytecodeSize = Just bytecodeSize
          , resCompileTime = Just compileTime
          , resExecutionTime = Just queryResultsElapsedNs
          }

    return $ if Thrift.userQueryOptions_omit_results opts
       then withoutFacts results
       else results

-- JSON queries:
userQueryImpl
  env
  schema@DbSchema{..}
  config@ServerConfig.Config{..}
  lookup
  repo
  Thrift.UserQuery{..} = do
    details@PredicateDetails{..} <-
      case lookupPredicate
          userQuery_predicate
          userQuery_predicate_version
          schema of
        Nothing -> throwIO $ Thrift.BadQuery $ mconcat
          [ "unknown predicate: "
          , userQuery_predicate
          , maybe
              ""
              (\ver -> "." <> Text.pack (show ver))
              userQuery_predicate_version ]
        Just details -> return details

    let
      opts = fromMaybe def userQuery_options
      stored = Thrift.userQueryOptions_store_derived_facts opts
      pred = predicateRef_name predicateRef <> "." <>
          showt (predicateRef_version predicateRef)

      mkResults pids derived qResults@QueryResults{..} = do
        userCont <- case queryResultsCont of
          Nothing -> return Nothing
          Just bs -> do
            nextId <- firstFreeId derived
            return $ Just $ mkUserQueryCont (Left pids) bs nextId

        stats <- getStats qResults
        when (isJust userCont) $
          addStatValueType "glean.query.truncated" 1 Stats.Sum

        -- If we're storing derived facts, queue them for writing and
        -- return the handle.
        maybeWriteHandle <-
          if stored
            then writeDerivedFacts env repo derived
            else return Nothing
        return Results
          { resFacts = Vector.toList queryResultsFacts
          , resPredicate = Just details
          , resNestedFacts = mkNestedFacts queryResultsNestedFacts
          , resCont = userCont
          , resStats = stats
          , resDiags = []
          , resWriteHandle = maybeWriteHandle
          , resFactsSearched = queryResultsStats
          , resType = Just pred
          , resCompileTime = Nothing
          , resBytecodeSize = Nothing
          , resExecutionTime = Just queryResultsElapsedNs
          }

      limits0 = mkQueryRuntimeOptions opts config
      getLimits pids
          | Thrift.userQueryOptions_recursive opts = limits0
          | otherwise = limits0 { queryDepth = ExpandPartial pids }

    results <- case Thrift.userQueryOptions_continuation opts of
      Just ucont@Thrift.UserQueryCont{..} -> do
        nextId <- if userQueryCont_nextId > 0
          then return (Fid userQueryCont_nextId)
          else firstFreeId lookup
        derived <- FactSet.new nextId
        let stack = stacked lookup derived
            pids = Set.fromList $ Pid <$> userQueryCont_pids
            limits = getLimits pids
        qResults <- restartCompiled schemaInventory stack (Just predicatePid)
          limits (Thrift.userQueryCont_continuation ucont)
        mkResults pids derived qResults

      Nothing -> do
        let
          oops = throwIO . Thrift.BadQuery . ("invalid JSON query: " <>)
          get_facts ids = userQueryFactsImpl schema config lookup def
            { Thrift.userQueryFacts_facts =
              [ def { Thrift.factQuery_id = i
                    , Thrift.factQuery_predicate_version =
                        Just (predicateRef_version predicateRef) }
              | Fid i <- ids ]
            , Thrift.userQueryFacts_options = Just opts }

          -- Handle queries for a key pattern. Queries for a specific id
          -- will be handed off to userQueryFactsImpl.
          userQueryTerm query = do
            -- 3. Do the nested queries
            nextId <- firstFreeId lookup
            let pids = getExpandPids query
                limits = getLimits pids
            gens <- case toGenerators schema stored details query of
              Left err -> throwIO $ Thrift.BadQuery err
              Right r -> return r
            derived <- FactSet.new nextId
            let stack = stacked lookup derived
            qResults <- bracket (compileQuery gens) (release . compiledQuerySub)
              $ \sub -> executeCompiled schemaInventory stack sub limits
            mkResults pids derived qResults

        -- 1. Decode the JSON
        pat <- case Aeson.eitherDecode (LB.fromStrict userQuery_query) of
          Left err -> throwIO $ Thrift.BadQuery $
            "query is not valid JSON: " <> Text.pack err
          Right pat -> return pat

        -- 2. Parse the JSON query
        case runExcept $ parseQuery schema opts details pat of
          Left err -> throwIO $ Thrift.BadQuery (Text.pack err)

          -- The only two options for userQueryTerm
          Right (MatchTerm (NestedPred _ Nothing (Just term))) ->
            userQueryTerm term
          Right (MatchTerm (NestedPred _ Nothing Nothing)) ->
            userQueryTerm (Ref Wildcard)

          -- No continuation
          Right (MatchTerm (NestedRef id)) -> do
            res@Results{resFacts = (fid,fact):_} <- get_facts [id]
            return $! res{resFacts = [(fid,fact)], resType = Just pred}
          Right (MatchTerm (NestedPred _ (Just ids) Nothing)) -> do
            res <- get_facts ids
            return res { resType = Just pred }
          Right (MatchTerm (NestedPred _ (Just _) (Just _))) ->
            oops "impossible NestedPred found"
          Right (MatchTerm NestedSum{}) -> oops "impossible NestedSum found"
          Right (MatchTerm NestedArray{}) -> oops "impossible NestedArray found"
          Right Variable -> oops "impossible Variable found"
          Right Wildcard -> oops "impossible Wildcard found"
          Right PrefixVariable{} -> oops "impossible PrefixVariable found"
          Right PrefixWildcard{} -> oops "impossible PrefixWildcard found"

    return $ if Thrift.userQueryOptions_omit_results opts
       then withoutFacts results
       else results



compileAngleQuery :: DbSchema -> ByteString -> Bool -> IO CodegenQuery
compileAngleQuery dbSchema source stored = do
  parsed <- checkBadQuery Text.pack $ Angle.parseQuery source
  vlog 2 $ "parsed query: " <> show (pretty parsed)

  typechecked <- checkBadQuery id $ runExcept $
    typecheck dbSchema latestAngleVersion (Qualified dbSchema) parsed
  vlog 2 $ "typechecked query: " <> show (pretty (qiQuery typechecked))

  flattened <- checkBadQuery id $ runExcept $
    flatten dbSchema latestAngleVersion stored typechecked
  vlog 2 $ "flattened query: " <> show (pretty (qiQuery flattened))

  optimised <- checkBadQuery id $ runExcept $ optimise flattened
  vlog 2 $ "optimised query: " <> show (pretty (qiQuery optimised))

  checkBadQuery id $ runExcept $ reorder dbSchema optimised
  -- no need to vlog, compileQuery will vlog it later
  where
  checkBadQuery :: (err -> Text) -> Either err a -> IO a
  checkBadQuery txt act = case act of
    Left str -> throwIO $ Thrift.BadQuery $ txt str
    Right a -> return a


-- | Put the nested facts in the right form for the conversion to
-- (JSON, Compact, Bin).
mkNestedFacts :: Vector (Fid, Thrift.Fact) -> IntMap Thrift.Fact
mkNestedFacts facts =
  IntMap.fromList
    [ (fromIntegral (fromFid id), f)
    | (id,f) <- Vector.toList facts ]

withoutFacts :: Results stats fact -> Results stats fact
withoutFacts results = results
    { resFacts = mempty
    , resNestedFacts = mempty
    }

mkQueryRuntimeOptions
  :: Thrift.UserQueryOptions
  -> ServerConfig.Config
  -> QueryRuntimeOptions
mkQueryRuntimeOptions Thrift.UserQueryOptions{..} ServerConfig.Config{..} =
  QueryRuntimeOptions
    { queryMaxResults = userQueryOptions_max_results
        <|> config_default_max_results -- from ServerConfig
    , queryMaxBytes = userQueryOptions_max_bytes
        <|> config_default_max_bytes -- from ServerConfig
    , queryMaxTimeMs = userQueryOptions_max_time_ms
        <|> config_default_max_time_ms -- from ServerConfig
    , queryWantStats = userQueryOptions_collect_facts_searched
    , queryDepth = if userQueryOptions_recursive
        then ExpandRecursive else ResultsOnly
    }


writeDerivedFacts :: Env -> Thrift.Repo -> FactSet -> IO (Maybe Thrift.Handle)
writeDerivedFacts env repo derived = do
  batch <- FactSet.serialize derived
  if Thrift.batch_count batch == 0
    then return Nothing
    else do
      resp <- enqueueBatch env Thrift.ComputedBatch
        { computedBatch_repo = repo
        , computedBatch_remember = True
        , computedBatch_batch = batch }
      case resp of
        Thrift.SendResponse_handle handle -> return (Just handle)
        Thrift.SendResponse_retry (Thrift.BatchRetry s) ->
          throwIO $ Thrift.Retry s
        Thrift.SendResponse_error e ->
          throwIO $ Thrift.Exception (Text.pack (show e))


-- | Check that the predicate declared in the UserQuery request
-- matches the actual type of the query as determined by the
-- typechecker. This adds some rudimentary type safety: the predicate
-- declared in the UserQuery is typically the one expected at the call
-- site.
checkPredicatesMatch
  :: DbSchema
  -> PredicateDetails
  -> Text
  -> Maybe Int32
  -> IO ()
checkPredicatesMatch dbSchema details predicate maybeVer = do
  case lookupPredicate predicate maybeVer dbSchema of
    Nothing -> throwIO $ Thrift.BadQuery $
      "unknown predicate: " <> showPred predicate maybeVer
    Just details' ->
      if predicatePid details == predicatePid details'
        then return ()
        else throwIO $ Thrift.BadQuery $ mconcat
          [ "predicate " <> showPred predicate maybeVer <>
            " does not match type of query: "
          <> predicateRef_name (predicateRef details) <> "."
          <> showt (predicateRef_version (predicateRef details)) ]

showPred :: Text -> Maybe Int32 -> Text
showPred predicate maybeVer =
  predicate <> maybe "" (\ver -> "." <> Text.pack (show ver)) maybeVer


data Stats = Stats
  { statFactCount :: {-# UNPACK #-} !Int
  , statResultCount :: {-# UNPACK #-} !Int
  }

getStats :: QueryResults -> IO Stats
getStats QueryResults{..} = do
  let
    results =
      Vector.length queryResultsFacts

    facts =
      Vector.length queryResultsFacts +
      Vector.length queryResultsNestedFacts

  addStatValueType "glean.query.facts" facts Stats.Sum
  addStatValueType "glean.query.results" results Stats.Sum
  return $ Stats
    { statFactCount = facts
    , statResultCount  = results
    }

withStats :: IO (Results Stats fact) -> IO (Results Thrift.UserQueryStats fact)
withStats io = do
  (secs, bytes, res) <- timeIt io
  let stats = Thrift.UserQueryStats
        { Thrift.userQueryStats_num_facts =
            fromIntegral $ statFactCount $ resStats res
        , Thrift.userQueryStats_result_count =
            fromIntegral $ statResultCount $ resStats res
        , Thrift.userQueryStats_elapsed_ns = truncate (secs * 1000000000)
        , Thrift.userQueryStats_allocated_bytes = fromIntegral bytes
        , Thrift.userQueryStats_facts_searched = resFactsSearched res
        , Thrift.userQueryStats_bytecode_size =
            fmap fromIntegral $ resBytecodeSize res
        , Thrift.userQueryStats_compile_time_ns =
            fmap (round . (* 1000000000)) (resCompileTime res )
        , Thrift.userQueryStats_execute_time_ns =
            fromIntegral <$> resExecutionTime res
        }
  return res{ resStats = stats }


-- | Parse a JSON query into the internal representation
parseQuery
  :: DbSchema
  -> Thrift.UserQueryOptions
  -> PredicateDetails
  -> Aeson.Value
  -> Except String (Match (Nested Fid))
parseQuery dbSchema Thrift.UserQueryOptions{..} details val =
  jsonToPredMatch details val
  where
  jsonToPredMatch
    :: PredicateDetails
    -> Aeson.Value
    -> Except String (Match (Nested Fid))
  jsonToPredMatch details@PredicateDetails{..} val = do
    let
      badQuery :: Except String a
      badQuery = queryError (Predicate (PidRef predicatePid predicateRef)) val
    case val of
      Aeson.Object obj -> do
        case sortOn fst $ HashMap.toList obj of
          -- { } means "fetch the fact" (only if the user wrote JSON directly)
          [] -> return (MatchTerm (NestedPred details Nothing Nothing))
          -- { "get" = { } } means "fetch the fact"
          [("get", _)] ->
            return (MatchTerm (NestedPred details Nothing Nothing))
          -- { id = N } means "match fact N" (and fetch it, if this is the root
          -- of the query, or if we're expanding recursively)
          [("id", n)] | Just id <- getId n ->
              return (MatchTerm (NestedRef id))
          [("ids", Aeson.Array ns)] | Just ids <- getIds ns ->
              return (MatchTerm (NestedPred details (Just ids) Nothing))
              -- TODO: we should probably have MatchRefs [Fid] for
              -- this case, but since it isn't needed for paging
              -- queries I'll leave it for later when we expose "ids" to
              -- the user.
          [("get", _), ("id", n)] | Just id <- getId n ->
              return (MatchTerm (NestedPred details (Just [id]) Nothing))
          [("id", n), ("key", key)] | Just id <- getId n ->
            MatchTerm . NestedPred details (Just [id]) . Just <$>
              jsonToValMatch predicateKeyType key
          [("get", _), ("ids", Aeson.Array ns)] | Just ids <- getIds ns ->
              return (MatchTerm (NestedPred details (Just ids) Nothing))
          [("ids", Aeson.Array ns), ("key", key)] | Just ids <- getIds ns ->
            MatchTerm . NestedPred details (Just ids) . Just <$>
              jsonToValMatch predicateKeyType key
          -- { key = T } means "match fact against T and fetch it"
          [("key", key)] ->
            MatchTerm . NestedPred details Nothing . Just <$>
              jsonToValMatch predicateKeyType key
          _ -> badQuery
      _ -> badQuery

  getId :: Aeson.Value -> Maybe Fid
  getId (Aeson.Number n)
    | Just id <- toBoundedInteger n, id /= 0 = Just (Fid id)
  getId _ = Nothing

  getIds :: Aeson.Array -> Maybe [Fid]
  getIds ns = mapM getId (Vector.toList ns)

  jsonToValMatch
    :: Type
    -> Aeson.Value
    -> Except String (Term (Match (Nested Fid)))
  jsonToValMatch typ val = case (typ,val) of
    (Nat, Aeson.Number n) | Just i <- toBoundedInteger n ->
      return (RTS.Nat i)
    (Byte, Aeson.Number n) | Just i <- toBoundedInteger n ->
      return (RTS.Byte i)
    (String, Aeson.String s) -> return $ RTS.String $ Text.encodeUtf8 s
    (String, Aeson.Object obj)
      | [("prefix", Aeson.String s)] <- HashMap.toList obj ->
         return (Ref (PrefixWildcard (Text.encodeUtf8 s)))
    (Array Byte, Aeson.String s)
      | userQueryOptions_no_base64_binary ->
        return (ByteArray (Text.encodeUtf8 s))
      | otherwise ->
        return (ByteArray (decodeBase64 (Text.encodeUtf8 s)))
    (Array ty, Aeson.Object obj)
      | Just val <- HashMap.lookup "every" obj -> do
        query <- jsonToValMatch ty val
        if refutableNested query
          then throwError "array query with \"every\" must be irrefutable"
          else return (Ref (MatchTerm (NestedArray query)))
      | Just (Aeson.Array vec) <- HashMap.lookup "exact" obj ->
        RTS.Array <$> mapM (jsonToValMatch ty) (Vector.toList vec)
    (Record fields, Aeson.Object obj)
      -- ensure that all the fields mentioned in the query are valid
      | all (`elem` map fieldDefName fields) (HashMap.keys obj) -> do
      let
        doField (Angle.FieldDef name ty)
          | Just val <- HashMap.lookup name obj =
            jsonToValMatch ty val
        doField _ = return (Ref Wildcard) -- missing field is a wildcard
      Tuple <$> mapM doField fields
    (Sum fields, Aeson.Object obj) -> matchSum fields obj
    (NamedType (ExpandedType _ ty), val) -> jsonToValMatch ty val
    (Predicate (PidRef pid ref), val) ->
      case lookupPid pid dbSchema of
        Nothing -> throwError $ "unknown predicate " ++ show (pretty ref)
        Just deets -> Ref <$> jsonToPredMatch deets val
    (Enumerated vals, Aeson.Number n)
      | Just i <- toBoundedInteger n, fromIntegral i < length vals ->
        return (Alt i (Tuple []))
    (Maybe ty, val) ->
      jsonToValMatch (lowerMaybe ty) val
    (Boolean{}, Aeson.Bool False) -> return (Alt 0 (Tuple []))
    (Boolean{}, Aeson.Bool True) -> return (Alt 1 (Tuple []))
    -- null can be used anywhere to indicate a wildcard. This is only
    -- possible when sending raw JSON queries, not when using the
    -- Thrift query types, but it enables a bit more flexibility. For
    -- example, if we have maybe(nat), we can use { "just" : null } to
    -- match the just case but without mathcing on the inner nat. To
    -- enable this kind of query with the Thrift query types would
    -- require adding an extra layer of wrapper types for sum and
    -- maybe fields, which would add a lot of clutter.
    (_any, Aeson.Null) -> return (Ref Wildcard)
    _otherwise -> queryError typ val

  queryError :: Type -> Aeson.Value -> Except String a
  queryError typ val = throwError $ show $ vcat
    [ "Error in query. Expecting a query for the " <> thing <> ":"
    , indent 2 (pretty typ)
    , "which should be of the form:"
    , indent 2 (expecting typ)
    , "but got:"
    , indent 2 (pretty (Text.decodeUtf8 (LB.toStrict (Aeson.encode val))))
    ]
    where
      thing
        | Predicate{} <- typ = "predicate"
        | otherwise = "type"

      allowed fields = hcat $
        punctuate ", " [ dquotes (pretty f) | Angle.FieldDef f _ <- fields ]

      expecting :: Type -> Doc ann
      expecting Nat{} = "number"
      expecting Byte{} = "number"
      expecting String{} = "string"
      expecting (Array Byte{}) = "string"
      expecting Array{} = vcat
        [ "{ \"exact\": [...] } or"
        , "{ \"every\": ... }" ]
      expecting (Record fields) =
        "{..} (allowed fields: " <> allowed fields <> ")"
      expecting (Sum fields) =
        "{\"any\": true|false, ..} (allowed fields: " <> allowed fields <> ")"
      expecting (Enumerated vals) =
        "number (< " <> pretty (length vals) <> ")"
      expecting (Maybe ty) =
        expecting (lowerMaybe ty)
      expecting Boolean{} = "true or false"
      expecting Predicate{} = vcat
        [ "{\"get\": {}} or"
        , "{\"id\": N} or"
        , "{\"key\": ...}" ]
      expecting _ = error "expecting"

  matchSum
    :: [FieldDef]
    -> Aeson.Object
    -> Except String (Term (Match (Nested Fid)))
  matchSum fields obj
    -- when "any":True, we ignore unknown fields, to allow
    -- backwards-compatible queries.
    | Just (Aeson.Bool True) <-  HashMap.lookup "any" obj = do
      alts <- parseAlts
      if any (any refutableNested) alts
        then
          -- If there are refutable alternatives, we have to turn
          -- this into a SumMatchThese and replace the missing alts
          -- with wildcards. There's a danger that this could make
          -- the query more expensive than the user expected, but
          -- that's a problem we'll need to deal with elsewhere.
          let
              missingToWildcard Nothing = Just (Ref Wildcard)
              missingToWildcard (Just t) = Just t
          in
          return $ Ref $ MatchTerm $ NestedSum SumMatchThese $
            map missingToWildcard alts
        else
          return $ Ref $ MatchTerm $ NestedSum SumMatchAny alts

    -- when "any":False, all fields must be present in the schema
    | all (`elem` ("any" : map fieldDefName fields)) (HashMap.keys obj) = do
      RTS.Ref . MatchTerm . NestedSum SumMatchThese <$> parseAlts
    | otherwise = queryError (Sum fields) (Aeson.Object obj)
    where
    parseAlts = forM fields $ \(Angle.FieldDef name ty) ->
      case HashMap.lookup name obj of
        Just val -> Just <$> jsonToValMatch ty val
        Nothing -> return Nothing

mkUserQueryCont
  :: Either (Set Pid) Type
  -> ByteString
  -> Fid
  -> Thrift.UserQueryCont
mkUserQueryCont contInfo cont nextId = hashUserQueryCont $ Thrift.UserQueryCont
  { userQueryCont_continuation = cont
  , userQueryCont_nextId = fromFid nextId
  , userQueryCont_version = fromIntegral Bytecode.version
  , userQueryCont_hash = 0
  , userQueryCont_returnType = returnType
  , userQueryCont_pids = pids
  }
  where
    returnType = case contInfo of
      Right ty -> Just $ serializeType ty
      Left _ -> Nothing
    pids = case contInfo of
      Right _ -> []
      Left ps -> map fromPid $ Set.elems ps

checkUserQueryCont :: Thrift.UserQueryCont -> IO ()
checkUserQueryCont cont@Thrift.UserQueryCont{..} = do
  when
    (userQueryCont_version < fromIntegral Bytecode.lowestSupportedVersion
      || userQueryCont_version > fromIntegral Bytecode.version)
    $ throwIO $ Thrift.BadQuery $
        "unsupported query continuation version "
          <> Text.pack (show userQueryCont_version)
  when
    (userQueryCont_hash /= Thrift.userQueryCont_hash (hashUserQueryCont cont))
    $ throwIO $ Thrift.BadQuery "invalid query continuation hash"

hashUserQueryCont :: Thrift.UserQueryCont -> Thrift.UserQueryCont
hashUserQueryCont Thrift.UserQueryCont{..} = Thrift.UserQueryCont
  -- NOTE: The hash is really just a checksum to detect accidental corruption
  -- so a 64 bit non-crypto hash should work fine. Hashable currently uses a
  -- slightly broken (https://github.com/tibbe/hashable/issues/190) version of
  -- FNV-1 but should still be good enough.
  { userQueryCont_hash = fromIntegral $ hash
      ( userQueryCont_continuation
      , userQueryCont_nextId
      , userQueryCont_version
      , userQueryCont_returnType
      )
  , ..
  }

serializeType :: Type -> ByteString
serializeType = Text.encodeUtf8 . renderStrict . layoutCompact . pretty

compileType :: DbSchema -> ByteString -> IO Type
compileType schema src = do
  parsed <- checkParsed $ Angle.parseType src
  let scope = toScope (Qualified schema)
  resolved <- checkResolved $ resolveType latestAngleVersion scope parsed
  checkConverted $ dbSchemaRtsType schema resolved
  where
    checkParsed = either (badQuery . Text.pack) return
    checkResolved = either badQuery return . runExcept
    checkConverted = maybe (badQuery "type not present in schema") return

    badQuery :: Text -> IO a
    badQuery err = throwIO $ Thrift.BadQuery $ Text.unlines
      ["unable to compile type: ", err]
