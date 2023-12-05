{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Write.JSON
  ( buildJsonBatch
  , syncWriteJsonBatch
  , writeJsonBatch
  , writeJsonBatchByteString
  ) where

import Control.Exception hiding (catch, throw)
import Control.Monad.Reader
import Control.Monad.Catch
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import Data.Coerce (coerce)
import Data.Default
import Data.IORef
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Storable as Vector
import Foreign hiding (void)
import Foreign.C.Types (CSize)
import TextShow hiding (Builder)

import Thrift.Protocol.JSON.Base64
import Util.FFI (invoke)

import Glean.Database.Open
import Glean.Database.Write.Batch
import Glean.Database.Schema.Types
import Glean.Database.Types as Database
import Glean.Display
import qualified Glean.FFI as FFI
import Glean.RTS as RTS
import Glean.RTS.Builder
import Glean.RTS.Constants
import qualified Glean.RTS.Foreign.JSON as J
import Glean.RTS.Foreign.Subst as Subst (empty)
import Glean.RTS.Types
import Glean.Angle.Types hiding (Type)
import Glean.Schema.Util
import Glean.Types as Thrift hiding (Value, Nat, Byte)


-- just an empty Subst for now. Later we might implement
-- returning substitutions from JSON writes.
emptySubst :: IO Batch -> IO WriteContent
emptySubst batchIO = do
  wc <- writeContentFromBatch <$> batchIO
  return wc{writeSubst = const Subst.empty}

syncWriteJsonBatch
  :: Env
  -> Repo
  -> [Thrift.JsonFactBatch]
  -> Maybe Thrift.SendJsonBatchOptions
  -> IO ()
syncWriteJsonBatch env repo batches opts = do
  let batch =
        Thrift.SendJsonBatch
          { Thrift.sendJsonBatch_batches = batches
          , Thrift.sendJsonBatch_options = opts
          , Thrift.sendJsonBatch_remember = False }
  content <- writeJsonBatch env repo batch
  void $ syncWriteContentDatabase env repo content

writeJsonBatch
  :: Env
  -> Repo
  -> SendJsonBatch
  -> IO WriteContent
writeJsonBatch env repo SendJsonBatch{..} = do
  dbSchema <- withOpenDatabase env repo (return . Database.odbSchema)
  emptySubst $
    buildJsonBatch dbSchema sendJsonBatch_options sendJsonBatch_batches

buildJsonBatch
  :: DbSchema
  -> Maybe SendJsonBatchOptions
  -> [JsonFactBatch]
  -> IO Batch
buildJsonBatch dbSchema opts batches =
  withFactBuilder $ \builders ->
    forM_ batches $ \JsonFactBatch{..} ->
      writeFacts dbSchema (fromMaybe def opts) builders
        jsonFactBatch_predicate jsonFactBatch_facts jsonFactBatch_unit

writeJsonBatchByteString
  :: Env
  -> Repo
  -> PredicateRef
  -> [ByteString] -- ^ facts
  -> SendJsonBatchOptions
  -> IO WriteContent
writeJsonBatchByteString env repo pred facts opts = do
  dbSchema <- withOpenDatabase env repo (return . Database.odbSchema)
  emptySubst $ withFactBuilder $ \builder ->
      writeFacts dbSchema opts builder pred facts Nothing{-TODO-}


writeFacts
  :: DbSchema
  -> SendJsonBatchOptions
  -> FactBuilder
  -> PredicateRef
  -> [ByteString]  -- ^ The facts to write, in JSON
  -> Maybe ByteString -- ^ The unit that owns the facts, if any
  -> IO ()
writeFacts dbSchema opts builder@FactBuilder{..} pred factList maybeUnit = do
  details <- predDetailsForWriting dbSchema pred
  before <- readIORef nextId
  mapM_ (writeFact dbSchema opts builder details) factList
  after <- readIORef nextId
  forM_ maybeUnit $ \unit ->
    when (after > before) $ do
      -- merge adjacent ranges
      let merge _ (Fid last : rest) | last+1 == before = Fid (after-1) : rest
          merge new old = new ++ old
      modifyIORef' owned $
        HashMap.insertWith merge unit [Fid (after-1), Fid before]

predDetailsForWriting :: DbSchema -> PredicateRef -> IO PredicateDetails
predDetailsForWriting dbSchema (PredicateRef name ver) = do
  let sourceRef = SourceRef name (Just ver)
  case lookupPredicateSourceRef sourceRef LatestSchemaAll dbSchema of
    Right info ->
      assert (predicateInStoredSchema info) $ return info
      -- it should be impossible for predicateInStoredSchema to be
      -- False because we don't update the schema of a writable DB.
    Left err ->
      throwIO $ Thrift.Exception err

writeFact
  :: DbSchema
  -> SendJsonBatchOptions
  -> FactBuilder
  -> PredicateDetails
  -> ByteString
  -> IO ()
writeFact dbSchema opts builders details str =
  J.withParsed str $ \json ->
    runReaderT (writeJsonFact dbSchema opts details json) builders


data FactBuilder = FactBuilder
  { facts :: Builder
  , nextId :: {-# UNPACK #-} !(IORef Int64)
  , idsRef :: {-# UNPACK #-} !(IORef [Fid])  -- TODO: use a Storable Vector
  , owned :: {-# UNPACK #-} !(IORef (HashMap ByteString [Fid]))
      -- maps each unit to a *reversed* list of inclusive fact ID ranges
      -- e.g. [end2,start2,end1,start1,end0,start0]
      -- reversed so that we can prepend the next one easily and merge it
      -- with the previous one.
  }

withFactBuilder :: (FactBuilder -> IO ()) -> IO Thrift.Batch
withFactBuilder action =
  withBuilder $ \facts -> do
  nextId <- newIORef firstAnonId
  idsRef <- newIORef []
  owned <- newIORef HashMap.empty
  action FactBuilder{..}
  mem <- finishBuilder facts
  ids <- readIORef idsRef
  ownerMap <- readIORef owned
  return $ Thrift.Batch
    firstAnonId
    (fromIntegral (length ids))
    mem
    (Just $ Vector.fromList $ coerce $ reverse ids)
    (fmap (Vector.fromList . coerce . reverse) ownerMap)
    mempty


type WriteFacts a = ReaderT FactBuilder IO a


writeBatchFact :: Builder -> Pid -> Builder -> CSize -> WriteFacts ()
writeBatchFact builder pid clause key_size = liftIO $
  -- TODO: avoid copy
  invoke $ glean_push_fact builder pid clause key_size

namedFact :: Fid -> Pid -> Builder -> CSize -> WriteFacts Fid
namedFact fid pid clause key_size = do
  when (fromFid fid >= firstAnonId) $
    liftIO $ throwIO $ Thrift.Exception $ "id too high: " <> showt (fromFid fid)
  FactBuilder{..} <- ask
  writeBatchFact facts pid clause key_size
  id <- liftIO $ readIORef nextId
  liftIO $ writeIORef nextId $! id+1
  liftIO $ modifyIORef' idsRef (fid:)
  return (Fid id)

anonFact :: Pid -> Builder -> CSize -> WriteFacts Fid
anonFact pid clause key_size = do
  FactBuilder{..} <- ask
  writeBatchFact facts pid clause key_size
  id <- liftIO $ readIORef nextId
  liftIO $ writeIORef nextId $! id+1
  liftIO $ modifyIORef' idsRef (Fid Thrift.iNVALID_ID :)
     -- Thrift.iNVALID_ID: this tells the fact renamer that this Id
     -- maps to itself. This is so that we avoid needing to construct
     -- a mapping with all the Ids for anonymous facts.
  return (Fid id)

--
-- | Convert a JSON value to a Term, according to the type (Type) given
-- by the schema for this predicate.
--
writeJsonFact
  :: DbSchema                           -- ^ needed for looking up typerefs
  -> Thrift.SendJsonBatchOptions        -- ^ needed for no_base64_binary
  -> PredicateDetails
  -> J.Value
  -> WriteFacts ()
writeJsonFact
    dbSchema
    Thrift.SendJsonBatchOptions{..}
    details json =
  wrapJsonContextM json $ void $ factToTerm details json
  where

  factToTerm PredicateDetails{..} json@(J.Object obj) = do
    r <- lift $ J.field obj "id"
    case r of
      Just (J.Int id)
        | J.arity obj == 1 ->
          if id == fromFid invalidFid
            then invalidFactIdError
            else return (Fid id)
        | otherwise -> fact 1 (namedFact (Fid id)) obj
      _ -> fact 0 anonFact obj
    where
      -- id_arity is 0 if we don't have an id field and 1 otherwise
      fact id_arity create obj = do
        key <- do
          r <- lift $ J.field obj "key"
          case r of
            Just key -> return key
            Nothing -> badFact json
        val <- lift $ J.field obj "value"
        when (J.arity obj /= id_arity + if isJust val then 2 else 1) $
          badFact json
        withBuilder $ \clause -> do
          jsonToTerm clause predicateKeyType key
          key_size <- liftIO $ sizeOfBuilder clause
          forM_ val $ \v ->
            jsonToTerm clause predicateValueType v
          create predicatePid clause key_size
  factToTerm _ json = badFact json

  wrapJsonContextM :: J.Value -> WriteFacts a -> WriteFacts a
  wrapJsonContextM json act =
    act `catch` \(Thrift.Exception msg) -> do
      msg' <- wrapJsonContext json (pretty msg)
      throwM $ Thrift.Exception $ Text.pack $ show msg'

  wrapJsonContext :: MonadIO m => J.Value -> Doc ann -> m (Doc ann)
  wrapJsonContext json doc = do
    enc <- liftIO $ J.encode json
    return $ vcat
      [ doc
      , "in JSON term:"
      , indent 2 (pretty (Text.decodeUtf8 enc))
      ]

  badFact :: J.Value -> WriteFacts a
  badFact json = do
    msg <- wrapJsonContext json $ vcat
      [ "Expecting a fact, which should be of the form:"
      , indent 2 "{[\"id\" : N, ] \"key\": ... [, \"value\": ...]}"
      ]
    throwM $ Thrift.Exception $ Text.pack $ show msg

  -- Defining a fact with ID 0 means "don't care"; we want this
  -- behaviour because Thrift serialization will use ID 0 when the
  -- "id" field is unspecified. However, we don't support referring to
  -- fact ID 0 (see anonFact above), so catch and report that error
  -- here rather than waiting for the typechecker.
  invalidFactIdError :: WriteFacts a
  invalidFactIdError = liftIO $ do
    enc <- J.encode json
    throwIO $ Thrift.Exception $ Text.pack $ show $ vcat
      [ "Cannot use " <> pretty (fromFid invalidFid) <> " as a fact ID:"
      , indent 2 (pretty (Text.decodeUtf8 enc))
      ]

  jsonToTerm
    :: Builder
    -> Type                               -- ^ the schema type
    -> J.Value                            -- ^ the JSON value
    -> WriteFacts ()
  jsonToTerm b typ v = case (typ, v) of
    (NatTy, J.Int n) ->
      lift $ invoke $ glean_push_value_nat b $ fromIntegral n
    (ByteTy, J.Int n) ->
      lift $ invoke $ glean_push_value_byte b $ fromIntegral n
    (StringTy, J.String (J.ByteStringRef p n)) ->
      lift $ invoke $ glean_push_value_string b (castPtr p) n
    (ArrayTy ByteTy, J.String (J.ByteStringRef p n))
      | sendJsonBatchOptions_no_base64_binary -> lift $ do
          invoke $ glean_push_value_array b n
          invoke $ glean_push_value_bytes b (castPtr p) n
      | otherwise -> lift $ do
          bytes <-
            decodeBase64 <$> BS.unsafePackCStringLen (castPtr p, fromIntegral n)
          FFI.unsafeWithBytes bytes $ \ptr len -> do
            invoke $ glean_push_value_array b len
            invoke $ glean_push_value_bytes b (castPtr ptr) len
    (ArrayTy ty, J.Array arr) -> do
      let !n = J.size arr
      lift $ invoke $ glean_push_value_array b $ fromIntegral n
      when (n > 0) $ forM_ [0 .. n-1] $ \i -> do
        x <- lift $ J.index arr i
        jsonToTerm b ty x
    (RecordTy fields, J.Object obj) -> do
      let
        doField !n (FieldDef name ty) = do
          r <- lift $ J.field obj $ Text.encodeUtf8 name
          case r of
            Just val -> do
              jsonToTerm b ty val
              return $! n+1
            Nothing -> do
              lift $ defaultValue b v (name, ty)
              return n
      n <- foldM doField 0 fields
      -- ensure that all the fields mentioned in the fact are valid
      when (J.arity obj /= n) $ termError typ v
    (SumTy fields, J.Object obj)
      | J.arity obj == 1 -> do
          let -- this is O(number of alternatives) but I don't expect this is
              -- a problem
              get !n (FieldDef name ty : rest) = do
                -- TODO: avoid the encodeUtf8 (name should really be a
                -- bytestring)
                r <- lift $ J.field obj $ Text.encodeUtf8 name
                case r of
                  Just val -> do
                    lift $ invoke $ glean_push_value_selector b n
                    jsonToTerm b ty val
                  Nothing -> get (n+1) rest
              get _ _ = termError typ v
          get 0 fields
    (NamedTy (ExpandedType _ ty), val) -> jsonToTerm b ty val
    (PredicateTy{}, J.Int n)
      | n == fromFid invalidFid -> invalidFactIdError
      | otherwise -> lift $ invoke $ glean_push_value_fact b $ Fid n
    -- allow { "id": N } for predicate refs, this allows us to accept
    -- JSON-serialized Thrift facts.
    (PredicateTy (PidRef pid ref), val) ->
      -- Facts can be nested. We know from the schema the predicate of
      -- the fact at this position.
      case lookupPid pid dbSchema of
        Nothing ->
          throwError $ "unknown predicate " ++ show (displayDefault ref)
        Just deets -> do
          fid <- factToTerm deets val
          lift $ invoke $ glean_push_value_fact b fid
    (EnumeratedTy vals, J.Int n)
      | fromIntegral n < length vals ->
        lift $ invoke $ glean_push_value_selector b $ fromIntegral n
    (MaybeTy ty, val) -> do
      lift $ invoke $ glean_push_value_selector b 1
      jsonToTerm b ty val
    (BooleanTy, J.Bool False) ->
      lift $ invoke $ glean_push_value_selector b 0
    (BooleanTy, J.Bool True) ->
      lift $ invoke $ glean_push_value_selector b 1
    _otherwise -> termError typ v

  -- Thrift might omit fields from the output if they have the
  -- default value, so we have to reconstruct the default value
  -- here.
  defaultValue :: Builder -> J.Value -> (Text, Type) -> IO ()
  defaultValue b json (fieldName, typ) = case typ of
    ByteTy -> invoke $ glean_push_value_byte b 0
    NatTy -> invoke $ glean_push_value_nat b 0
    StringTy -> invoke $ glean_push_value_string b nullPtr 0
    ArrayTy{} -> invoke $ glean_push_value_array b 0
    RecordTy fields ->
      mapM_ (defaultValue b json) [(nm, ty) | FieldDef nm ty <- fields ]
    SumTy (FieldDef _ ty : _) -> do
      invoke $ glean_push_value_selector b 0
      defaultValue b json (fieldName, ty)
    NamedTy (ExpandedType _ ty) -> defaultValue b json (fieldName, ty)
    PredicateTy{} -> do
      msg <- wrapJsonContext json $
        "Field '" <> pretty fieldName <>
        "' is missing, but it has a predicate type: " <> displayDefault typ
      throwError $ show msg
    EnumeratedTy{} -> invoke $ glean_push_value_selector b 0
    MaybeTy ty -> defaultValue b json (fieldName, lowerMaybe ty)
    BooleanTy -> invoke $ glean_push_value_selector b 0
    _otherwise -> throwError $ "internal: defaultValue: " <> show typ

  throwError :: MonadIO m => String -> m a
  throwError str = liftIO $ throwIO $ Thrift.Exception $ Text.pack str

  termError typ v = do
    msg <- wrapJsonContext v $ vcat
      [ "Error in fact. Expecting an expression of type:"
      , indent 2 (displayDefault typ)
      , "which should be of the form:"
      , indent 2 (expecting typ)
      ]
    throwError $ show msg

  allowed fields = hcat $
    punctuate ", " [ dquotes (pretty f) | FieldDef f _ <- fields ]

  expecting :: Type -> Doc ann
  expecting NatTy = "number"
  expecting ByteTy = "number"
  expecting StringTy = "string"
  expecting (ArrayTy ByteTy) = "string"
  expecting ArrayTy{} = "[..]"
  expecting (RecordTy fields) =
    "{..} (allowed fields: " <> allowed fields <> ")"
  expecting (SumTy fields) =
    "{\"field\" : value} (allowed fields: " <> allowed fields <> ")"
  expecting (EnumeratedTy vals) =
    "number (< " <> pretty (length vals) <> ")"
  expecting (MaybeTy ty) =
    expecting (lowerMaybe ty)
  expecting BooleanTy = "true or false"
  expecting PredicateTy{} = "N, or {\"id\": N }, where N is a fact ID"
  expecting _ = error "expecting"
