{-# LANGUAGE ApplicativeDo #-}
module Glean.LSP (main) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Aeson as Aeson
import Data.Default
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.LSP.Server as LSP
import Language.LSP.Server (LspM)
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import Options.Applicative
import Thrift.Protocol
import UnliftIO.Exception
import UnliftIO.IORef
import Util.Log.Text
import Util.Timing

-- use Glass as a library directly
import qualified Glean.Glass.Env as Glass
import qualified Glean.Glass.Main as Glass
import qualified Glean.Glass.Options as Glass
import qualified Glean.Glass.Tracing as Glass
import qualified Glean.Glass.Types as Glass
import qualified Glean.Glass.Handler.Documents as Glass.Handler
import qualified Glean.Glass.Handler.Symbols as Glass.Handler

import Data.Path as Path

{- TODO / ideas
  - concurrent requests and cancellation
  - go to decl / go to impl / go to type def?
  - documentSymbols:
    - make hierarchical
    - maybe filter out type parameters?
  - call hierarchy
  - update index after edits
    1. make it possible to re-index on the side and create a new DB,
       using a separate glean-server. Need to flush the cache if we
       detect that the data has changed.
    2. Trigger re-indexing from the UI somehow, with a progress update
    3. Support full re-index & in-memory incremental index for changed files only?
  - can we show errors somehow?
  - maybe use tree-sitter to get uptodate documentSymbols and
    merge those with the real symbols from Glass?
  - implement setTrace?
-}

-- -----------------------------------------------------------------------------
-- Command-line options

-- | Inherit CLI options from Glass
data LspOptions = LspOptions {
    glass :: Glass.Config Glass.GlassTrace
  }

options :: ParserInfo LspOptions
options = info (helper <*> (LspOptions <$> Glass.configParser)) fullDesc

-- -----------------------------------------------------------------------------
-- Config

-- | Config from the LSP client. e.g. for VS Code, @<workspace>/.vscode/settings.json@:
--
-- > {
-- >   "glean-lsp": {
-- >     "repo": "stackage"
-- >   },
-- >   ...
-- > }
--
data LspConfig = LspConfig {
    repo :: Glass.RepoName
  }
  deriving (Show)

instance FromJSON LspConfig where
  parseJSON = withObject "LspConfig" $ \v -> LspConfig
    <$> fmap Glass.RepoName (v .: "repo")

instance Default LspConfig where
  def = LspConfig {
    repo = Glass.RepoName "stackage"
  }

-- -----------------------------------------------------------------------------
-- Monad

-- | Environment of the running LSP server after initialisation
data LspEnv = LspEnv {
    options :: LspOptions,
    wsRoot :: AbsPath,
    glass :: Glass.Env,
    -- TODO: use a better symbol cache so that if two threads simultaneously
    -- try to fetch symbols for a file we should only make a single Glass request.
    symbolCache :: IORef (HashMap RelPath Glass.DocumentSymbolIndex)
  }

newtype GleanLspM a =
  GleanLspM { unGleanLspM :: ReaderT (IORef (Maybe LspEnv)) (LspM LspConfig) a }
  deriving (Monad, Applicative, Functor, MonadIO, MonadThrow, MonadUnliftIO)

deriving instance LSP.MonadLsp LspConfig GleanLspM

runGleanLspM :: IORef (Maybe LspEnv) -> GleanLspM a -> LspM LspConfig a
runGleanLspM envRef = flip runReaderT envRef . (.unGleanLspM)

newtype GleanLspException = GleanLspException Text
  deriving Show

instance Exception GleanLspException

getGleanLspEnv :: GleanLspM LspEnv
getGleanLspEnv = GleanLspM $ do
  ref <- ask
  r <- readIORef ref
  case r of
    Nothing -> throwIO $ GleanLspException "not initialized"
    Just env -> return env

-- -----------------------------------------------------------------------------
-- Setup & initialisation

initServer ::
  Glass.Env ->
  LspOptions ->
  IORef (Maybe LspEnv) ->
  LSP.LanguageContextEnv LspConfig ->
  LSP.TMessage 'LSP.Method_Initialize ->
  IO (Either (LSP.TResponseError LSP.Method_Initialize) (LSP.LanguageContextEnv LspConfig))
initServer glass options envRef serverConfig _msg = do
  runExceptT $ do
    wsRoot <- ExceptT $ LSP.runLspT serverConfig getWsRoot
    wsRoot <- filePathToAbs wsRoot
    symbolCache <- newIORef HashMap.empty
    writeIORef envRef (Just LspEnv { options, glass, wsRoot, symbolCache })
    liftIO $ logInfo $ "wsRoot: " <> Text.pack (Path.toFilePath wsRoot)
    pure serverConfig
  where
  getWsRoot :: LSP.LspM config (Either (LSP.TResponseError LSP.Method_Initialize) FilePath)
  getWsRoot = do
    mRootPath <- LSP.getRootPath
    pure $ case mRootPath of
      Nothing -> Left $ LSP.TResponseError (LSP.InR LSP.ErrorCodes_InvalidRequest)
        "No root workspace was found" Nothing
      Just p -> Right p

serverDef
  :: Glass.Env
  -> LspOptions
  -> IO (LSP.ServerDefinition LspConfig)
serverDef glass options = do
  envRef <- newIORef Nothing
  let
    mapReq ::
      forall (a :: LSP.Method LSP.ClientToServer LSP.Request).
      LSP.Handler GleanLspM a ->
      LSP.Handler (LspM LspConfig) a
    mapReq f = \msg responseCont -> runGleanLspM envRef $
      f msg (\resp -> GleanLspM (lift (responseCont resp)))

    mapNot ::
      forall (a :: LSP.Method LSP.ClientToServer LSP.Notification).
      LSP.Handler GleanLspM a ->
      LSP.Handler (LspM LspConfig) a
    mapNot f = \msg -> runGleanLspM envRef $ f msg

  pure
    LSP.ServerDefinition
      { onConfigChange = \_conf -> runGleanLspM envRef $ do
          liftIO $ logInfo "config change"
          flushSymbolCache
      , configSection = "glean-lsp"
      , parseConfig = \_conf value ->
          case fromJSON value of
            Error err -> Left (Text.pack err)
            Aeson.Success conf -> Right conf
      , doInitialize = initServer glass options envRef
      , -- TODO: Do handlers need to inspect clientCapabilities?
        staticHandlers = \_clientCapabilities ->
          LSP.mapHandlers mapReq mapNot $
            mconcat [
                  handleInitialized
                , handleChangeConfiguration
                , handleTextDocumentHoverRequest
                , handleDefinitionRequest
                -- , handleTypeDefinitionRequest
                -- , handleImplementationRequest
                , handleReferencesRequest
                -- , handleRenameRequest
                -- , handlePrepareRenameRequest
                -- , handleCancelNotification
                , handleDidOpen
                -- , handleDidChange
                -- , handleDidSave
                , handleDidClose
                , handleWorkspaceSymbol
                , handleSetTrace
                -- , handleCodeAction
                -- , handleResolveCodeAction
                , handleDocumentSymbols
                -- , handleCompletion
                -- , handleCompletionItemResolve
                ]
      , interpretHandler = \env -> LSP.Iso (LSP.runLspT env) liftIO
      , options = lspOptions
      , defaultConfig = def
      }

lspOptions :: LSP.Options
lspOptions =
  LSP.defaultOptions
    { LSP.optTextDocumentSync =
        Just
          LSP.TextDocumentSyncOptions
            { LSP._openClose = Just True
            , LSP._change = Just LSP.TextDocumentSyncKind_Incremental
            , LSP._willSave = Just False
            , LSP._willSaveWaitUntil = Just False
            , LSP._save =
                Just $
                  LSP.InR $
                    LSP.SaveOptions
                      { LSP._includeText = Just False
                      }
            }
    , LSP.optCompletionTriggerCharacters = Just ['.']
    }

main :: IO ()
main = do
  opts <- execParser options
  Glass.withEnv opts.glass Nothing $ \glass -> do
    server <- serverDef glass opts
    void $ LSP.runServer server

-- -----------------------------------------------------------------------------
-- Handlers

handleChangeConfiguration :: LSP.Handlers GleanLspM
handleChangeConfiguration =
  LSP.notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $
    pure $ pure ()

handleInitialized :: LSP.Handlers GleanLspM
handleInitialized =
  LSP.notificationHandler LSP.SMethod_Initialized $
    pure $ pure ()

handleDidOpen :: LSP.Handlers GleanLspM
handleDidOpen =
  LSP.notificationHandler LSP.SMethod_TextDocumentDidOpen $ \message -> do
    liftIO $ logInfo $ "did open: " <> message._params._textDocument._uri.getUri

handleDidClose :: LSP.Handlers GleanLspM
handleDidClose =
  LSP.notificationHandler LSP.SMethod_TextDocumentDidClose $ \req -> do
    let uri = req._params._textDocument._uri
    liftIO $ logInfo $ "did close: " <> uri.getUri
    path <- uriToAbsPath uri
    removeCachedSymbols path

handleDocumentSymbols :: LSP.Handlers GleanLspM
handleDocumentSymbols =
  LSP.requestHandler LSP.SMethod_TextDocumentDocumentSymbol $ \req res ->
    logTimed ("documentSymbols: " <> req._params._textDocument._uri.getUri) $ do
      let params = req._params
      path <- uriToAbsPath params._textDocument._uri
      syms <- getDocumentSymbols path
      liftIO $ logInfo $ "symbols: " <> Text.pack (show (length syms))
      res $ Right $ LSP.InR $ LSP.InL syms

handleDefinitionRequest :: LSP.Handlers GleanLspM
handleDefinitionRequest =
  LSP.requestHandler LSP.SMethod_TextDocumentDefinition $ \req resp -> do
    let params = req._params
    logTimed ("definition: " <> params._textDocument._uri.getUri <>
      Text.pack (show req._params._position)) $ do
      path <- uriToAbsPath params._textDocument._uri
      defs <- getDefinition path params._position
      resp $ Right . LSP.InR $ LSP.InL defs

handleSetTrace :: LSP.Handlers GleanLspM
handleSetTrace = LSP.notificationHandler LSP.SMethod_SetTrace $ \_ -> pure ()

handleTextDocumentHoverRequest :: LSP.Handlers GleanLspM
handleTextDocumentHoverRequest =
  LSP.requestHandler LSP.SMethod_TextDocumentHover $ \req resp -> do
    let hoverParams = req._params
    logTimed ("hover: " <> hoverParams._textDocument._uri.getUri <>
      Text.pack (show hoverParams._position)) $ do
      path <- uriToAbsPath hoverParams._textDocument._uri
      hover <- retrieveHover path hoverParams._position
      resp $ Right $ LSP.maybeToNull hover

handleReferencesRequest :: LSP.Handlers GleanLspM
handleReferencesRequest =
  LSP.requestHandler LSP.SMethod_TextDocumentReferences $ \req res -> do
    let params = req._params
    logTimed ("references: " <> params._textDocument._uri.getUri <>
      Text.pack (show params._position)) $ do
      path <- uriToAbsPath params._textDocument._uri
      refs <- findRefs path params._position
      res $ Right $ LSP.InL refs

handleWorkspaceSymbol :: LSP.Handlers GleanLspM
handleWorkspaceSymbol = LSP.requestHandler LSP.SMethod_WorkspaceSymbol $ \req res -> do
  -- https://hackage.haskell.org/package/lsp-types-1.6.0.0/docs/Language-LSP-Types.html#t:WorkspaceSymbolParams
  symbols <- symbolSearch req._params._query
  res $ Right . LSP.InL $ symbols

-- -----------------------------------------------------------------------------
-- Glean / Glass stuff

getDefinition ::
  AbsPath ->
  LSP.Position ->
  GleanLspM [LSP.DefinitionLink]
getDefinition path lineCol = do
  env <- getGleanLspEnv
  symbols <- findSymbol lineCol <$> getSymbolsCached path
  return $ fmap (LSP.DefinitionLink . locationToLocationLink) $
    refTargets env.wsRoot symbols

getSymbols ::
  RelPath ->
  Bool ->
  GleanLspM Glass.DocumentSymbolIndex
getSymbols path includeRefs = do
  env <- getGleanLspEnv
  cfg :: LspConfig <- LSP.getConfig
  let
    query = def {
            Glass.documentSymbolsRequest_repository = cfg.repo
          , Glass.documentSymbolsRequest_filepath =
              Glass.Path (Text.pack (Path.toFilePath path))
          , Glass.documentSymbolsRequest_include_refs = includeRefs
    }
    opts = def
  liftIO $ Glass.Handler.documentSymbolIndex env.glass query opts

getSymbolsCached :: AbsPath -> GleanLspM Glass.DocumentSymbolIndex
getSymbolsCached path = do
  env <- getGleanLspEnv
  cache <- readIORef env.symbolCache
  let relPath = Path.makeRelative env.wsRoot path
  case HashMap.lookup relPath cache of
    Just symbols -> return symbols
    Nothing -> do
      symbols <- getSymbols relPath True {- includeRefs -}
      atomicModifyIORef' env.symbolCache $ \old ->
        (HashMap.insert relPath symbols old, ())
      return symbols

flushSymbolCache :: GleanLspM ()
flushSymbolCache = do
  env <- getGleanLspEnv
  writeIORef env.symbolCache HashMap.empty

removeCachedSymbols :: AbsPath -> GleanLspM ()
removeCachedSymbols path = do
  env <- getGleanLspEnv
  let relPath = Path.makeRelative env.wsRoot path
  atomicModifyIORef' env.symbolCache $ \cache ->
    (HashMap.delete relPath cache, ())

findSymbol ::
  LSP.Position ->
  Glass.DocumentSymbolIndex ->
  [Glass.SymbolX]
findSymbol (LSP.Position l c) ix =
    [ sym
    | sym <- Map.findWithDefault [] (fromIntegral (l+1)) refs
    , inRange (fromIntegral (l+1)) (fromIntegral (c+1)) sym.symbolX_range
    ]
  where
  refs = Glass.documentSymbolIndex_symbols ix
  inRange line col (Glass.Range lb cb le ce) =
    line >= lb && line <= le &&
    (if line == lb then col >= cb else True) &&
    (if line == le then col <= ce else True)

retrieveHover ::
  AbsPath ->
  LSP.Position ->
  GleanLspM (Maybe LSP.Hover)
retrieveHover path position = do
  syms <- getSymbolsCached path
  case findSymbol position syms of
    (sym:_) | Just ty <- attrSymbolSignature sym.symbolX_attributes -> do
      -- TODO: pick the innermost match
      logInfo $ "hover: " <> Text.pack (show sym)
      return $ Just $ LSP.Hover
        { _range = Just $ toLspRange sym.symbolX_range
        , _contents = LSP.InL $ LSP.MarkupContent LSP.MarkupKind_PlainText ty
        }
    _ -> return Nothing

findRefs ::
  AbsPath ->
  LSP.Position ->
  GleanLspM [LSP.Location]
findRefs path pos = do
  logInfo $ "Glean.findRefs: " <> Text.pack (show pos)
  env <- getGleanLspEnv
  syms <- getSymbolsCached path
  case findSymbol pos syms of
    [] -> do
      logInfo $ "not found"
      return []
    (defn:_) -> do
      -- TODO: pick the innermost or tightest range if there are many
      logInfo $ "found: " <> Text.pack (show defn)
      ranges <- liftIO $
        Glass.Handler.findReferenceRanges env.glass defn.symbolX_sym def
      return (map (toLspLocation env.wsRoot) ranges)

getDocumentSymbols ::
  AbsPath ->
  GleanLspM [LSP.DocumentSymbol]
getDocumentSymbols path = do
  syms <- getSymbolsCached path
  return [
    LSP.DocumentSymbol {
      _name = name,
      _detail = attrSymbolSignature sym.symbolX_attributes,
      _kind = kind,
      _tags = Nothing, -- TODO?
      _deprecated = Nothing,
      _range = range,
      _selectionRange = range,
      _children = Nothing -- TODO, use symbolParent attribute?
    }
    | sym <- concat $ Map.elems syms.documentSymbolIndex_symbols
    , Nothing <- [sym.symbolX_target] -- only definitions
    , Just name <- [attrSymbolName sym.symbolX_attributes]
    , let kind = fromMaybe LSP.SymbolKind_Function (attrSymbolKind sym.symbolX_attributes)
    , let range = toLspRange sym.symbolX_range
    ]

symbolSearch ::
  Text ->
  GleanLspM [LSP.SymbolInformation]
symbolSearch query = do
  env <- getGleanLspEnv
  cfg :: LspConfig <- LSP.getConfig
  let
    req = def {
      Glass.symbolSearchRequest_name = query,
      Glass.symbolSearchRequest_repo_name = Just cfg.repo,
      Glass.symbolSearchRequest_options = def {
        Glass.symbolSearchOptions_detailedResults = True -- we need kinds
      }
    }
    opts = def
  res <- liftIO $ Glass.Handler.searchSymbol env.glass req opts
  return [
    LSP.SymbolInformation {
      _name = sym.symbolDescription_name.qualifiedName_localName.unName,
      _kind = maybe LSP.SymbolKind_Function toLspSymbolKind sym.symbolDescription_kind,
      _tags = Nothing,
      _deprecated = Nothing,
      _location = toLspLocation env.wsRoot sym.symbolDescription_sym_location,
      _containerName = Nothing
    }
    | sym <- res.symbolSearchResult_symbolDetails
    ]

-- -----------------------------------------------------------------------------
-- Data conversion Glass <-> LSP

refTargets :: AbsPath -> [Glass.SymbolX] -> [LSP.Location]
refTargets wsRoot syms =
  [ toLspLocation wsRoot rg
  | Glass.SymbolX{symbolX_target = Just rg} <- syms
  ]

toLspLocation :: AbsPath -> Glass.LocationRange -> LSP.Location
toLspLocation wsRoot locRange =
  LSP.Location uri (toLspRange locRange.locationRange_range)
  where
  path = Glass.unPath locRange.locationRange_filepath
  uri = absPathToUri (wsRoot Path.</> Path.filePathToRel (Text.unpack path))

toLspPosition :: Int64 -> Int64 -> LSP.Position
toLspPosition line col =
  LSP.Position (fromIntegral (line-1)) (fromIntegral (col-1))

toLspRange :: Glass.Range -> LSP.Range
toLspRange range = LSP.Range begin end
  where
  begin = toLspPosition range.range_lineBegin range.range_columnBegin
  end = toLspPosition range.range_lineEnd range.range_columnEnd

toLspSymbolKind :: Glass.SymbolKind -> LSP.SymbolKind
toLspSymbolKind = \case
  Glass.SymbolKind_Package -> LSP.SymbolKind_Package
  Glass.SymbolKind_Type -> LSP.SymbolKind_Class -- ?
  Glass.SymbolKind_Value -> LSP.SymbolKind_Constant -- ?
  Glass.SymbolKind_File -> LSP.SymbolKind_File
  Glass.SymbolKind_Module -> LSP.SymbolKind_Module
  Glass.SymbolKind_Namespace -> LSP.SymbolKind_Namespace
  Glass.SymbolKind_Class_ -> LSP.SymbolKind_Class
  Glass.SymbolKind_Method -> LSP.SymbolKind_Method
  Glass.SymbolKind_Property -> LSP.SymbolKind_Property
  Glass.SymbolKind_Field -> LSP.SymbolKind_Field
  Glass.SymbolKind_Constructor -> LSP.SymbolKind_Constructor
  Glass.SymbolKind_Enum -> LSP.SymbolKind_Enum
  Glass.SymbolKind_Interface -> LSP.SymbolKind_Interface
  Glass.SymbolKind_Function -> LSP.SymbolKind_Function
  Glass.SymbolKind_Variable -> LSP.SymbolKind_Variable
  Glass.SymbolKind_Constant -> LSP.SymbolKind_Constant
  Glass.SymbolKind_String -> LSP.SymbolKind_String
  Glass.SymbolKind_Number -> LSP.SymbolKind_Number
  Glass.SymbolKind_Boolean -> LSP.SymbolKind_Boolean
  Glass.SymbolKind_Array -> LSP.SymbolKind_Array
  Glass.SymbolKind_Object -> LSP.SymbolKind_Object
  Glass.SymbolKind_Key -> LSP.SymbolKind_Key
  Glass.SymbolKind_Null -> LSP.SymbolKind_Null
  Glass.SymbolKind_Enumerator -> LSP.SymbolKind_EnumMember
  Glass.SymbolKind_Struct -> LSP.SymbolKind_Struct
  Glass.SymbolKind_Event -> LSP.SymbolKind_Event
  Glass.SymbolKind_Operator -> LSP.SymbolKind_Operator
  Glass.SymbolKind_TypeParameter -> LSP.SymbolKind_TypeParameter
  Glass.SymbolKind_Union -> LSP.SymbolKind_Class -- ?
  Glass.SymbolKind_Macro -> LSP.SymbolKind_Function -- ?
  Glass.SymbolKind_Trait -> LSP.SymbolKind_Interface -- ?
  Glass.SymbolKind_Fragment -> LSP.SymbolKind_Struct -- ?
  Glass.SymbolKind_Operation -> LSP.SymbolKind_Method -- ?
  Glass.SymbolKind_Directive -> LSP.SymbolKind_Null -- ?
  _ -> LSP.SymbolKind_Null

attrSymbolSignature :: Glass.Attributes -> Maybe Text
attrSymbolSignature attrs =
  case Map.lookup "symbolSignature" attrs.unAttributes of
    Just (Glass.Attribute_aString ty) -> Just ty
    _ -> Nothing

attrSymbolName :: Glass.Attributes -> Maybe Text
attrSymbolName attrs =
  case Map.lookup "symbolName" attrs.unAttributes of
    Just (Glass.Attribute_aString nm) -> Just nm
    _ -> Nothing

attrSymbolKind :: Glass.Attributes -> Maybe LSP.SymbolKind
attrSymbolKind attrs =
  case Map.lookup "symbolKind" attrs.unAttributes of
    Just (Glass.Attribute_aInteger kind) ->
      Just (toLspSymbolKind (toThriftEnum (fromIntegral kind)))
    _ -> Nothing

-- -----------------------------------------------------------------------------
-- Utils

uriToAbsPath :: (MonadThrow m) => LSP.Uri -> m AbsPath
uriToAbsPath uri =
  case LSP.uriToFilePath uri of
    Nothing -> throwM $ GleanLspException $ "URI is not a file: " <> LSP.getUri uri
    Just path -> Path.filePathToAbsThrow path

absPathToUri :: AbsPath -> LSP.Uri
absPathToUri = LSP.filePathToUri . Path.toFilePath

locationToLocationLink :: LSP.Location -> LSP.LocationLink
locationToLocationLink LSP.Location {_uri, _range} =
  LSP.LocationLink
    { _originSelectionRange = Nothing
    , _targetUri = _uri
    , _targetRange = _range
    , _targetSelectionRange = _range
    }

logTimed :: MonadIO m => Text -> m a -> m a
logTimed msg io = do
  (t, b, a) <- timeIt io
  liftIO $ logInfo $ msg <> ": " <>
    Text.pack (showTime t) <> ", " <> Text.pack (showAllocs b)
  return a
