-- Copyright (c) Facebook, Inc. and its affiliates.

-- | run with
--
-- > ./hyperlink --port=8080

{-# LANGUAGE ApplicativeDo, TypeApplications #-}
module Hyperlink (main) where

import Data.Function
import Control.Monad
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.Pp1.Types as Pp1
import qualified Glean.Schema.Codemarkup.Types as CodeMarkup

import qualified Glean
import Glean.Impl.ConfigProvider
import Glean.Angle as Angle
import Glean.Util.ConfigProvider
import Glean.Util.Range (ByteRange(..), byteOffsetToLineCol, getLineOffsets)
import Glean.Util.XRefs (collectXRefTargets)
import Glean.Util.Some

import Util.EventBase (withEventBaseDataplane)
import Util.Log
import Util.OptParse
import Util.Timing

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Control.DeepSeq
import Control.Exception
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader as Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Binary.Builder (Builder)
import qualified Data.Binary.Builder as Builder
import Data.Char (ord)
import Data.List (sort, sortBy)
import Data.Maybe
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
import qualified Options.Applicative as O
import System.FilePath

data Config = Config
  { cfgService :: Glean.ThriftSource Glean.ClientConfig
  , cfgHttp :: Maybe Int
  , cfgHttpIface :: String
  , cfgRepoName :: Text
  , cfgRepoHash :: Maybe String
  , cfgRoot :: FilePath
  , cfgStyles :: [String]
  }

options :: O.ParserInfo Config
options = O.info (O.helper <*> parser) O.fullDesc
  where
    parser = Config
      <$> Glean.options
      <*> O.optional (O.option O.auto
            (O.long "http" <> O.metavar "PORT"))
      <*> O.strOption (O.long "http-iface" <> O.metavar "IFACE" <> O.value "*6")
      <*> textOption (O.long "repo" <> O.metavar "NAME" <> O.value "fbsource")
      <*> O.optional (O.strOption (O.long "repo-hash" <> O.metavar "HASH"))
      <*> O.strOption (O.long "root" <> O.metavar "PATH" <> O.value "")
      <*> O.many (O.option (O.maybeReader style)
            (O.long "highlight" <> O.short 'l' <> O.metavar "KIND:COLOUR"))

    style s
      | (kind,':':color) <- break (==':') s =
          Just $ '.' : kind ++ " { background-color: " ++ color ++ "; }"
      | otherwise = Nothing

data State = State
  { stateCfg  :: Config
  , stateBackend :: Some Glean.Backend
  , stateRepo :: Glean.Repo
  }

type HM = Reader.ReaderT State IO


haxl :: Glean.Haxl w a -> HM a
haxl h = do
  backend <- Reader.asks stateBackend
  repo <- Reader.asks stateRepo
  liftIO $ Glean.runHaxl backend repo h

listFiles :: State -> IO Builder.Builder
listFiles State{..} = do
  files <- Glean.runQuery_ stateBackend stateRepo $ Angle.query $
    predicate @Src.File wild
  let
    paths =
      [ path
      | Src.File { Src.file_key = Just path } <- files ]
  return $ htmlPre stateCfg Nothing $ mconcat
    [mconcat $ map Builder.fromByteString
      ["<a href=\"", path, "\">", path, "</a>\n"]
        | path <- map Text.encodeUtf8 $ sort paths]

data TargetLoc
  = TargetLine !Int
  | TargetByteOffset !Int

data Target = Target
  { targetKind :: !ByteString
  , targetPath :: !ByteString
  , targetLoc :: !TargetLoc
  }


data Hyperlink = Hyperlink
  { hlBegin :: !Int
  , hlEnd :: !Int
  , hlTarget :: !Target
  }
instance NFData Hyperlink where
  rnf x = x `seq` ()

hyperlinkFile :: State -> FilePath -> Maybe Int -> IO Builder.Builder
hyperlinkFile st path offset = do
  text <- BS.readFile $ cfgRoot (stateCfg st) </> path
  links <- reportTime ("queries for " ++ path) $ do
    r <- flip runReaderT st $ haxl $ do
      cxx <- cxxGetHyperlinks (fromString path)
      cm <- codeMarkupHyperlinks (fromString path)
      return (cxx ++ cm)
    evaluate (force r)
  let
    jump = case offset of
      Nothing -> Nothing
      Just o -> Just $ fromIntegral $ fst $
        byteOffsetToLineCol (getLineOffsets text) (fromIntegral o)

  return $ hyperlink (stateCfg st) links jump text

codeMarkupHyperlinks :: Text.Text -> Glean.Haxl w [Hyperlink]
codeMarkupHyperlinks path = do
  xrefs <- Glean.search_ $ Angle.data_ $
    var $ \x -> x `where_` [
      wild .= predicate @CodeMarkup.FileEntityXRefs (
        rec $
          field @"file" (string path) $
          field @"xref" x
        end)
      ]

  hyperlinks <- forM xrefs $ \CodeMarkup.DirectXRef{..} -> do
    file <- Glean.keyOf (CodeMarkup.declaration_file directXRef_target)
    let
      start = fromIntegral $ Glean.unNat $
        Src.byteSpan_start directXRef_source
      length = fromIntegral $ Glean.unNat $
        Src.byteSpan_length directXRef_source
    return Hyperlink
      { hlBegin = start
      , hlEnd = start + length
      , hlTarget = Target
        { targetKind = Text.encodeUtf8 $
            CodeMarkup.declaration_name directXRef_target
        , targetPath = Text.encodeUtf8 file
        , targetLoc = TargetByteOffset $
            fromIntegral (Glean.unNat (Src.byteSpan_start (
              CodeMarkup.declaration_span directXRef_target)))
        }
      }

  let
    -- When there are annotations covering identical spans, prefer an
    -- annotation that points to a different file.  This is mainly to
    -- support Flow, which for a non-local reference produces two
    -- Annotations, one pointing to the import declaration and another
    -- pointing to the original declaraiton.
    unoverlap :: [Hyperlink] -> [Hyperlink]
    unoverlap links = walk links
      where
      walk (a : b : xs)
        | hlBegin a == hlBegin b && hlEnd a == hlEnd b =
          walk (preferred a b : xs)
        | otherwise = a : walk (b : xs)
      walk xs = xs

      -- prefer links that point to a different file
      preferred a b
        | targetPath (hlTarget a) /= Text.encodeUtf8 path = a
        | otherwise = b

  return $ unoverlap $ sortBy (compare `on` hlBegin) hyperlinks


-- | Find all 'Hyperlink' spans for the given file
cxxGetHyperlinks :: Text.Text -> Glean.Haxl w [Hyperlink]
cxxGetHyperlinks path = do  -- ApplicativeDo makes these parallel:

  xref_links <- do
    filexrefs_ <-
      Glean.search_ $ Angle.query $
        predicate @Cxx.FileXRefs $
          rec $
            field @"xmap" (rec $ field @"file" (string path) end)
          end
    filexrefs <- forM filexrefs_ $ \f -> do
      xrefs <- Glean.keyOf f
      xmap <- Glean.keyOf (Cxx.fileXRefs_key_xmap xrefs)
      return f { Cxx.fileXRefs_key = Just xrefs {
        Cxx.fileXRefs_key_xmap = (Cxx.fileXRefs_key_xmap xrefs) {
          Cxx.fileXRefMap_key = Just xmap }}}

    let
      crossref (ByteRange{byteRange_begin=b, byteRange_length=l}, tgt) =
        fmap (Hyperlink (fromIntegral b) (fromIntegral (b+l)))
          <$> xrefTarget tgt

      unoverlap :: [(ByteRange, a)] -> [(ByteRange, a)]
      unoverlap = go 0
        where
          go !_ [] = []
          go k (x@(ByteRange{byteRange_begin=b, byteRange_length=l}, _) : xs)
            | b >= k = x : go (b+l) xs
            | otherwise = go k xs

      -- A given file can have *many* cxx1.FileXRefs facts corresponding
      -- to different compilation traces, but we only want one hyperlink
      -- for each non-overlapping source range. So we want to de-duplicate
      -- the xrefs *before* we start fetching the data about what they
      -- refer to, otherwise we overfetch.
      xrefs = unoverlap $ Set.toList $ collectXRefTargets filexrefs

    mapM crossref xrefs

  pp_links <- do
    traces <- Glean.search_ $ Angle.query $
      predicate @Cxx.PPTrace $ rec $ field @"file" (string path) end

    let crossref (Cxx.PPEvent_include_ trace) = do
          key <- Glean.getKey (Cxx.includeTrace_include_ trace)
          let !Pp1.Include_key
                { include_key_file = file
                , include_key_path = Src.ByteRange b e } = key
          fmap (Hyperlink (fromIntegral $ Glean.unNat b)
              (fromIntegral $ Glean.unNat e))
            <$> target_locH "include" file (Glean.Nat 1)

        crossref (Cxx.PPEvent_use use) = do
          key <- Glean.getKey use
          case key of
            Pp1.Use_key
              { use_key_name = Src.ByteRange b e
              , use_key_definition = Just (Src.Loc file line _) } ->
              fmap (Hyperlink (fromIntegral $ Glean.unNat b)
                  (fromIntegral $ Glean.unNat e))
                <$> target_locH "macro" file line
            _ -> return Nothing

        crossref _ = return Nothing

    fmap catMaybes
      $ mapM crossref
      $ concatMap Cxx.pPTrace_key_events
      $ mapMaybe Cxx.pPTrace_key traces

  return
    $ unoverlap
    $ sortBy order $ catMaybes xref_links ++ pp_links

  where
    order :: Hyperlink -> Hyperlink -> Ordering
    order (Hyperlink a1 b1 _) (Hyperlink a2 b2 _) =
      compare a1 a2 <> compare b1 b2

    unoverlap [] = []
    unoverlap (h : hs) = h : go (hlEnd h) hs
      where
        go !_ [] = []
        go k (h : hs)
          | hlBegin h >= k = h : go (hlEnd h) hs
          | otherwise = go k hs

    -- Thanks to the magic of Haxl, all the Glean.getKey calls below
    -- are batched into a single request to Glean, and sharing in
    -- the results is retained.
    xrefTarget :: Cxx.XRefTarget -> Glean.Haxl w (Maybe Target)
    xrefTarget x = case x of
      Cxx.XRefTarget_declaration (Cxx.Declaration_namespace_ r) -> do
        key <- Glean.getKey r
        target_range "namespace" $ Cxx.namespaceDeclaration_key_source key

      Cxx.XRefTarget_declaration Cxx.Declaration_usingDeclaration{} ->
        return Nothing

      Cxx.XRefTarget_declaration Cxx.Declaration_usingDirective{} ->
        return Nothing

      Cxx.XRefTarget_declaration (Cxx.Declaration_record_ r) -> do
        key <- Glean.getKey r
        target_range "record" $ Cxx.recordDeclaration_key_source key

      Cxx.XRefTarget_declaration (Cxx.Declaration_enum_ r) -> do
        key <- Glean.getKey r
        target_range "enum" $ Cxx.enumDeclaration_key_source key

      Cxx.XRefTarget_declaration (Cxx.Declaration_typeAlias r) -> do
        key <- Glean.getKey r
        let kind = case Cxx.typeAliasDeclaration_key_kind key of
              Cxx.TypeAliasKind_Typedef -> "typedef"
              Cxx.TypeAliasKind_Using -> "using"
        target_range
          ("type alias (" <> kind <> ")")
          $ Cxx.typeAliasDeclaration_key_source key

      Cxx.XRefTarget_declaration (Cxx.Declaration_function_ r) -> do
        key <- Glean.getKey r
        target_range "function" $
          Cxx.functionDeclaration_key_source key

      Cxx.XRefTarget_declaration (Cxx.Declaration_variable r) -> do
        key <- Glean.getKey r
        target_range
          (case Cxx.variableDeclaration_key_kind key of
            Cxx.VariableKind_global_{} -> "variable"
            Cxx.VariableKind_field{} -> "field"
            Cxx.VariableKind_ivar{} -> "ivar") $
          Cxx.variableDeclaration_key_source key

      Cxx.XRefTarget_declaration (Cxx.Declaration_objcContainer r) -> do
        key <- Glean.getKey r
        let kind = case Cxx.objcContainerDeclaration_key_id key of
              Cxx.ObjcContainerId_protocol{} -> "objc protocol"
              Cxx.ObjcContainerId_interface_{} -> "objc interface"
              Cxx.ObjcContainerId_categoryInterface{} -> "objc category"
              Cxx.ObjcContainerId_extensionInterface{} -> "objc extension"
              Cxx.ObjcContainerId_implementation{} -> "objc implementation"
              Cxx.ObjcContainerId_categoryImplementation{} ->
                "objc category implementation"
        target_range kind $
          Cxx.objcContainerDeclaration_key_source key

      Cxx.XRefTarget_declaration (Cxx.Declaration_objcMethod r) -> do
        key <- Glean.getKey r
        target_range "objc method" $
          Cxx.objcMethodDeclaration_key_source key

      Cxx.XRefTarget_declaration (Cxx.Declaration_objcProperty r) -> do
        key <- Glean.getKey r
        target_range "objc property" $
          Cxx.objcPropertyDeclaration_key_source key

      Cxx.XRefTarget_enumerator r -> do
        key <- Glean.getKey r
        target_range "enumerator" $ Cxx.enumerator_key_source key

      Cxx.XRefTarget_objcSelector{} -> return Nothing

      Cxx.XRefTarget_unknown (Src.Loc file line _) ->
        target_locH "unknown" file line

      Cxx.XRefTarget_indirect r -> do
        key <- Glean.getKey r
        xrefTarget $ Cxx.xRefIndirectTarget_key_target key

    target_range kind (Src.Range file line _ _ _) =
      target_locH kind file line

    target_locH kind file line = do
      path <- Glean.getKey (file :: Src.File)
      return (Just $ Target kind (Text.encodeUtf8 path) $
        TargetLine $ fromIntegral $ Glean.unNat line)



hyperlink :: Config -> [Hyperlink] -> Maybe Int -> ByteString -> Builder
hyperlink cfg links jump s =
  htmlPre cfg jump $ mconcat $ map html $ chunks links s

data Chunk
  = BeginLink !Target
  | EndLink
  | BeginLine !Int
  | EndLine
  | Text ByteString

ord8 :: Char -> Word8
ord8 = fromIntegral . ord

html :: Chunk -> Builder
html (BeginLink t) = Builder.fromByteString $ mconcat $
  ["<a href=\"/", targetPath t] ++
  (case targetLoc t of
    TargetLine l -> ["#", fromString $ show l]
    TargetByteOffset o -> ["?offset=", fromString $ show o]) ++
  ["\" title=\"", targetKind t, "\" class=\"", targetKind t, "\"/>"]
html EndLink = Builder.fromByteString "</a>"
html (BeginLine n) = Builder.fromByteString $ mconcat
  ["\n<code id=\"", fromString $ show n, "\">"]
html EndLine = Builder.fromByteString "</code>"
html (Text s) =
  mconcat $ map (Builder.fromByteString . escape) $ BS.unpack s
  where
    escape c
      | c == ord8 '<' = "&lt;"
      | c == ord8 '>' = "&gt;"
      | c == ord8 '&' = "&amp;"
      | otherwise = BS.singleton c

chunks :: [Hyperlink] -> ByteString -> [Chunk]
chunks links !s = BeginLine 1 : notLinked 1 0 0 links
  where
    !n = BS.length s

    notLinked !_line !begin !end _
      | end == n = [text begin end, EndLine]
    notLinked !line !begin !end (link : links)
      | hlBegin link < end = notLinked line begin end links
      | hlBegin link == end =
        let begin_link = BeginLink $ hlTarget link
        in
        text begin end
        : begin_link : linked line end end (hlEnd link) begin_link links
    notLinked !line !begin !end links
      | BS.index s end == ord8 '\n' =
          text begin end
          : EndLine
          : BeginLine (line+1)
          : notLinked (line+1) (end+1) (end+1) links
      | otherwise = notLinked line begin (end+1) links

    linked !line !begin !end !k begin_link links
      | end == n = [text begin end, EndLink, EndLine]
      | end == k = text begin end : EndLink : notLinked line end end links
      | BS.index s end == ord8 '\n' =
          text begin end
          : EndLink
          : EndLine
          : BeginLine (line+1)
          : begin_link
          : linked (line+1) (end+1) (end+1) k begin_link links
      | otherwise = linked line begin (end+1) k begin_link links

    text begin end = Text $ BS.take (end-begin) $ BS.drop begin s

htmlPre :: Config -> Maybe Int -> Builder -> Builder
htmlPre cfg j s = mconcat
  [ Builder.fromByteString header
  , s
  , Builder.fromByteString footer
  ]
  where
    header = fromString $ unlines $
      ["<html><head>"
      ,"<style>"]
      ++
      cfgStyles cfg
      ++
      ["</style>"] ++
      jump j ++
      ["</head><body><pre>"]
    footer = "</pre></body></html>"

    -- Scroll the page to the desired line
    jump :: Maybe Int -> [String]
    jump Nothing = []
    jump (Just o) =
      [ "<script>"
      , "function scroll(id){"
      , "var e = document.getElementById(id);"
      , "window.scrollTo(e.offsetLeft, e.offsetTop);}"
      , "window.onload = function() { scroll(\"" <> show o <> "\"); };"
      , "</script>"
      ]

serve :: State -> Wai.Application
serve state req respond = do
  s <- if Wai.pathInfo req `elem` [[],["index.html"]]
    then do
      logInfo "list files"
      listFiles state
    else do
      let path = joinPath $ map Text.unpack $ Wai.pathInfo req
      logInfo path
      let
        offset = case Wai.queryString req of
          [("offset", Just n)] -> Just (read (BC.unpack n))
          _ -> Nothing
      hyperlinkFile state path offset
  respond $
    Wai.responseBuilder
      HTTP.status200
      [(HTTP.hContentType, "text/html")]
      s

main :: IO ()
main =
  withConfigOptions options $ \(cfg, cfgOpts) ->
  withEventBaseDataplane $ \evb ->
  withConfigProvider cfgOpts $ \(configAPI :: ConfigAPI) -> do
    Glean.withRemoteBackend evb configAPI (cfgService cfg)
      $ \backend -> do
      repo <- case cfgRepoHash cfg of
        Nothing -> Glean.getLatestRepo backend (cfgRepoName cfg)
        Just hash -> return $ Glean.Repo
          (cfgRepoName cfg)
          (fromString hash)

      let state = State
            { stateCfg = cfg
            , stateBackend = Some backend
            , stateRepo = repo
            }

      whenJust (cfgHttp cfg) $ \port -> do
        Warp.runSettings
          (Warp.setPort port
            $ Warp.setHost (fromString $ cfgHttpIface cfg)
            Warp.defaultSettings)
          (serve state)
