{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-

Convert Data.SCIP into glean/schema/lsif.angle-compatible data via JSON.

Note: this module generates Angle but has no dependency on the Glean LSIF
schema (which it targets), to make developer iteration quicker.

-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.SCIP.Angle (
    scipToAngle,
    SCIP.LanguageId(..)
  ) where

import Control.Monad
import Lens.Micro ((^.))
import Data.Bits ( Bits(testBit) )
import Data.Maybe ( catMaybes, fromMaybe )
import Data.Function ((&))
import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Aeson as Aeson
import Data.Int ( Int32, Int64 )
import qualified Data.ByteString as B
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Strict ( Map, ( !? ) )
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import qualified Data.ProtoLens as Proto
import qualified Data.Vector as V
import Data.Aeson

import qualified Proto.Scip as Scip
import qualified Proto.Scip_Fields as Scip

import qualified Data.LSIF.Gen as SCIP

{-

Debug scip files directly via protoc. Assuming you the scip.proto file handy
From https://github.com/sourcegraph/scip/blob/main/scip.proto

> protoc --decode scip.Index scip.proto  < index.scip

The approach is to faithfully capture SCIP keyed by scip.Symbol.
Then derive xref relationships (and connect hovers to definitions),
in the Glean side

-}

type Parse a = forall m . Monad m => StateT Env m a

data StringPredicate =
  Symbol
  | LocalName
  | File
  | DisplayName
  deriving (Eq, Ord)

data Env = Env {
    -- unique supply for new Glean fact identifiers
    unique :: {-# UNPACK #-}!Int64,

    -- Hashmaps from any raw text fact to the id we generated.
    -- Used to do a bit of sharing before emitting to Glean.
    -- We maintain one hashmap per string predicate.
    factId :: !(Map StringPredicate (HashMap Text SCIP.Id))
  }

emptyState :: Env
emptyState = Env
  1 {- cannot use 0 as a fact id -}
  Map.empty

--
-- Scip doesn't number facts, but it is still useful for us to do so,
-- to get more sharing in the output json
--
nextId :: Parse SCIP.Id
nextId = do
  !i <- gets unique
  modify $ \e -> e { unique = i + 1 }
  return (SCIP.Id i)

setDefFact :: StringPredicate -> Text -> SCIP.Id -> Parse ()
setDefFact pred val i = modify $ \e ->
  let m = factId e !? pred & fromMaybe HashMap.empty & HashMap.insert val i in
  e { factId = Map.insert pred m (factId e)}

getDefFactId :: StringPredicate -> Text -> Parse (Maybe SCIP.Id)
getDefFactId pred sym = do
  m <- gets factId
  pure $ m !? pred >>= HashMap.lookup sym

-- | Make a fresh name or return an existing one if we've seen it
getOrSetFact :: StringPredicate -> Text -> Parse (SCIP.Id, Bool)
getOrSetFact pred sym = do
  mId <- getDefFactId pred sym
  case mId of
    Nothing -> do
      id_ <- nextId
      setDefFact pred sym id_
      return (id_, False)
    Just id_ -> return (id_, True)

--
-- | Parse scip.proto into JSON-encoded Angle facts for the scip.angle schema
--
-- Uses the proto-lens interface to scip.proto
--
scipToAngle
  :: Maybe SCIP.LanguageId
  -> Bool
  -> Maybe FilePath
  -> Maybe FilePath
  -> B.ByteString
  -> Aeson.Value
scipToAngle mlang inferLanguage mPathPrefix mStripPrefix scip =
  Aeson.Array $ V.fromList $
    SCIP.generateSCIPJSON (SCIP.insertPredicateMap HashMap.empty result)
  where
    (result,_) = runState (runTranslate mlang
      inferLanguage mPathPrefix mStripPrefix scip) emptyState

-- | First pass, grab all the occurences with _role := Definition
-- build up symbol string -> fact id for all defs
runTranslate
  :: Maybe SCIP.LanguageId
  -> Bool
  -> Maybe FilePath
  -> Maybe FilePath
  -> B.ByteString
  -> Parse [SCIP.Predicate]
runTranslate mlang inferLanguage mPathPrefix mStripPrefix scip =
  case Proto.decodeMessage scip of
    Left err -> error err
    Right (v :: Scip.Index) -> do
      a <- decodeScipMetadata (v ^. Scip.metadata)
      bs <- mapM
        (decodeScipDoc mlang inferLanguage mPathPrefix mStripPrefix)
        (v ^. Scip.documents)
      return (a <> concat bs)

--
-- Each document has a repo-relative filepath, defs and refs (symbols and
-- occurences). Generate fact ids and record symbol id facts as we find them,
-- then cross-reference with occurences in second pass
--
decodeScipDoc
  :: Maybe SCIP.LanguageId
  -> Bool
  -> Maybe FilePath
  -> Maybe FilePath
  -> Scip.Document
  -> Parse [SCIP.Predicate]
decodeScipDoc mlang inferLanguage mPathPrefix mStripPrefix doc = do
  srcFileId <- nextId
  let filepath0 = doc ^. Scip.relativePath
      -- first, strip any matching prefix
      filepath1 = case Text.pack <$> mStripPrefix of
        Nothing -> filepath0
        Just prefix -> fromMaybe filepath0 $ Text.stripPrefix prefix filepath0
      -- and maybe prepend a new prefix
      filepath = case Text.pack <$> mPathPrefix of
        Nothing -> filepath1
        Just prefix -> prefix <> filepath1
  setDefFact File filepath srcFileId
  let srcFile = SCIP.srcFile srcFileId filepath
  langFileId <- nextId
  let parseLang = SCIP.parseLanguage (doc ^. Scip.language)
      langEnum = fromEnum $ case parseLang of
                SCIP.UnknownLanguage
                  -- if --infer-language , look at the suffix
                  | inferLanguage
                  , Just langId <- fileLanguageOf filepath
                  -> langId
                  -- otherwise if --language, assume that's correct
                  | Just langId <- mlang -> langId -- use default if present
                  -- otherwise its really unknown
                  | otherwise -> SCIP.UnknownLanguage
                x -> x -- scip document provides the language
  fileLang <- SCIP.predicateId "scip.FileLanguage" langFileId
    [ "file" .= srcFileId
    , "language" .= langEnum
    ]
  occs <- mapM (decodeScipOccurence srcFileId filepath)
      (doc ^. Scip.occurrences)
  infos <- mapM (decodeScipInfo filepath) (doc ^. Scip.symbols)
  return (srcFile : fileLang <> concat (occs <> infos))

-- We really don't want to do a general purpose language detector
-- but rely on the indexer knowing things. For the Java/Kotlin case,
-- files are frequently intermingled in the same build so we can't
-- decide a priori which language is being indexed
fileLanguageOf :: Text -> Maybe  SCIP.LanguageId
fileLanguageOf filepath
  | "kt" `Text.isSuffixOf` filepath = Just SCIP.Kotlin
  | "java" `Text.isSuffixOf` filepath = Just SCIP.Java
  | otherwise = Nothing

decodeScipInfo :: Text -> Scip.SymbolInformation -> Parse [SCIP.Predicate]
decodeScipInfo filepath info = do
  (docIds, docFacts) <- unzip <$> forM scipDocs (\docStr -> do
    docId <- nextId
    return (docId, SCIP.Predicate "scip.Documentation" [
            object [ SCIP.factId docId, "key" .= Text.strip docStr ]
          ]))
  mSymId <- getSymbolId scipSymbol filepath
  symDocFacts <- case mSymId of
    Nothing -> return []
    Just symId -> forM docIds (\docId ->
        SCIP.predicateId "scip.SymbolDocumentation" docId [
          "symbol" .= symId,
          "docs" .= docId
        ])
  displayNameFacts <- case mSymId of
    Nothing -> return []
    Just symId -> case scipDisplayName of
      "" -> return []
      _ ->  displayNameFacts scipDisplayName symId
  relationshipsFacts <- case mSymId of
    Nothing -> return []
    Just symId -> forM scipRelationshps (\rel ->
        if rel ^. Scip.isImplementation then do
          let implementedSymbol = rel ^. Scip.symbol
          implementedSymbolId <- getSymbolId implementedSymbol filepath
          case implementedSymbolId of
            Nothing -> return []
            Just implementedSymbolId ->
              SCIP.predicate "scip.IsImplementation" [
                  "symbol" .= symId,
                  "implemented" .= implementedSymbolId
                ]
        else
          pure [] )
  return
    ( docFacts
    <> concat symDocFacts
    <> displayNameFacts
    <> concat relationshipsFacts )

  where
    scipSymbol = info ^. Scip.symbol
    scipDocs = info ^. Scip.documentation
    scipDisplayName = info ^. Scip.displayName
    scipRelationshps = info ^. Scip.relationships

getSymbolId :: Text -> Text -> Parse (Maybe SCIP.Id)
getSymbolId symbol filepath = do
  let eSym = symbolFromString symbol
  let qualifiedSymbol = case eSym of
        Left err -> error(show err)
        Right (Local _) -> filepath <> "/" <> symbol
        Right (Global {}) -> symbol
  getDefFactId Symbol qualifiedSymbol


displayNameFacts :: Text -> SCIP.Id -> Parse [SCIP.Predicate]
displayNameFacts scipDisplayName symId = do
  (displayNameId, seenDisplayName) <- getOrSetFact DisplayName scipDisplayName
  let displayNameFact =
        ([ SCIP.Predicate "scip.DisplayName"
              [object [SCIP.factId displayNameId, "key" .= scipDisplayName]] |
          not seenDisplayName ])
  symbolDisplayNameFact <-
    SCIP.predicate
      "scip.DisplayNameSymbol"
      ["symbol" .= symId, "displayName" .= displayNameId]
  return (displayNameFact <> symbolDisplayNameFact)


-- | An occurence of a symbol in a given document the optional symbol role
-- will tell us if it is an xref or a def or other
decodeScipOccurence
  :: SCIP.Id
  -> Text
  -> Scip.Occurrence
  -> Parse [SCIP.Predicate]
decodeScipOccurence fileId filepath occ = do
    fileRangeId <- nextId
    fileRange <- SCIP.predicateId "scip.FileRange" fileRangeId
      [ "file" .= fileId
      , "range" .= decodeScipRange scipRange
      ]
    let eSym = symbolFromString scipSymbol
    symbolFacts <- case eSym of
          Left err -> error (show err) -- Can't handle this symbol format
          Right (Local _) -> decodeLocalOccurence filepath scipSymbol symRoles
              fileRangeId
          Right Global{..} -> decodeGlobalOccurence scipSymbol symRoles
              fileRangeId descriptor
    return (fileRange <> symbolFacts)
  where
    scipRange = occ ^. Scip.range
    scipSymbol = occ ^. Scip.symbol
    symRoles = toSymbolRole (occ ^. Scip.symbolRoles)

decodeGlobalOccurence
  :: Text -> Set Scip.SymbolRole -> SCIP.Id -> Descriptor
  -> Parse [SCIP.Predicate]
decodeGlobalOccurence scipSymbol symRoles fileRangeId Descriptor{..} = do
  (symbolId, seenSymbol) <- getOrSetFact Symbol scipSymbol
  let symbolFact :: [SCIP.Predicate]
        | seenSymbol = []
        | otherwise = pure $
            SCIP.Predicate "scip.Symbol" [
                object [ SCIP.factId symbolId, "key" .= scipSymbol ]
            ]
  let roleFact :: [[SCIP.Predicate]] = if Scip.Definition `Set.member` symRoles
        then SCIP.predicate "scip.Definition" [
            "symbol" .= symbolId,
            "location" .= fileRangeId
          ]
        else SCIP.predicate "scip.Reference" [
            "symbol" .= symbolId,
            "location" .= fileRangeId
          ]
  (nameId, seenName) <- getOrSetFact LocalName text
  let nameFact :: [SCIP.Predicate]
        | seenName = []
        | otherwise = pure $
            SCIP.Predicate "scip.LocalName" [
                object [ SCIP.factId nameId, "key" .= text ]
            ]
  let symbolNameFact :: [[SCIP.Predicate]]
        | seenSymbol = []
        | otherwise = SCIP.predicate "scip.SymbolName" [
              "symbol" .= symbolId,
              "name" .= nameId
           ]
  let kindFact :: [SCIP.Predicate] = concat $
        case SCIP.kindFromSuffix suffix of
          SCIP.SkUnknown -> [[]]
          kind -> SCIP.predicate "scip.SymbolKind" [
              "symbol" .= symbolId,
              "kind" .= fromEnum kind
            ]
  return $ symbolFact <> concat roleFact <> nameFact <>
            concat symbolNameFact <> kindFact

decodeLocalOccurence
  :: Text
  -> Text
  -> Set Scip.SymbolRole
  -> SCIP.Id
  -> Parse [SCIP.Predicate]
decodeLocalOccurence filepath localSymbol symRoles fileRangeId = do
  let qualifiedSymbol = filepath <> "/" <> localSymbol
  (symbolId, seenSymbol) <- getOrSetFact Symbol qualifiedSymbol
  let symbolFact :: [SCIP.Predicate]
        | seenSymbol = []
        | otherwise = pure $
            SCIP.Predicate "scip.Symbol" [
                object [ SCIP.factId symbolId, "key" .= qualifiedSymbol ]
            ]
  let roleFact :: [[SCIP.Predicate]] = if Scip.Definition `Set.member` symRoles
        then SCIP.predicate "scip.Definition" [
            "symbol" .= symbolId,
            "location" .= fileRangeId
          ]
        else SCIP.predicate "scip.Reference" [
            "symbol" .= symbolId,
            "location" .= fileRangeId
          ]
  (nameId, seenName) <- getOrSetFact LocalName localSymbol
  let nameFact :: [SCIP.Predicate]
        | seenName = []
        | otherwise = pure $
            SCIP.Predicate "scip.LocalName" [
                object [ SCIP.factId nameId, "key" .= localSymbol ]
            ]
  let symbolNameFact :: [[SCIP.Predicate]]
        | seenSymbol = []
        | otherwise = SCIP.predicate "scip.SymbolName" [
              "symbol" .= symbolId,
              "name" .= nameId
           ]
  let kindFact :: [[SCIP.Predicate]]
        | seenSymbol = []
        | otherwise = SCIP.predicate "scip.SymbolKind" [
              "symbol" .= symbolId,
              -- TODO: this could be any SymbolInformation.Kind
              "kind" .= fromEnum SCIP.SkVariable
            ]
  return $ symbolFact <> concat roleFact <> nameFact <>
            concat symbolNameFact <> concat kindFact

-- | For sharding we might want to take a repo-relative anchor here
-- as it potentially differs to project root when combining SCIP files
decodeScipMetadata :: Scip.Metadata -> Parse [SCIP.Predicate]
decodeScipMetadata v = SCIP.predicate "scip.Metadata" $
  [ "version" .= fromEnum (v ^. Scip.version)
  , "textEncoding" .= fromEnum (v ^. Scip.textDocumentEncoding)
  ] ++ (case v ^. Scip.maybe'toolInfo of
          Nothing -> []
          Just ti ->
            ["toolInfo" .= object [
              "toolName" .= (ti ^. Scip.name),
              "toolArgs" .= (ti ^. Scip.arguments),
              "version" .= (ti ^. Scip.version)
            ]])

-- scip ranges are int32
toNat :: Int32 -> Int64
toNat = fromIntegral

-- [startLine, startCharacter, endCharacter]`. The end line
--  is inferred to have the same value as the start line.
decodeScipRange :: [Int32] -> Aeson.Value
decodeScipRange [lineBegin,colBegin,colEnd] =
  SCIP.toRange (SCIP.Range
    (SCIP.Position (toNat lineBegin) (toNat colBegin))
    (SCIP.Position (toNat lineBegin) (toNat colEnd))) -- n.b
-- : `[startLine, startCharacter, endLine, endCharacter]`
decodeScipRange [lineBegin,colBegin,lineEnd,colEnd] =
  SCIP.toRange (SCIP.Range
    (SCIP.Position (toNat lineBegin) (toNat colBegin))
    (SCIP.Position (toNat lineEnd) (toNat colEnd)))
decodeScipRange range = error $
  "decodeScipRange: unexpected range format: " <> show range

{-

//   <symbol>               ::= <scheme> ' ' <package> ' ' { <descriptor> }
//      | 'local ' <local-id>
//   <package>              ::= <manager> ' ' <package-name> ' ' <version>
//   <scheme>               ::= any UTF-8, escape spaces with double space.
//   <manager>              ::= same as above, use the placeholder '.' to
//          indicate an empty value
//   <package-name>         ::= same as above
//   <version>              ::= same as above
//   <descriptor>           ::= <package> | <type> | <term> | <method>
//       | <type-parameter> | <parameter> | <meta>
//   <package>              ::= <name> '/'
//   <type>                 ::= <name> '#'
//   <term>                 ::= <name> '.'
//   <meta>                 ::= <name> ':'
//   <method>               ::= <name> '(' <method-disambiguator> ').'
//   <type-parameter>       ::= '[' <name> ']'
//   <parameter>            ::= '(' <name> ')'
//   <name>                 ::= <identifier>
//   <method-disambiguator> ::= <simple-identifier>
//   <identifier>           ::= <simple-identifier> | <escaped-identifier>
//   <simple-identifier>    ::= { <identifier-character> }
//   <identifier-character> ::= '_' | '+' | '-' | '$' | ASCII letter or digit
//   <escaped-identifier>   ::= '`' { <escaped-character> } '`'
//   <escaped-characters>   ::= any UTF-8 character, escape backticks with
//   d        ouble backtick.

-}

data ScipSymbol
  = Local !Text
  | Global
      { scheme :: !Text
      , package :: !Package
      , descriptor :: !Descriptor -- grammar says these can be repetitions...
      }

data Package = Package
  { manager :: !Text
  , pkgname :: !Text
  , version :: !Text
}

data Descriptor = Descriptor
  { text:: !Text
  , suffix :: !SCIP.Suffix
  }

--
-- Parser and ADT for Scip.Symbol strings.
-- These are well structured but stored as strings for reasons
--
-- https://github.com/sourcegraph/scip/blob/main/scip.proto#L81
-- e.g.
--
-- >  "scip-typescript npm new.docusaurus.io 2.0.0-beta.17 \
-- >      functionUtils/`playgroundUtils.ts`/CookieName."
--
symbolFromString :: Text -> Either Text ScipSymbol
symbolFromString str
  -- 'local ' <local-id>
  | ("local", localId) <- split normalStr
  = Right $ Local localId
  --  <scheme> ' ' <package> ' ' { <descriptor> }
  | (scheme, rest1) <- split normalStr
  -- <package> ::= <manager> ' ' <package-name> ' ' <version>
  , (manager, rest2) <- split rest1
  , (pkgname, rest3) <- split rest2
  , (version, symStrs) <- split rest3
  , (text, suffix) <- SCIP.parseSuffix symStrs
  = Right $ Global scheme Package{..} Descriptor {..}

  where
    -- we normalize space-escaping in identifiers with underscore
    normalStr = Text.intercalate "_" (Text.splitOn doubleSpace str)
      where doubleSpace = "  "

    split xs = case Text.breakOn " " xs of
      (tok, rest) -> (tok, if Text.null rest then rest else Text.tail rest)

{-
bitmask in i32

enum SymbolRole {
  // unused
  UnspecifiedSymbolRole = 0;
  // Is the symbol defined here? If not, then this is a symbol reference.
  Definition = 0x1;
  // Is the symbol imported here?
  Import = 0x2;
  // Is the symbol written here?
  WriteAccess = 0x4;
  // Is the symbol read here?
  ReadAccess = 0x8;
  // Is the symbol in generated code?
  Generated = 0x10;
  // Is the symbol in test code?
  Test = 0x20;
-}
toSymbolRole :: Int32 -> Set Scip.SymbolRole
toSymbolRole i = Set.fromList $ catMaybes
  [ Nothing -- if i == 0 then Just Scip.UnspecifiedSymbolRole else Nothing
  , has 0 Scip.Definition
  , has 1 Scip.Import
  , has 2 Scip.WriteAccess
  , has 3 Scip.ReadAccess
  , has 4 Scip.Generated
  , has 5 Scip.Test
  ]
  where
    has n ty
      | i `testBit` n = Just ty
      | otherwise = Nothing
