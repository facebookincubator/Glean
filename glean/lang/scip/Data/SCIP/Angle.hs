{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-

Convert Data.LSIF into glean/schema/lsif.angle-compatible data via JSON.

Note: this module generates Angle but has no dependency on the lsif schema, to
make developer iteration quicker.

-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.SCIP.Angle (
    scipToAngle
  ) where

import Data.Aeson ( object, KeyValue((.=)) )
import Lens.Micro ((^.))
import Data.Bits ( Bits(testBit) )
import Data.Maybe ( catMaybes )
import Util.Text ( textShow, textToInt )
import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Aeson as Aeson
import Data.Int ( Int32, Int64 )
import qualified Data.ByteString as B
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.State.Strict
import qualified Data.ProtoLens as Proto
import qualified Data.Vector as V

import qualified Proto.Scip as Scip
import qualified Proto.Scip_Fields as Scip

import qualified Data.LSIF.Gen as LSIF
import qualified Data.LSIF.Types as LSIF

import qualified Debug.Trace as Debug

{-

Debug scip files directly via protoc: .e.g
> protoc --decode scip.Index scip.proto  --proto_path scip < index.scip

-- approach
-- iterate over symbols (range vector), and store symbol -> id mapping
-- iterate over occurences (ranges), and emit defs and xrefs

-}

type Parse a = forall m . Monad m => StateT Env m a

data Env = Env {
    -- unique supply for new facts
    unique :: {-# UNPACK #-}!Int64,

    -- hashmap from symbol strings to their definition fact id
    defFactId :: !(HashMap Text LSIF.Id)
  }

emptyState :: Env
emptyState = Env
  1 {- cannot use 0 as a fact id -}
  HashMap.empty

--
-- Scip doesn't number facts, but it is still useful for us to do so,
-- to get more sharing in the output json
--
nextId :: Parse LSIF.Id
nextId = do
  !i <- gets unique
  modify $ \e -> e { unique = i + 1 }
  return (LSIF.Id i)

setDefFact :: Text -> LSIF.Id -> Parse ()
setDefFact sym i = modify $ \e ->
  e { defFactId = HashMap.insert sym i (defFactId e) }

getDefFactId :: Text -> Parse (Maybe LSIF.Id)
getDefFactId sym = do
  hm <- gets defFactId
  pure (HashMap.lookup sym hm)

--
-- | Parse scip.proto into JSON-encoded Angle facts for the lsif.angle schema
--
-- Uses the proto-lens interface to scip.proto
--
-- Rough translation of bindings/go/scip/convert.go
--
scipToAngle :: B.ByteString -> Aeson.Value
scipToAngle scip = Aeson.Array $ V.fromList $
    LSIF.generateJSON (LSIF.insertPredicateMap HashMap.empty (preds1 ++ preds2))
  where
    (preds1,symEnv) = runState (runGatherDefs scip) emptyState
    (preds2,_) = runState (runScipAll scip) symEnv

-- | First pass, grab all the occurences with _role := Definition
-- build up symbol string -> fact id for all defs
runGatherDefs :: B.ByteString -> Parse [LSIF.Predicate]
runGatherDefs scip = case Proto.decodeMessage scip of
  Left err -> error err
  Right (v :: Scip.Index) -> do
    a <- decodeScipMetadata (v ^. Scip.metadata)
    b <- concat <$> mapM decodeScipDoc (v ^. Scip.documents)
    pure (a <> b)

runScipAll :: B.ByteString -> Parse [LSIF.Predicate]
runScipAll scip = case Proto.decodeMessage scip of
  Left err -> error err
  Right (v :: Scip.Index) ->
    concat <$> mapM decodeScipDocXRefs (v ^. Scip.documents)

decodeScipMetadata :: Scip.Metadata -> Parse [LSIF.Predicate]
decodeScipMetadata v = LSIF.predicate "lsif.Metadata.1" $
  [ "lsifVersion" .= textShow (v ^. Scip.version)
  , "positionEncoding" .= textShow (v ^. Scip.textDocumentEncoding)
  ] ++ (case v ^. Scip.maybe'toolInfo of
          Nothing -> []
          Just ti ->
            ["toolInfo" .= object [
              "toolName" .= (ti ^. Scip.name),
              "toolArgs" .= (ti ^. Scip.arguments),
              "version" .= (ti ^. Scip.version)
            ]])
-- note: we don't need projectRoot as filepaths are root-relative in scip

--
-- Each document has a repo-relative filepath, defs and refs (symbols and
-- occurences). Generate fact ids and record symbol id facts as we find them,
-- then cross-reference with occurences in second pass
--
decodeScipDoc :: Scip.Document -> Parse [LSIF.Predicate]
decodeScipDoc doc = do
  docId <- nextId
  file <- mkDocumentFact docId doc
  occs <- concat <$> mapM (decodeScipOccurence docId) (doc ^. Scip.occurrences)
  pure $ file <> occs

decodeScipDocXRefs :: Scip.Document -> Parse [LSIF.Predicate]
decodeScipDocXRefs doc = do
  docId <- nextId -- we will have recorded it previously, but glean will de-dup
  file <- mkDocumentFact docId doc
  occs <- concat <$> mapM (decodeScipXRefs docId) (doc ^. Scip.occurrences)
  hovers <- concat <$> mapM decodeScipHovers (doc ^. Scip.symbols)
  pure $ file <> occs <> hovers

-- we have sym -> id map in env, now generate xref facts
decodeScipXRefs :: LSIF.Id -> Scip.Occurrence -> Parse [LSIF.Predicate]
decodeScipXRefs docId occ = do
  let Occ{..} = decodeOcc occ
  if Scip.Definition `Set.notMember` symRoles
    then do
      -- get the xref range
      rangeId <- nextId
      rangeFact <- decodeScipRange rangeId symScipSymbol (occ ^. Scip.range)

      -- look up defn id fact in env
      mDefId <- getDefFactId symFullName
      refFacts <- case mDefId of
        Just defId -> do -- emit a reference fact
          a <- LSIF.predicate "lsif.Reference.1"
            [ "file" .= docId
            , "range" .= rangeId
            , "target" .= defId
            ]
          b <- LSIF.predicate "lsif.DefinitionUse.1"
            [ "target" .= defId
            , "file" .= docId
            , "range" .= rangeId
            ]
          return (a <> b)

        -- if we didn't find a definining occurence, then it is an external
        -- or 3rd party xref. we don't know the precise location
        -- we can likely _guess_ by parsing the symbol descriptor tho..
        Nothing ->
          pure []

      pure (rangeFact <> refFacts)
    else pure []

decodeScipHovers :: Scip.SymbolInformation -> Parse [LSIF.Predicate]
decodeScipHovers symInfo = do
  let symFullName = symInfo ^. Scip.symbol
  mDefId <- getDefFactId symFullName
  case mDefId of
    Nothing -> Debug.trace (show symFullName) $ pure [] -- impossible?
    Just defId -> concat <$> do
      forM (symInfo ^. Scip.documentation) $ \docstr -> do
        hoverId <- nextId
        a <- LSIF.predicateId "lsif.HoverContent.1" hoverId
          [ "text" .= LSIF.string docstr
          , "language" .= fromEnum LSIF.UnknownLanguage -- could parse docstr
          ]
        b <- LSIF.predicate "lsif.DefinitionHover.1"
          [ "defn" .= defId
          , "hover" .= hoverId
          ]
        pure (a <> b)

-- | an occurence of a symbol in a given document the optional (?) symbol role
-- will tell us if it is an xref or a def or other
decodeScipOccurence :: LSIF.Id -> Scip.Occurrence -> Parse [LSIF.Predicate]
decodeScipOccurence docId occ = do
  let Occ{..} = decodeOcc occ
  if Scip.Definition `Set.member` symRoles
    then do
      -- get the defn range
      rangeId <- nextId
      rangeFact <- decodeScipRange rangeId symScipSymbol (occ ^. Scip.range)

      -- emit a definition fact
      defId <- nextId
      defFact <- LSIF.predicateId "lsif.Definition.1" defId
        [ "file" .= docId
        , "range" .= rangeId
        ]

      kindFact <- case symScipSymbol of
        Nothing -> pure []
        Just Local{} ->
          LSIF.predicate "lsif.DefinitionKind.1"
            [ "defn" .= defId
            , "kind" .= fromEnum LSIF.Local
            ]
        Just Global{..} ->
          LSIF.predicate "lsif.DefinitionKind.1"
            [ "defn" .= defId
            , "kind" .= fromEnum (kindFromSuffix (suffix descriptor))
            ]

      -- record symbol -> fact id for xref use later
      setDefFact symFullName defId

      -- we know its a def, so generate the moniker facts
      monikerFacts <- case symScipSymbol of
        Nothing -> do
          LSIF.predicate "lsif.DefinitionMoniker.1"
            [ "defn" .= defId
            ]
        Just Local{} -> do
          LSIF.predicate "lsif.DefinitionMoniker.1"
            [ "defn" .= defId
            ] -- these are anonymous locals with an integer.

        Just Global{..} -> do
          monikerId <- nextId
          monikerFact <- LSIF.predicateId "lsif.Moniker.1" monikerId
            [ "kind" .= fromEnum (
                          if Scip.Import `Set.member` symRoles
                          then LSIF.Import else LSIF.Export )
            , "scheme" .= LSIF.string scheme
            , "ident" .= LSIF.string symFullName
            ]

          entityFact <- LSIF.predicate "lsif.DefinitionMoniker.1"
            [ "defn" .= defId
            , "moniker" .= monikerId
            ]

          -- can throw in a (highly duplicated) package information fact
          pkgInfoFact <- LSIF.predicate "lsif.PackageInformation.1"
            [ "name" .=  pkgname package
            , "manager" .= manager package
            , "version" .= version package
            ]

          pure (monikerFact <> entityFact <> pkgInfoFact)
      pure (rangeFact <> defFact <> monikerFacts <> kindFact)

    else pure [] -- handle xrefs in second pass


-- scip ranges are int32
toNat :: Int32 -> Int64
toNat = fromIntegral

toLanguage :: Text.Text -> LSIF.LanguageId
toLanguage file
  | ".ts" `Text.isSuffixOf` file = LSIF.TypeScript
  | ".tsx" `Text.isSuffixOf` file = LSIF.TypeScript
  | otherwise = LSIF.UnknownLanguage

  -- [startLine, startCharacter, endCharacter]`. The end line
  --   is inferred to have the same value as the start line.
decodeScipRange :: LSIF.Id -> Maybe ScipSymbol -> [Int32] -> Parse [LSIF.Predicate]
decodeScipRange factId name [lineBegin,colBegin,colEnd] =
  LSIF.predicateId "lsif.Range.1" factId
      [ "range" .= LSIF.toRange (LSIF.Range
          (LSIF.Position (toNat lineBegin) (toNat colBegin))
          (LSIF.Position (toNat lineBegin) (toNat colEnd)))
      , "text" .= LSIF.string (localIdent name)
      ]

  -- : `[startLine, startCharacter, endLine, endCharacter]`
decodeScipRange factId name [lineBegin,colBegin,lineEnd,colEnd] =
  LSIF.predicateId "lsif.Range.1" factId
      [ "range" .= LSIF.toRange (LSIF.Range
          (LSIF.Position (toNat lineBegin) (toNat colBegin))
          (LSIF.Position (toNat lineEnd) (toNat colEnd)))
      , "text" .= LSIF.string (localIdent name)
      ]

decodeScipRange _ _ _ = pure [] -- unknown/invalid range type

------------------------------------------------------------------------

mkDocumentFact :: LSIF.Id -> Scip.Document -> Parse [LSIF.Predicate]
mkDocumentFact id doc = LSIF.predicateId "lsif.Document.1" id
  [ "file" .= LSIF.string (doc ^. Scip.relativePath) -- src.File fact
  , "language" .= fromEnum (toLanguage (doc ^. Scip.relativePath))
  ]

data Occ = Occ
  { symFullName :: Text
  , symRoles :: Set Scip.SymbolRole
  , symScipSymbol :: Maybe ScipSymbol
  }

decodeOcc :: Scip.Occurrence -> Occ
decodeOcc occ = Occ{..}
  where
    symFullName = occ ^. Scip.symbol
    symRoles = toSymbolRole (occ ^. Scip.symbolRoles)
    symScipSymbol = case symbolFromString symFullName of
      Left _err -> Nothing
      Right x -> Just x

------------------------------------------------------------------------
--
-- Parser and ADT for Scip.Symbol strings.
-- These are well structured but stored as strings for reasons
--
{-

//   <symbol>               ::= <scheme> ' ' <package> ' ' { <descriptor> } | 'local ' <local-id>
//   <package>              ::= <manager> ' ' <package-name> ' ' <version>
//   <scheme>               ::= any UTF-8, escape spaces with double space.
//   <manager>              ::= same as above, use the placeholder '.' to indicate an empty value
//   <package-name>         ::= same as above
//   <version>              ::= same as above
//   <descriptor>           ::= <package> | <type> | <term> | <method> | <type-parameter> | <parameter> | <meta>
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
//   <escaped-characters>   ::= any UTF-8 character, escape backticks with double backtick.

-}

data ScipSymbol
  = Local Int
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
  , suffix :: !Suffix
  }

data Suffix
  = SymUnspecifiedSuffix
  | SymPackage
  | SymType
  | SymTerm
  | SymMethod
  | SymTypeParameter
  | SymParameter
  | SymMeta -- anything
  deriving Enum

-- These are all a bit wonky
kindFromSuffix :: Suffix -> LSIF.SymbolKind
kindFromSuffix s = case s of
  SymUnspecifiedSuffix -> LSIF.SkUnknown
  SymPackage -> LSIF.SkPackage
  SymType -> LSIF.SkClass
  SymTerm -> LSIF.SkVariable
  SymMethod -> LSIF.SkMethod
  SymTypeParameter -> LSIF.SkTypeParameter
  SymParameter -> LSIF.SkField
  SymMeta  -> LSIF.SkUnknown

-- https://github.com/sourcegraph/scip/blob/main/scip.proto#L81
-- e.g.
--
-- >  "scip-typescript npm new.docusaurus.io 2.0.0-beta.17 \
-- >      functionUtils/`playgroundUtils.ts`/CookieName."
--
symbolFromString :: Text -> Either Text ScipSymbol
symbolFromString str
  -- 'local ' <local-id>
  | ("local", rest) <- split normalStr
  = case textToInt rest of
      Left err -> Left (textShow err)
      Right n -> Right (Local n) -- locals are anonymous

  --  <scheme> ' ' <package> ' ' { <descriptor> }
  | (scheme, rest1) <- split normalStr
  -- <package> ::= <manager> ' ' <package-name> ' ' <version>
  , (manager, rest2) <- split rest1
  , (pkgname, rest3) <- split rest2
  , (version, symStrs) <- split rest3
  , (text, suffix) <- parseSuffix symStrs
  = Right $ Global scheme Package{..} Descriptor {..}

  | otherwise = Left $ "Unknown symbol: " <> str

  where
    -- we normalize space-escaping in identifiers with underscore
    normalStr = Text.intercalate "_" (Text.splitOn doubleSpace str)
      where doubleSpace = "  "

    split xs = case Text.breakOn " " xs of
      (tok, rest) -> (tok, if Text.null rest then rest else Text.tail rest)

parseSuffix :: Text -> (Text, Suffix)
parseSuffix "" = ("", SymUnspecifiedSuffix)
parseSuffix str = case Text.last str of
  '/' -> (sym, SymPackage)
  '#' -> (sym, SymType)
  '.' -> case Text.last sym of
    ')' -> (sym, SymMethod)
    _'  -> (sym, SymTerm)
  ':' -> (sym, SymMeta)
  ']' -> (sym, SymTypeParameter)
  ')' -> (sym, SymParameter)
  _ -> (str, SymUnspecifiedSuffix)
  where
    sym = Text.init str

-- Extract the identifier name from an encoded symbol
localIdent :: Maybe ScipSymbol -> Text
localIdent Nothing = ""
localIdent (Just Local{}) = ""
localIdent (Just Global{..}) = Text.takeWhileEnd (/= '/') (text descriptor)

{-
bitmask in i32

enum SymbolRole {
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
  [ if i == 0 then Just Scip.UnspecifiedSymbolRole else Nothing
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

