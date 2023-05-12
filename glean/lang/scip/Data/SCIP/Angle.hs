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
    scipToAngle
  ) where

import Data.Aeson ( object, KeyValue((.=)) )
import Lens.Micro ((^.))
import Data.Bits ( Bits(testBit) )
import Data.Maybe ( catMaybes, fromMaybe )
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

{-

Debug scip files directly via protoc. Assuming you the scip.proto file handy
From https://github.com/sourcegraph/scip/blob/main/scip.proto

> protoc --decode scip.Index scip.proto  < index.scip

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
decodeScipMetadata v = LSIF.predicate "lsif.Metadata" $
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
  mDocId <- getDefFactId (doc ^. Scip.relativePath)
  (docId, mFile) <- case mDocId of
    Nothing -> do
      docId <- nextId
      file <- mkDocumentFact docId doc
      return (docId, Just file)
    Just docId -> return (docId, Nothing)
  occs <- concat <$> mapM (decodeScipXRefs docId) (doc ^. Scip.occurrences)
  hovers <- concat <$> mapM decodeScipHovers (doc ^. Scip.symbols)
  pure $ fromMaybe [] mFile <> occs <> hovers

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
          a <- LSIF.predicate "lsif.Reference"
            [ "file" .= docId
            , "range" .= rangeId
            , "target" .= defId
            ]
          b <- LSIF.predicate "lsif.DefinitionUse"
            [ "target" .= defId
            , "file" .= docId
            , "range" .= rangeId
            ]
          return (a <> b)

      -- discard xrefs to unknown defs
        Nothing ->
          pure []
        -- this has the effect of discarding xrefs to not-yet indexed
        -- things. It might not be the right way to shard sets.
        --
        -- if we didn't find a definining occurence, then it is an external
        -- or 3rd party xref. we don't know the precise location
        -- we can likely _guess_ by parsing the symbol descriptor tho..

      pure (rangeFact <> refFacts)
    else pure []

decodeScipHovers :: Scip.SymbolInformation -> Parse [LSIF.Predicate]
decodeScipHovers symInfo = do
  let symFullName = symInfo ^. Scip.symbol
  mDefId <- getDefFactId symFullName
  case mDefId of
    Nothing -> -- this is likely the hover text for a third-party xref
         pure [] -- for now, discard, as we don't have an anchor xref
    Just defId -> concat <$> do
      forM (symInfo ^. Scip.documentation) $ \docstr -> do
        hoverId <- nextId
        a <- LSIF.predicateId "lsif.HoverContent" hoverId
          [ "text" .= LSIF.string docstr
          , "language" .= fromEnum LSIF.UnknownLanguage -- could parse docstr
          ]
        b <- LSIF.predicate "lsif.DefinitionHover"
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
      rangeId <- nextId
      rangeFact <- decodeScipRange rangeId symScipSymbol (occ ^. Scip.range)

      -- emit a definition fact
      defId <- nextId
      defFact <- LSIF.predicateId "lsif.Definition" defId
        [ "file" .= docId
        , "range" .= rangeId
        ]

      kindFact <- case symScipSymbol of
        Nothing -> pure []
        Just Local{} ->
          LSIF.predicate "lsif.DefinitionKind"
            [ "defn" .= defId
            , "kind" .= fromEnum LSIF.Local
            ]
        Just Global{..} ->
          LSIF.predicate "lsif.DefinitionKind"
            [ "defn" .= defId
            , "kind" .= fromEnum (LSIF.kindFromSuffix (suffix descriptor))
            ]

      -- record symbol -> fact id for xref use later
      setDefFact symFullName defId

      -- we know its a def, so generate the moniker facts
      monikerFacts <- case symScipSymbol of
        Nothing -> do
          LSIF.predicate "lsif.DefinitionMoniker"
            [ "defn" .= defId
            ]
        Just Local{} -> do
          LSIF.predicate "lsif.DefinitionMoniker"
            [ "defn" .= defId
            ] -- these are anonymous locals with an integer.

        Just Global{..} -> do
          monikerId <- nextId
          monikerFact <- LSIF.predicateId "lsif.Moniker" monikerId
            [ "kind" .= fromEnum (
                          if Scip.Import `Set.member` symRoles
                          then LSIF.Import else LSIF.Export )
            , "scheme" .= LSIF.string scheme
            , "ident" .= LSIF.string symFullName
            ]

          entityFact <- LSIF.predicate "lsif.DefinitionMoniker"
            [ "defn" .= defId
            , "moniker" .= monikerId
            ]

          -- can throw in a (highly duplicated) package information fact
          pkgInfoFact <- LSIF.predicate "lsif.PackageInformation"
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

  -- [startLine, startCharacter, endCharacter]`. The end line
  --  is inferred to have the same value as the start line.
decodeScipRange
  :: LSIF.Id -> Maybe ScipSymbol -> [Int32] -> Parse [LSIF.Predicate]
decodeScipRange factId name [lineBegin,colBegin,colEnd] =
  LSIF.predicateId "lsif.Range" factId
      [ "range" .= LSIF.toRange (LSIF.Range
          (LSIF.Position (toNat lineBegin) (toNat colBegin))
          (LSIF.Position (toNat lineBegin) (toNat colEnd)))
      , "text" .= LSIF.string (localIdent name)
      ]

  -- : `[startLine, startCharacter, endLine, endCharacter]`
decodeScipRange factId name [lineBegin,colBegin,lineEnd,colEnd] =
  LSIF.predicateId "lsif.Range" factId
      [ "range" .= LSIF.toRange (LSIF.Range
          (LSIF.Position (toNat lineBegin) (toNat colBegin))
          (LSIF.Position (toNat lineEnd) (toNat colEnd)))
      , "text" .= LSIF.string (localIdent name)
      ]

decodeScipRange _ _ _ = pure [] -- unknown/invalid range type

------------------------------------------------------------------------

mkDocumentFact :: LSIF.Id -> Scip.Document -> Parse [LSIF.Predicate]
mkDocumentFact id doc = do
  let filepath = doc ^. Scip.relativePath
  setDefFact filepath id -- record file -> fact id for xref use later
  LSIF.predicateId "lsif.Document" id
    [ "file" .= LSIF.string filepath
    , "language" .= fromEnum (LSIF.parseLanguage (doc ^. Scip.language))
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
  , suffix :: !LSIF.Suffix
  }

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
  , (text, suffix) <- LSIF.parseSuffix symStrs
  = Right $ Global scheme Package{..} Descriptor {..}

  | otherwise = Left $ "Unknown symbol: " <> str

  where
    -- we normalize space-escaping in identifiers with underscore
    normalStr = Text.intercalate "_" (Text.splitOn doubleSpace str)
      where doubleSpace = "  "

    split xs = case Text.breakOn " " xs of
      (tok, rest) -> (tok, if Text.null rest then rest else Text.tail rest)

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
