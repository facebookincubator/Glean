{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
module HieIndexer.Index (indexHieFile) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Default
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Unboxed as Vector
import Compat.HieTypes (HieFile(..))
import HieDb.Compat (nameModule_maybe, nameOccName)
import System.Directory
import System.FilePath
import Control.Monad.Extra (findM)

import qualified GHC
import GHC.Iface.Ext.Utils (generateReferencesMap)
import GHC.Iface.Ext.Types (
  getAsts, ContextInfo(..), IdentifierDetails(..), RecFieldContext(..),
  BindType(..), DeclType(..), IEType(..))
import qualified GHC.Types.Name.Occurrence as GHC
import qualified GHC.Types.Name as GHC (isSystemName)
import GHC.Unit.Types (unitFS)
import qualified GHC.Unit.Module.Name as GHC (moduleNameFS)
import qualified GHC.Data.FastString as GHC (FastString, bytesFS, mkFastString)

import Util.Log

import qualified Glean
import Glean.Impl.ConfigProvider ()
import qualified Glean.Schema.Hs.Types as Hs
import qualified Glean.Schema.Src.Types as Src
import Glean.Util.Range

{- TODO

- issues with record fields
  - DuplicateRecordFields generates names like $sel:field:Rec
  - weird references to the record constructor from field decls
  - why do we get a ref for the field decl?
  - span of record field in pattern match is wrong

- class methods
  - method ref should point to the class method binder
  - (Haddock hyperlinker doesn't do this either)

- types
  - types of decls (e.g. ValDecls)
  - signatures
  - types on refs

- declarations
  - details of declarations (e.g. class methods, constructors, fields)
    - doing this properly is a lot of work, and it's hard to get this
      info from Hie. A possible way to do this is using the Haddock API:
      Haddock's Interface type has all the ASTs for the declarations
      in addition to the Hie.

- exclude generated names in a cleaner way

- Glass / codemarkup
  - search by name
  - type signatures for symbols (needs types)
  - should modules be symbols?
  - SymbolKind, Visibility, etc.
  - RelationType_Contains child/parent (needs declarations)
  - full signatures for symbols (needs declarations)
  - Haddock docs for symbol
-}

mkModule :: Glean.NewFact m => GHC.Module -> m Hs.Module
mkModule mod = do
  modname <- Glean.makeFact @Hs.ModuleName $
    fsToText (GHC.moduleNameFS (GHC.moduleName mod))
  unitname <- Glean.makeFact @Hs.UnitName $
    fsToText (unitFS (GHC.moduleUnit mod))
  Glean.makeFact @Hs.Module $
    Hs.Module_key modname unitname

mkName :: Glean.NewFact m => GHC.Name -> Hs.Module -> Hs.NameSort -> m Hs.Name
mkName name mod sort = do
  let occ = nameOccName name
  occFact <- Glean.makeFact @Hs.OccName $
    Hs.OccName_key (fsToText (GHC.occNameFS occ)) (toNamespace occ)
  Glean.makeFact @Hs.Name $
    Hs.Name_key occFact mod sort

srcSpanToSrcRange :: Src.File -> GHC.RealSrcSpan -> Src.Range
srcSpanToSrcRange file sp =
  Src.Range file
    (Glean.Nat $ fromIntegral $ GHC.srcSpanStartLine sp)
    (Glean.Nat $ fromIntegral $ GHC.srcSpanStartCol sp)
    (Glean.Nat $ fromIntegral $ GHC.srcSpanEndLine sp)
    (Glean.Nat $ fromIntegral $ GHC.srcSpanEndCol sp - 1)
    -- GHC.RealSrcSpan is exclusive while Src.Range is inclusive, so
    -- we subtract one from the end column when converting.

fsToText :: GHC.FastString -> Text
fsToText = Text.decodeUtf8 . GHC.bytesFS

toNamespace :: GHC.OccName -> Hs.Namespace
toNamespace occ
  | GHC.isVarOcc occ = Hs.Namespace_var_
  | GHC.isTvOcc occ = Hs.Namespace_tyvar
  | GHC.isTcOcc occ = Hs.Namespace_tycon
  | GHC.isDataOcc occ = Hs.Namespace_datacon
  | otherwise = error "toNamespace"

produceDecl
 :: Glean.NewFact m
 => Hs.Name
 -> ContextInfo
 -> m ()
produceDecl name ctx = case ctx of
  ValBind RegularBind _ _ ->
    Glean.makeFact_ @Hs.ValBind $ Hs.ValBind_key name
  Decl FamDec _ ->
    Glean.makeFact_ @Hs.TypeFamilyDecl $ Hs.TypeFamilyDecl_key name
  Decl SynDec _ ->
    Glean.makeFact_ @Hs.TypeSynDecl $ Hs.TypeSynDecl_key name
  Decl DataDec _ ->
    Glean.makeFact_ @Hs.DataDecl $ Hs.DataDecl_key name
  Decl ConDec _ ->
    Glean.makeFact_ @Hs.ConDecl $ Hs.ConDecl_key name
  Decl PatSynDec _ ->
    Glean.makeFact_ @Hs.PatSynDecl $ Hs.PatSynDecl_key name
  Decl ClassDec _ ->
    Glean.makeFact_ @Hs.ClassDecl $ Hs.ClassDecl_key name
  Decl InstDec _ ->
    Glean.makeFact_ @Hs.InstanceDecl $ Hs.InstanceDecl_key name
  PatternBind{} ->
    Glean.makeFact_ @Hs.PatBind $ Hs.PatBind_key name
  TyVarBind{} ->
    Glean.makeFact_ @Hs.TyVarBind $ Hs.TyVarBind_key name
  ClassTyDecl{} ->
    Glean.makeFact_ @Hs.MethodDecl $ Hs.MethodDecl_key name
  RecField{} ->
    Glean.makeFact_ @Hs.RecFieldDecl $ Hs.RecFieldDecl_key name
  _ -> return ()

indexHieFile
  :: Glean.Writer
  -> NonEmpty Text
  -> FilePath
  -> HieFile
  -> IO ()
indexHieFile writer srcPaths path hie = do
  srcFile <- findSourceFile srcPaths (hie_module hie) (hie_hs_file hie)
  logInfo $ "Indexing: " <> path <> " (" <> srcFile <> ")"
  Glean.writeFacts writer $ do
    modfact <- mkModule smod

    let offs = getLineOffsets (hie_hs_src hie)
    let hsFileFS = GHC.mkFastString $ hie_hs_file hie
    filefact <- Glean.makeFact @Src.File (Text.pack srcFile)
    let fileLines = mkFileLines filefact offs
    Glean.makeFact_ @Src.FileLines fileLines

    Glean.makeFact_ @Hs.ModuleSource $
      Hs.ModuleSource_key modfact filefact

    let toByteRange = srcRangeToByteRange fileLines (hie_hs_src hie)
        toByteSpan sp
          | GHC.srcSpanEndLine sp >= Vector.length (lineOffsets offs) =
            Src.ByteSpan (Glean.toNat 0) (Glean.toNat 0)
          | otherwise =
            rangeToByteSpan (toByteRange (srcSpanToSrcRange filefact sp))

    let allIds = [ (n, p) | (Right n, ps) <- Map.toList refmap, p <- ps ]

    -- produce names & declarations
    names <- fmap catMaybes $ forM allIds $ \(name, (span, dets)) -> if
      | Just sp <- getBindSpan span (identInfo dets)
      , localOrGlobal name smod
      , GHC.srcSpanFile sp == hsFileFS -> do -- Note [#included source files]
        let byteSpan = toByteSpan sp
        sort <- case nameModule_maybe name of
          Nothing -> return $ Hs.NameSort_internal byteSpan
          Just{} -> return $ Hs.NameSort_external def
        namefact <- mkName name modfact sort
        Glean.makeFact_ @Hs.DeclarationLocation $
          Hs.DeclarationLocation_key namefact filefact byteSpan
        {-
          trace ("decl: " <>
            GHC.occNameString (nameOccName name) <> ": " <>
           show (ppr sp)) $ return ()
        -}
        mapM_ (produceDecl namefact) (Set.toList (identInfo dets))
        return $ Just (name, namefact)
      | otherwise -> return Nothing

    -- A map for Names defined in this module
    let
      nameMap :: Map GHC.Name Hs.Name
      nameMap = Map.fromList names

    Glean.makeFact_ @Hs.ModuleDeclarations $ Hs.ModuleDeclarations_key
      modfact (map snd names)

    let refs = Map.fromListWith (Map.unionWith (++))
          [ (n, Map.singleton kind [span])
          | (n, (span, dets)) <- allIds,
            Just kind <- map isRef (Set.toList (identInfo dets)),
            not (GHC.isSystemName n),
            -- TODO: we should exclude generated names in a cleaner way
            not (GHC.isDerivedOccName (nameOccName n))
          ]

    refs <- fmap catMaybes $ forM (Map.toList refs) $ \(name, kindspans) -> do
      maybe_namefact <-
        case Map.lookup name nameMap of
          Just fact -> return $ Just fact
          Nothing -> case nameModule_maybe name of
            Nothing ->
              -- This shouldn't happen, it's probably a bug in the hie file.
              -- But it does happen, so let's not crash.
              return Nothing
            Just mod -> do
              namemod <- mkModule mod
              Just <$> mkName name namemod (Hs.NameSort_external def)
      forM maybe_namefact $ \namefact -> do
        refspans <- forM (Map.toList kindspans) $ \(kind, spans) -> do
          let gleanspans = map toByteSpan spans
          return $ map (Hs.RefSpan kind) gleanspans
        Glean.makeFact @Hs.Reference $
          Hs.Reference_key namefact (concat refspans)

    Glean.makeFact_ @Hs.FileXRefs $ Hs.FileXRefs_key filefact refs

  where
  smod = hie_module hie
  refmap = generateReferencesMap $ getAsts $ hie_asts hie

  localOrGlobal :: GHC.Name -> GHC.Module -> Bool
  localOrGlobal name mod = case nameModule_maybe name of
    Nothing -> True
    Just nameMod
      | nameMod == mod -> True
      | otherwise -> False

  -- returns True if this ContextInfo is a reference
  isRef Use = Just Hs.RefKind_coderef
  isRef (RecField r _) | isRecFieldRef r = Just Hs.RefKind_coderef
  isRef (ValBind InstanceBind _ _) = Just Hs.RefKind_coderef
    -- treat these as refs, not binds
  isRef TyDecl{} = Just Hs.RefKind_coderef
  isRef (IEThing Export) = Just Hs.RefKind_exportref
  isRef (IEThing _) = Just Hs.RefKind_importref
  isRef _ = Nothing

  isRecFieldRef RecFieldAssign = True
  isRecFieldRef RecFieldMatch = True
  isRecFieldRef RecFieldOcc = True
  isRecFieldRef _ = False

  -- returns (Just span) if this ContextInfo is a definition
  getBindSpan defaultSpan = getFirst . foldMap (First . goDecl)
    where
    goDecl (ValBind RegularBind _ sp) = sp
    goDecl (PatternBind _ _ sp) = sp <|> Just defaultSpan
    goDecl (Decl _ sp) = sp
    goDecl (RecField r sp) | not (isRecFieldRef r) = sp
    goDecl TyVarBind{} = Just defaultSpan
    goDecl (ClassTyDecl sp) = sp
    goDecl _ = Nothing

-- |
-- Attempt to find the original source file given the hie_hs_src value
-- from the .hie file, the module, and the --src flags provided to the
-- indexer.
--
-- See Note [source file paths]
--
findSourceFile :: NonEmpty Text -> GHC.Module -> FilePath -> IO FilePath
findSourceFile srcPaths mod src = do
  r <- findM doesFileExist
    (fmap ((</> src) . Text.unpack) (NonEmpty.toList spliced))
  cwd <- getCurrentDirectory
  -- normalise because some paths are of the form ./A/B/C.hs
  -- makeRelative because generated files can have absolute paths
  makeRelative cwd . normalise <$> case r of
    Nothing -> do
      logWarning $ "couldn't find src for: " <> src
      return src
    Just f -> return f
  where
  pkg = fsToText (unitFS (GHC.moduleUnit mod))
  isVer = Text.all (\c -> isDigit c || c == '.')
  pkgNameAndVersion = case break isVer (Text.splitOn "-" pkg) of
    (before, after) -> Text.intercalate "-" (before <> take 1 after)
  spliced = fmap (Text.replace "$PACKAGE" pkgNameAndVersion) srcPaths

{-
Note [source file paths]

We need
- source file paths for src.File facts
- source file contents so that we can create src.FileLines and convert
  line/column to bytespan

A src.File should uniquely determine the hs.Module, and should be
relative to the root of the project we're indexing.

The .hie file contains the file name of the original .hs file as seen
by GHC, together with its contents as a ByteString.

The source file path in the .hie file isn't exactly what we need, for
a few reasons:

1. If the project has multiple packages (with a cabal.project), then
the source file names in the .hie files will be relative to each
package. In that case we need to find the original source file to
generate the `src.File` fact. This is done by passing a @--src@ flag to
the indexer, e.g. @--src '$PACKAGE'@.

2. The path often needs normalising, e.g. it's common to see paths
like ./src/A/B/C.hs

3. Sometimes the file path in the .hie file is absolute, but we need
to make it relative, e.g. the Paths_foo.hs modules generated by Cabal
will have absolute path names like
/a/b/c/dist-newstyle/build/x86_64-linux/ghc-9.4.7/HUnit-1.6.2.0/build/autogen/Paths_HUnit.hs
-}

{-
Note [#included source files]

There might be other source files involved when compiling a module,
e.g. if CPP is being used and the .hs file uses `#include`. We
currently ignore Names that come from another source file (TODO).
-}
