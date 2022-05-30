{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Schema.Gen.Utils
  ( -- * Types
    unitT
  , isByt
  , shouldNameKeyType
    -- * Dependency analysis
  , SomeDecl(..)
  , orderDecls
    -- * Names and namespaces
  , NameSpaces
  , splitDot
  , joinDot
  , dotted
  , underscored
  , sortDeclsByNamespace
    -- * Text utils
  , myUnlines
  , newline
  , cap1
  , low1
    -- * Monad for code generation
  , M
  , Env
  , Mode(..)
  , NamePolicy(..)
  , mkNamePolicy
  , runM
  , getMode
  , isQuery
  , typeDef
  , repType
  , NewOrOld(..)
  , nameThisType
  , withPredicateDefHint
  , withRecordFieldHint
  , withUnionFieldHint
  , withTypeDefHint
  , withTopLevelHint
  , pushDefs
  , popDefs
  , predicateName
  , typeName
  , thriftName
  , haskellThriftName
  , addNamespaceDependencies
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Graph
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import TextShow

import Util.List

import Glean.Angle.Types
import Glean.Schema.Types
import Glean.Schema.Util


-- -----------------------------------------------------------------------------
-- Types

unitT :: ResolvedType
unitT = Record []

isByt :: ResolvedType -> Bool
isByt Byte = True
isByt _ = False

-- | Return 'True' if we should generate a new type in Thrift for a
-- predicate key type.  If the type is a base type or is already a
-- typedef, there's no need to generate a new named type.
shouldNameKeyType :: ResolvedType -> Bool
shouldNameKeyType Record{} = True
shouldNameKeyType Sum{} = True
shouldNameKeyType _ = False

-- -----------------------------------------------------------------------------
-- Dependency analysis of decls

data SomeDecl
  = PredicateDecl ResolvedPredicateDef
  | TypeDecl ResolvedTypeDef

type Node = (Name,Version)

-- | Sort the declarations to keep C++ Happy.
orderDecls :: [SomeDecl] -> [SomeDecl]
orderDecls decls = map betterNotBeAnyCyclesIn sccs
 where
  betterNotBeAnyCyclesIn (AcyclicSCC one) = one
  betterNotBeAnyCyclesIn (CyclicSCC defs) =
    error $ "cycle: " ++ unwords (map (show . declNode) defs)

  sccs = stronglyConnComp
    [ (decl, declNode decl, outEdges decl) | decl <- decls ]

  declNode :: SomeDecl -> (Name,Version)
  declNode (PredicateDecl PredicateDef{..}) =
    (predicateRef_name predicateDefRef, predicateRef_version predicateDefRef)
  declNode (TypeDecl TypeDef{..}) =
    (typeRef_name typeDefRef, typeRef_version typeDefRef)

  outEdges :: SomeDecl -> [Node]
  outEdges d = case d of
    PredicateDecl PredicateDef{..} ->
      outEdgesTs [predicateDefKeyType, predicateDefValueType]
    TypeDecl TypeDef{..} ->
      outEdgesT typeDefType

  outEdgesTs = concatMap outEdgesT
  outEdgesFields fields = outEdgesTs [ ty | FieldDef _ ty <- fields ]

  outEdgesT :: ResolvedType -> [Node]
  outEdgesT Byte{} = []
  outEdgesT Nat{} = []
  outEdgesT Boolean{} = []
  outEdgesT String{} = []
  outEdgesT (Array ty) = outEdgesT ty
  outEdgesT (Maybe ty) = outEdgesT ty
  outEdgesT (Record fields)  = outEdgesFields fields
  outEdgesT (Sum fields)  = outEdgesFields fields
  outEdgesT (NamedType (TypeRef name ver)) = [(name,ver)]
  outEdgesT Predicate{} = [] -- See Note [predicate type references]
  outEdgesT Enumerated{} = []

{- Note [predicate type references]

In a struct definition, references to predicates look like this:

  struct SrcLoc {
    Fact<File> file;
    ...
  }

The structure of Fact<P> doesn't depend on the full definition of
P, it's enough to have a "struct P;" declaration, which we have for
all predicates.

In a predicate definition, references to other predicates are like this:

  struct Target : Predicate<Sys::Blob> {..}

And Predicate<> contains types only, so this also doesn't need the
full definition of Sys::Blob.

Therefore, we conclude that predicate references do not constitute a
dependency for the purposes of ordering definitions, and this enables
us to break all cycles involving predicates.  There can of course be
no cycles between typedefs; these will cause an error.
-}

-- -----------------------------------------------------------------------------
-- Names and namespaces

-- Convenience to split a name

joinDot :: (NameSpaces, Text) -> Text
joinDot (ns,t) = Text.intercalate "." (ns ++ [t])

dotted :: NameSpaces -> Text
dotted = Text.intercalate "."

underscored :: NameSpaces -> Text
underscored = Text.intercalate "_"

sortDeclsByNamespace
  :: [ResolvedPredicateDef]
  -> [ResolvedTypeDef]
  -> HashMap NameSpaces ([ResolvedPredicateDef], [ResolvedTypeDef])
sortDeclsByNamespace preds types =
  HashMap.fromListWith combine $
    [ (namespace, ([pred],[]))
    | pred@PredicateDef{..} <- preds
    , let (namespace,_) = splitDot (predicateRef_name predicateDefRef) ] ++
    [ (namespace, ([],[tdef]))
    | tdef@TypeDef{..} <- types
    , let (namespace,_) = splitDot (typeRef_name typeDefRef)  ]
  where
    combine (as,bs) (cs,ds) = (as++cs, bs++ds)

-- -----------------------------------------------------------------------------
-- Text Utils

-- | Better than Text.unlines for codegen purposes
myUnlines :: [Text] -> Text
myUnlines = Text.intercalate newline
          . map Text.stripEnd
          . dropWhile Text.null . dropWhileEnd Text.null
  where
    dropWhileEnd :: (a -> Bool) -> [a] -> [a]
    dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

newline :: Text
newline = Text.pack "\n"

cap1 :: Text -> Text
cap1 tIn = if Text.null tIn then tIn else
  let (h, t) = Text.splitAt 1 tIn
  in Text.toUpper h <> t

low1 :: Text -> Text
low1 tIn = if Text.null tIn then tIn else
  let (h, t) = Text.splitAt 1 tIn
  in Text.toLower h <> t

-- -----------------------------------------------------------------------------

-- | Thrift must turn tuples into named structures.  Solve the hard
-- problem of naming inside monad M
type M = ReaderT Env (State ExtraDefs)

type ExtraDefs = [Text]

data Mode = Data | Query
  deriving (Eq, Show)

data Env = Env
  { mode :: Mode  -- ^ whether to generate the query format
  , nameHint :: [Text]
      -- ^ stack of "hints": name components that will be used to
      -- to name an anonymous type from the schema. The hint at
      -- any given point is unique: a type generated from this name
      -- will not clash with anything else.
      --
      -- Note: the 'Text' are sometimes 'Safe' and sometimes not 'Safe'
  , namePolicy :: NamePolicy
  , typeEnv :: HashMap (Name, Version) ResolvedTypeDef
  }

data NamePolicy = NamePolicy
  { predNames :: HashMap PredicateRef (NameSpaces,Text)
  , typeNames :: HashMap TypeRef (NameSpaces,Text)
  }

mkNamePolicy :: [ResolvedPredicateDef] -> [ResolvedTypeDef] -> NamePolicy
mkNamePolicy preds types = NamePolicy{..}
  where
  nameMap = HashMap.fromListWith (++) $
    [ (predicateRef_name predicateDefRef,
       [predicateRef_version predicateDefRef])
    | PredicateDef{..} <- preds ] ++
    [ (typeRef_name typeDefRef, [typeRef_version typeDefRef])
    | TypeDef{..} <- types ]

  -- Figure out how we're going to name each predicate. The most
  -- recent version of P gets to be called P, all the older versions
  -- are called P_v.
  predNames = HashMap.fromList
    [ let
        (ns, name) = splitDot predicateRef_name
      in
      if predicateRef_version == maximum versions
        then (predicateDefRef, (ns, name))
        else (predicateDefRef, (ns, name <> "_" <> showt predicateRef_version))
    | PredicateDef{..} <- preds
    , let PredicateRef{..} = predicateDefRef
    , Just versions <- [HashMap.lookup predicateRef_name nameMap] ]

  -- Similarly for type names.
  typeNames = HashMap.fromList
    [ let
        (ns, name) = splitDot typeRef_name
      in
      if typeRef_version == maximum versions
        then (typeDefRef, (ns, name))
        else (typeDefRef, (ns, name <> "_" <> showt typeRef_version))
    | TypeDef{..} <- types
    , let TypeRef{..} = typeDefRef
    , Just versions <- [HashMap.lookup typeRef_name nameMap] ]


-- | Look up the name to use for a predicate, taking into account versions
predicateName :: Monad m => PredicateRef -> ReaderT Env m (NameSpaces, Text)
predicateName pred = do
  Env{..} <- ask
  case HashMap.lookup pred (predNames namePolicy) of
    Just (ns,x) -> return (ns,x)
    _ -> error $ "predicateThriftName: " ++ show pred

-- | Look up the name to use for a type, taking into account versions
typeName :: Monad m => TypeRef -> ReaderT Env m (NameSpaces, Text)
typeName typeref = do
  Env{..} <- ask
  case HashMap.lookup typeref (typeNames namePolicy) of
    Just (ns,x) -> return (ns,x)
    Nothing ->
      -- it's one we invented, the name should be unique
      return (splitDot (typeRef_name typeref))

-- | The textual name to use for an entity relative to the current namespace
thriftName :: Mode -> NameSpaces -> (NameSpaces, Text) -> Text
thriftName mode here (ns, x)
  | here == ns = getSafe $ safeThrift mode x
  | otherwise = getSafe $ case ns of
    [] -> checkSafe ("thriftName " ++ show (mode,here,(ns,x))) mode x
    _  -> Safe $ underscored ns <> "." <> getSafe (safeThrift mode x)

-- | Witness that the name is fully mangled (see 'safeThrift' for the
-- defining smart constructor)
newtype Safe x = Safe { getSafe :: x }

-- | This asserts the input already qualifies as 'Safe' and that 'safeThift'
-- would not transform it.  If 'safeThift' would transform the input then
-- this throws an error.
checkSafe :: String -> Mode -> Text -> Safe Text
checkSafe msg mode x =
  let safe = safeThrift mode x
  in if x == getSafe safe
        then safe
        else error $ msg <> " : " <> show x <> " not really Safe in checkSafe"

-- | alas, we discovered that "Type" clashes with the Thrift-generated
-- C++ code, but only for the query types. It was too late to change
-- the schemas without a major upheaval, so we munge the name here.
--
-- This step acts to define the meaning of the 'Safe' newtype
safeThrift :: Mode -> Text -> Safe Text
safeThrift Query "Type" = Safe "Type_"
safeThrift _ x = Safe x

-- The name of the Thrift type as it appears in Haskell
haskellThriftName :: Mode -> (NameSpaces, Text) -> Text
haskellThriftName query (ns, x) =
  dotted $
    prefix ++
    [ Text.concat (map cap1 ns), "Types", getSafe $ safeThrift query x ]
  where
  prefix = "Glean" : "Schema" : case query of
    Data -> []
    Query -> ["Query"]

runM
  :: Mode
  -> s
  -> NamePolicy
  -> [ResolvedTypeDef]
  -> ReaderT Env (State s) a
  -> (a, s)

runM mode initState namePolicy types act = (result, finalState)
  where
    (result, finalState) = runState (runReaderT act env) initState
    env = Env
      { mode = mode
      , namePolicy = namePolicy
      , nameHint = mempty
      , typeEnv = HashMap.fromList
          [ ((typeRef_name typeDefRef, typeRef_version typeDefRef), typedef)
          | typedef@TypeDef{..} <- types ]
      }


getMode :: Monad m => ReaderT Env m Mode
getMode = mode <$> ask

isQuery :: Mode -> Bool
isQuery = (== Query)

-- | Returned by 'nameThisType'
data NewOrOld = New | Old
  deriving (Eq, Ord)

-- Make a name for a type. The returned name is assumed to be in the
-- current namespace.
nameThisType :: Monad m => ResolvedType -> ReaderT Env m (NewOrOld, Text)
nameThisType (Record []) = return (Old, "builtin.Unit")
nameThisType _ = do
  Env{..} <- ask
  let
    name = Text.intercalate "_" $ case reverse nameHint of
      (h:hs) -> cap1 h : hs
      [] -> error "nameThisType: empty hint"
  return (New, name)

repType :: Monad m => ResolvedType -> ReaderT Env m (Maybe ResolvedType)
repType (NamedType tr) = do
  maybeTy <- typeDef tr
  case maybeTy of
    Nothing -> return Nothing
    Just ty -> repType ty
repType t = return (Just t)

typeDef :: Monad m => TypeRef -> ReaderT Env m (Maybe ResolvedType)
typeDef TypeRef{..} = do
  te <- typeEnv <$> ask
  case HashMap.lookup (typeRef_name, typeRef_version) te of
    Just TypeDef{..} -> return (Just typeDefType)
    Nothing -> return Nothing -- we don't know


-- | set the 'hint' (in a nested scope) for 'nameThisType' while running 'act'
withHint :: Monad m => Text -> ReaderT Env m a -> ReaderT Env m a
withHint hint = local pushHint
  where pushHint s = s{ nameHint = hint : nameHint s }

-- | For a typedef, we only add this as a hint component if we're at
-- the top level and therefore generating a user-supplied
-- typedef. Otherwise we will get the names of generated typedefs as
-- hints themselves.
withTypeDefHint :: Monad m => Text -> ReaderT Env m a -> ReaderT Env m a
withTypeDefHint hint = local pushHint
  where
  pushHint s = s{ nameHint = if null (nameHint s) then [hint] else nameHint s }

withTopLevelHint :: Monad m => Text -> ReaderT Env m a -> ReaderT Env m a
withTopLevelHint hint = local $ \s -> s { nameHint = [hint] }

withPredicateDefHint :: Monad m => Text -> ReaderT Env m a -> ReaderT Env m a
withPredicateDefHint = withHint

withRecordFieldHint :: Monad m => Text -> ReaderT Env m a -> ReaderT Env m a
withRecordFieldHint = withHint

{- |
For a union field, there's a potential clash to avoid:

> union T {
>    T_x x;
> }
>
> struct T_x {  // generated type
>   ...
> }

We cannot use T_x for the name of the generated type, because it
clashes with the T_x constructor that the hs2 thrift compiler
generates for the field x of T.

So, we add another underscore to the hint for a union field, giving us
T_x_.
-}
withUnionFieldHint :: Monad m => Text -> ReaderT Env m a -> ReaderT Env m a
withUnionFieldHint p = withHint (p <> "_")

pushDefs :: [Text] -> M ()
pushDefs ts = modify (reverse ts ++)

popDefs :: M [Text]
popDefs = state $ \ s -> (reverse s, mempty)


addNamespaceDependencies
  :: HashMap NameSpaces ([ResolvedPredicateDef], [ResolvedTypeDef])
  -> HashMap NameSpaces ([NameSpaces], [ResolvedPredicateDef], [ResolvedTypeDef])
addNamespaceDependencies nss =
  HashMap.fromList
    [ (ns, (outEdges ns preds types, preds, types))
    | (ns, (preds, types)) <- HashMap.toList nss
    ]
  where
  outEdges ns preds types =
    uniq $ filter (/= ns) $
      ["builtin"] :  -- builtin is always a dep, so we can get builtin.Unit
      concat (map outEdgesPred preds ++ map outEdgesTypeDef types)

  outEdgesPred :: ResolvedPredicateDef -> [NameSpaces]
  outEdgesPred PredicateDef{..} =
    outEdgesTs [predicateDefKeyType, predicateDefValueType]

  outEdgesTypeDef :: ResolvedTypeDef -> [NameSpaces]
  outEdgesTypeDef TypeDef{..} = outEdgesT typeDefType

  outEdgesTs = concatMap outEdgesT
  outEdgesFields fields = outEdgesTs [ ty | FieldDef _ ty <- fields ]

  outEdgesT :: ResolvedType -> [NameSpaces]
  outEdgesT Byte{} = []
  outEdgesT Nat{} = []
  outEdgesT Boolean{} = []
  outEdgesT String{} = []
  outEdgesT (Array ty) = outEdgesT ty
  outEdgesT (Maybe ty) = outEdgesT ty
  outEdgesT (Record fields)  = outEdgesFields fields
  outEdgesT (Sum fields)  = outEdgesFields fields
  outEdgesT (NamedType (TypeRef name _)) = [fst (splitDot name)]
  outEdgesT (Predicate (PredicateRef name _)) = [fst (splitDot name)]
  outEdgesT Enumerated{} = []
