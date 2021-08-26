-- Copyright (c) Facebook, Inc. and its affiliates.

-- | Generate C++ client types from the Glean schema

{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Glean.Schema.Gen.Cpp
  ( genSchemaCpp
  ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import TextShow

import Glean.Schema.Gen.Utils hiding (pushDefs, popDefs)
import Glean.Angle.Types hiding (schemaName)


genSchemaCpp :: Version -> [PredicateDef] -> [TypeDef] -> [(FilePath,Text)]
genSchemaCpp version preddefs typedefs =
  [("", leading <> newline <> newline <> body)]
  where
    namePolicy = mkNamePolicy preddefs typedefs
    someDecls = map PredicateDecl preddefs ++ map TypeDecl typedefs
    ordered = orderDecls someDecls
    ((ds,schema), extra) = runM Data [] namePolicy typedefs $ do
      ds <- mapM genDecl ordered
      sc <- defineSchema ordered
      return (ds, sc)
    decls = concat ds ++ reverse extra
    pieces = withNS $
      [(schemaNamespace, "constexpr int version = " <> showt version <> ";")]
      ++ forwardDeclare decls
      ++ [(schemaNamespace, schema)]

    body = Text.intercalate (newline <> newline) pieces

-- Check against hardcoded list of what glean.h provides
provided :: (NameSpaces, Text) -> Maybe Type
provided (_,ident) = Map.lookup ident known
  where known = Map.fromList [("Unit", unitT)]

leading :: Text
leading = Text.unlines
  ["#pragma once"
  ,"// @" <> "generated"
  ,"// Glean.Schema.Gen.Cpp definitions for fbcode/glean/lang/clang/schema.h"
  ,"// by //glean/hs:predicates using --cpp"
  ,""
  ,"#include <tuple>"
  ,"#include <boost/variant.hpp>"
  ,"#include \"glean/lang/clang/common.h\""
  ,""
  ]

forwardDeclare
  :: [(NameSpaces, (Text,Text))]
  -> [(NameSpaces, Text)]
forwardDeclare xs =
  declarations xs
  ++ definitions xs
  where
    declarations = filter (not . Text.null . snd) . map (fmap fst)
    definitions = map (fmap snd)

defineSchema :: [SomeDecl] -> CppGen Text
defineSchema ds = do
  let preds = [ p | PredicateDecl p <- ds ]
  pnames <- forM preds $ \PredicateDef{..} -> do
    cppNameIn schemaNamespace . schemaName <$> predicateName predicateDefRef
  return $ Text.unlines $ concat
    [ ["struct SCHEMA {"]
    ,indentLines
      ["template<typename P> struct index;"]
    ,indentLines
      ["template<> struct index<"
        <> p
        <> "> { static constexpr size_t value = "
        <> Text.pack (show i)
        <> "; };" | (i,p) <- zip [0::Int ..] pnames]
    ,[""]
    ,indentLines
      ["static constexpr size_t count = "
        <> Text.pack (show $ length pnames)
        <> ";"]
    ,[""]
    ,indentLines
      ["template<size_t i> struct predicate;"]
    ,indentLines
      ["template<> struct predicate<"
        <> Text.pack (show i)
        <> "> { using type = "
        <> p
        <> "; };" | (i,p) <- zip [0::Int ..] pnames]
    ,["};"]]

-- Insert all the namespace opening and closing around declaration regions
withNS :: [(NameSpaces, Text)] -> [Text]
withNS [] = []
withNS (h@(start, _):t) = catMaybes (transNS [] start : addNS h t)
  where addNS (end, x) [] = [ Just x, transNS end [] ]
        addNS (ns, x) (h'@(ns', _): t') =
          Just x : transNS ns ns' : addNS h' t'

        transNS :: NameSpaces -> NameSpaces -> Maybe Text
        transNS xs ys =
          let common = length . takeWhile (True==) $ zipWith (==) xs ys
              toClose = reverse (drop common xs)
              toOpen = drop common ys
              close t = "} // namespace " <> t
              open t = "namespace " <> t <> " {"
              pieces = map close toClose ++ map open toOpen
          in if null pieces then Nothing
              else Just $ Text.intercalate (newline <> newline) pieces

withExtra
  :: CppGen [(NameSpaces, (Text, Text))]
  -> CppGen [(NameSpaces, (Text, Text))]
withExtra act = do
  most <- act
  extra <- popDefs
  return (extra ++ most)


-- ----------------------------------------------------------------------------
-- Naming & namespaces

-- safety pass looking for bad characters, etc
safe :: Text -> Text
safe s
  | s `elem` keywords = s <> "_"
  | otherwise = s
  where
    keywords = [
       "asm", "auto", "bool", "break", "case", "catch", "char", "class",
       "const", "const_cast", "continue", "default", "delete", "do",
       "double", "dynamic_cast", "else", "enum", "explicit", "export",
       "extern", "false", "float", "for", "friend", "goto", "if",
       "inline", "int", "long", "mutable", "namespace", "new", "operator",
       "private", "protected", "public", "register", "reinterpret_cast",
       "return", "short", "signed", "sizeof", "static", "static_cast",
       "struct", "switch", "template", "this", "throw", "true", "try",
       "typedef", "typeid", "typename", "union", "unsigned", "using",
       "virtual", "void", "volatile", "wchar_t", "while",

       -- avoid clashing with glean.h types
       "Alt",
       "Array",
       "Bool",
       "Byte",
       "Enum",
       "Maybe",
       "Nat",
       "Predicate",
       "Repr",
       "Sum",
       "Tuple"
     ]



-- Upper-case the namespaces. We could change this.
schemaName :: (NameSpaces,Text) -> (NameSpaces,Text)
schemaName (spaces,ident) = (schemaNamespace ++ map cap1 spaces, ident)

builtinName :: Text -> (NameSpaces,Text)
builtinName name = (builtinNamespace, snd $ splitDot name)

schemaNamespace :: NameSpaces
schemaNamespace = ["facebook", "glean", "cpp", "schema"]

-- The namespace within which we must declare Repr<T> specializations
reprNamespace :: NameSpaces
reprNamespace = builtinNamespace

builtinNamespace :: NameSpaces
builtinNamespace = ["facebook", "glean", "cpp"]

cppIdent :: Text -> Text
cppIdent = safe

-- | Glue the cpp names together. We qualify everything except things in the
-- same namespace. Here's why:
--
-- namespace foo {
--   struct A {...};
--   namespace bar {
--     struct A {...};
--     struct B { foo::A x; };
--   }
-- }
--
-- FIXME: What we're currently doing still isn't enough. Consider this:
--
-- namespace foo {
--   struct A {...};
--   namespace bar {
--     namespace foo {
--       struct A {...};
--     }
--     struct B { foo::A x; /* should have been ::foo::A */ };
--   }
-- }
--
-- So we need to fully qualify things but for that, we need to know which
-- namespace(s) the generated code is wrapped in and we aren't threading that
-- information at the moment.
cppNameIn :: NameSpaces -> (NameSpaces, Text) -> Text
cppNameIn here (ns, t)
  | here == ns = cppIdent t
  | otherwise = cppName (ns, t)
  where
    -- | Glue the cpp names together
    cppName :: (NameSpaces, Text) -> Text
    cppName (ns, t) = Text.intercalate "::" (map cppIdent (ns ++ [t]))


-- ----------------------------------------------------------------------------
-- Monad

type CppGen a = ReaderT Env (State [(NameSpaces, (Text,Text))]) a

pushDefs :: [(NameSpaces, (Text,Text))] -> CppGen ()
pushDefs defs = modify (defs++)

popDefs :: CppGen [(NameSpaces, (Text,Text))]
popDefs = state $ \s -> (reverse s, [])

-- ----------------------------------------------------------------------------
-- Types

-- | Generate a representation type
reprTy :: NameSpaces -> Type -> CppGen Text
reprTy here t = case t of
  -- Leaves
  Byte{} -> return "Byte" -- glean.h hardcoded here
  Nat{} -> return "Nat"   -- glean.h hardcoded here
  Boolean{} -> return "Bool"
  String{} -> return "String"
  -- Containers
  Array ty -> do
    rTy <- reprTy here ty
    return $ "Array<" <> rTy <> ">"
  Record fields -> do
    ts <- mapM (reprTy here . fieldDefType) fields
    return $ "Tuple<" <> Text.intercalate ", " ts <> ">"
  Sum fields -> do
    ts <- mapM (reprTy here . fieldDefType) fields
    return $ "Sum<" <> Text.intercalate ", " ts <> ">"
  Maybe ty -> do
    rTy <- reprTy here ty
    return $ "Maybe<" <> rTy <> ">"
  -- References
  Predicate pref -> do
    name <- schemaName <$> predicateName pref
    return (cppNameIn here name)
  NamedType tref -> do
    name <- schemaName <$> typeName tref
    if isJust (provided name)
      then return $ cppNameIn here (builtinName (snd name))
      else return $ "Repr<" <> cppNameIn here name <> ">"
  Enumerated elts -> return $ "Enum<" <> showt (length elts) <> ">"

shareTypeDef :: NameSpaces -> Type -> CppGen Text
shareTypeDef here t = do
  (no, name) <- nameThisType t
  case no of
    Old -> return ()
    New -> do
      -- getting the namespace for the generated type right is a bit of a hack,
      -- because we've already mangled the namespace that we're passing around.
      let qname = joinDot (drop (length schemaNamespace) here, name)
          dNew = TypeDef
            { typeDefRef = TypeRef qname 0
            , typeDefType = t }
      pushDefs =<< genDecl (TypeDecl dNew)
  return name

-- | Generate a value type
valueTy :: NameSpaces -> Type -> CppGen Text
valueTy here t = case t of
  -- Leaves
  Byte{} -> return "uint8_t" -- glean.h hardcoded here
  Nat{} -> return "uint64_t" -- glean.h hardcoded here
  Boolean{} -> return "bool" -- glean.h hardcoded here
  String{} -> return "std::string"
  -- Containers
  Array ty -> do
    vTy <- valueTy here ty
    return $ "std::vector<" <> vTy <> ">"
  Record fields -> do
    ts <- mapM (valueTy here . fieldDefType) fields
    return $ "std::tuple<" <> Text.intercalate ", " ts <> ">"
  Sum fields -> do
    ts <- mapM (valueTy here .fieldDefType) fields
    return $ "boost::variant<" <> Text.intercalate ", " (altsOf ts) <> ">"
  Maybe ty ->
    valueTy here $ Sum
      [ FieldDef "^Nothing^" unitT
      , FieldDef "^Just^" ty ]
  Enumerated{} -> shareTypeDef here t
  -- References, Fact is a phantom-typed Id
  Predicate pref -> do
    name <- schemaName <$> predicateName pref
    return $ "Fact<" <> cppNameIn here name <> ">"
  NamedType tref -> do
    name <- schemaName <$> typeName tref
    case provided name of
      Just ty -> valueTy here ty
      Nothing -> return (cppNameIn here name)


-- ----------------------------------------------------------------------------
-- Definitions

genDecl :: SomeDecl -> CppGen [(NameSpaces, (Text,Text))]
genDecl (PredicateDecl PredicateDef{..}) = withExtra $ do
  (spaces,name) <- schemaName <$> predicateName predicateDefRef
  withPredicateDefHint name $ do
  kTy <- valueTy spaces predicateDefKeyType
  vTy <- valueTy spaces predicateDefValueType
  let
    ident = cppIdent name
    value'
      | predicateDefValueType == unitT = ""
      | otherwise = ", " <> vTy
    declare = "struct " <> ident <> ";"
    define = myUnlines
      [ "struct " <> ident <> " : Predicate<" <> kTy
                                             <> value' <> "> {"
      , "  static const char* GLEAN_name() {"
      , "    return \"" <> predicateRef_name predicateDefRef <> "\";"
      , "  }"
      , ""
      , "  static constexpr size_t GLEAN_version() {"
      ,"     return " <> showt (predicateRef_version predicateDefRef) <> ";"
      , "  }"
      , "}; // struct " <> ident
      ]
  return [(spaces, (declare, define))]
genDecl (TypeDecl TypeDef{..}) = withExtra $ do
  name@(_,base) <- schemaName <$> typeName typeDefRef
  withTypeDefHint base $ do
  if isJust (provided name)
    then return []
    else do
      case typeDefType of
        Record fields -> recordDef name fields
        Sum fields -> unionDef name fields
        Enumerated vals -> enumDef name vals
        _other -> aliasDef name typeDefType


aliasDef :: (NameSpaces,Text) -> Type -> CppGen [(NameSpaces, (Text, Text))]
aliasDef name@(spaces,ident) aTy
  | isJust (provided name) = return []
  | otherwise = do
  cppTy <- valueTy spaces aTy
  let define = "using " <> ident <> " = " <> cppTy <> ";"
  return [(spaces, ("", define))]

mkAlt :: Int -> Text -> Text
mkAlt i t = "Alt<" <> showt i <> ", " <> t <> ">"

altsOf :: [Text] -> [Text]
altsOf = zipWith mkAlt [0..]

indentLines :: [Text] -> [Text]
indentLines = map (\t -> if Text.null t then t else "  " <> t)

relops :: Text -> [Text] -> [Text]
relops name fields = concatMap relop ["==","!=","<","<=",">",">="]
  where
    relop op = concat
      [ ["bool operator" <> op <> "(const " <> name <> "& other) const {"]
      , indentLines
          ["return " <> tie fields
          ,"         " <> op <> " " <> tie (map ("other."<>) fields) <> ";"]
      , ["}"] ]
    tie xs = "std::tie(" <> Text.intercalate "," xs <> ")"

mkReprDecl :: (NameSpaces,Text) -> Type -> CppGen (NameSpaces, (Text,Text))
mkReprDecl name typ = do
  rTy <- reprTy reprNamespace typ
  let
    decl = Text.unlines
      [ "template<> struct Repr_<" <> cppNameIn reprNamespace name <> "> {"
      , "  using Type = " <> rTy <> ";"
      , "};"
      ]
  return (reprNamespace, ("", decl))

recordDef
  :: (NameSpaces, Text)
  -> [FieldDef]
  -> CppGen [(NameSpaces, (Text, Text))]
recordDef (spaces,name) fields = do
  rDecl <- mkReprDecl (spaces,name) (Record fields)
  fieldTys <- mapM field fields
  return [rDecl, (spaces, (declare, define fieldTys))]
  where
    ident = cppIdent name
    field (FieldDef param t) = do
      vTy <- withRecordFieldHint param $ valueTy spaces t
      return $ vTy <> " " <> cppIdent param <> ";"
    field_names = map (cppIdent . fieldDefName) fields
    names = Text.intercalate ", " field_names
    outputRepr =
      [ "void outputRepr(Output<Repr<" <> ident <> ">> out) const {"
      , "  outputValue(out, std::make_tuple(" <> names <> "));"
      , "}"
      ]
    declare = "struct " <> ident <> ";"
    define fieldTys = myUnlines $ concat
      [ [ "struct " <> ident <> " {" ]
      , indentLines $ concat
          [ fieldTys
          , [""]
          , relops ident field_names
          , outputRepr ]
      , [ "}; // struct " <> ident ]
      ]


-- Would really prefer std::variant from later C++ standard
unionDef
  :: (NameSpaces,Text)
  -> [FieldDef]
  -> CppGen [(NameSpaces, (Text, Text))]
unionDef (spaces,name) fields = do
  let
    ident = cppIdent name

    makeFieldTy (FieldDef param t) =
      withUnionFieldHint param $ valueTy spaces t

  theTypes <- mapM makeFieldTy fields
  let
    theTs = map fieldDefType fields
    theAlts = altsOf theTypes :: [Text]
    variant = "boost::variant<" <> Text.intercalate ", " theAlts <> ">"
    field = variant <> " GLEAN_value;"

    static_constructor (field, ty, altTy) = do
      let
        tuple fields = do
          let
            doField ty param = do
              vTy <- valueTy spaces ty
              return $ "const " <> vTy <> "& " <> cppIdent param
          tys <- sequence [ doField ty param | FieldDef param ty <- fields ]
          return
            ( Text.intercalate ", " tys
            , "std::make_tuple("
                <> Text.intercalate ", " (map (cppIdent . fieldDefName) fields)
                <> ")")
      (args, ret) <- case ty of
          -- special case, Unit is not a concrete type
        NamedType (TypeRef "builtin.Unit" _) -> tuple []
        Record fields -> tuple fields
        _ -> do
          vTy <- valueTy spaces ty
          return ("const " <> vTy <> "& a", "a")
      return
        [ "static " <> ident <> " " <> field <> "(" <> args <> ") {"
         , "  return " <> ident <> "{" <> altTy <> "(" <> ret <> ")};"
         , "}"]

    field_names = map (cppIdent . fieldDefName) fields

  static_constructors <-
    concat <$> mapM static_constructor (zip3 field_names theTs theAlts)
  rDecl <- mkReprDecl (spaces,name) (Sum fields)
  let
    outputRepr =
      [ "void outputRepr(Output<Repr<" <> ident <> ">> out) const {"
      , "  outputValue(out, GLEAN_value);"
      , "}" ]

    declare = "struct " <> ident <> ";"
    define = myUnlines $ concat
      [ [ "struct " <> ident <> " {" ]
      , indentLines $ intercalate [""]
          [ [field]
          , static_constructors
          , relops ident ["GLEAN_value"]
          , outputRepr ]
      , [ "}; // struct " <> ident ]
      ]

  return [ rDecl, (spaces, (declare, define)) ]


enumDef
  :: (NameSpaces,Text)
  -> [Name]
  -> CppGen [(NameSpaces, (Text, Text))]
enumDef (spaces,name) eVals = do
  rDecl <- mkReprDecl (spaces,name) (Enumerated eVals)
  return  [ rDecl, (spaces, (declare, define)) ]
  where
    ident = cppIdent name
    declare = "enum class " <> ident <> ";"
    define = "enum class " <> ident <> " { "
      <> Text.intercalate ", " (map cppIdent eVals) <> " };"
