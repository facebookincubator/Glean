{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.CSharp
  ({- instances -})
  where

import Data.Text ( Text )
import qualified Data.Text as Text

import Glean.Glass.SymbolId.Class
import qualified Glean
import qualified Glean.Haxl.Repos as Glean

import Glean.Glass.Types
import qualified Glean.Schema.Csharp.Types as CSharp
import Glean.Schema.CodeCsharp.Types as CodeCsharp ( Entity(..) )

instance Symbol CodeCsharp.Entity where
  toSymbol e = case e of
    CodeCsharp.Entity_decl defn -> toSymbol defn
    CodeCsharp.Entity_EMPTY -> return []

instance Symbol CSharp.Definition where
  toSymbol d = case d of
    CSharp.Definition_method m -> toSymbolPredicate m
    CSharp.Definition_type t -> toSymbol t
    CSharp.Definition_field f -> toSymbolPredicate f
    CSharp.Definition_parameter p -> toSymbolPredicate p
    CSharp.Definition_typeParameter t -> toSymbolPredicate t
    CSharp.Definition_local p -> toSymbolPredicate p
    CSharp.Definition_property t -> toSymbolPredicate t
    CSharp.Definition_EMPTY -> return []

instance Symbol CSharp.Name where
  toSymbol n = (:[]) <$> Glean.keyOf n

instance Symbol CSharp.Namespace where
  toSymbol ns = Glean.keyOf ns >>= \CSharp.Namespace_key{..} -> do
    n <- Glean.keyOf namespace_key_name
    case namespace_key_containingNamespace of
      Nothing -> return [n]
      Just ns -> do
        ns' <- toSymbol ns
        return $ if null ns' then [n] else ns' ++ [n]

instance Symbol CSharp.FullName_key where
  toSymbol CSharp.FullName_key{..} =
    fullName_key_containingNamespace <:> fullName_key_name

instance Symbol CSharp.FullName where
  toSymbol fn = Glean.keyOf fn >>= toSymbol

instance Symbol CSharp.Method_key where
  toSymbol CSharp.Method_key{..} =
    method_key_containingType <:> method_key_name

instance Symbol CSharp.Field_key where
  toSymbol CSharp.Field_key{..} = do
    nameStr <- Glean.keyOf field_key_name
    return [nameStr]

instance Symbol CSharp.Parameter_key where
  toSymbol CSharp.Parameter_key{..} = do
    nameStr <- Glean.keyOf parameter_key_name
    return [nameStr]

instance Symbol CSharp.TypeParameter_key where
  toSymbol CSharp.TypeParameter_key{..} = do
    nameStr <- Glean.keyOf typeParameter_key_name
    return [nameStr]

instance Symbol CSharp.Local_key where
  toSymbol CSharp.Local_key{..} = do
    nameStr <- Glean.keyOf local_key_name
    return [nameStr]

instance Symbol CSharp.Property_key where
  toSymbol CSharp.Property_key{..} = do
    nameStr <- Glean.keyOf property_key_name
    return [nameStr]

instance Symbol CSharp.ArrayType_key where
  toSymbol CSharp.ArrayType_key{..} = toSymbol arrayType_key_elementType

instance Symbol CSharp.PointerType_key where
  toSymbol CSharp.PointerType_key{..} = toSymbol pointerType_key_pointedAtType

instance Symbol CSharp.FunctionPointerType_key where
  toSymbol CSharp.FunctionPointerType_key{..} = do
    CSharp.FullName_key name _ns <- Glean.keyOf functionPointerType_key_name
    nameStr <- Glean.keyOf name
    return [nameStr]

instance Symbol CSharp.Class_key where
  toSymbol CSharp.Class_key{..} = Glean.keyOf class_key_name >>= toSymbol

instance Symbol CSharp.Interface_key where
  toSymbol CSharp.Interface_key{..} =
    Glean.keyOf interface_key_name >>= toSymbol

instance Symbol CSharp.Record_key where
  toSymbol CSharp.Record_key{..} = Glean.keyOf record_key_name >>= toSymbol

instance Symbol CSharp.Struct_key where
  toSymbol CSharp.Struct_key{..} = Glean.keyOf struct_key_name >>= toSymbol

instance Symbol CSharp.NamedType where
  toSymbol nt = case nt of
    CSharp.NamedType_class_ c -> toSymbolPredicate c
    CSharp.NamedType_interface_ i -> toSymbolPredicate i
    CSharp.NamedType_record_ r -> toSymbolPredicate r
    CSharp.NamedType_struct_ s -> toSymbolPredicate s
    CSharp.NamedType_EMPTY -> return []

instance Symbol CSharp.AType where
  toSymbol d = case d of
    CSharp.AType_arrayType at -> toSymbolPredicate at
    CSharp.AType_namedType nt -> toSymbol nt
    CSharp.AType_functionPointerType fpt -> toSymbolPredicate fpt
    CSharp.AType_pointerType pt -> toSymbolPredicate pt
    CSharp.AType_typeParameter tp -> toSymbolPredicate tp
    CSharp.AType_EMPTY -> return []

instance ToQName CodeCsharp.Entity where
  toQName e = case e of
    CodeCsharp.Entity_decl defn -> toQName defn
    CodeCsharp.Entity_EMPTY -> pure $ Left "ToQName: Unknown Buck entity"

instance ToQName CSharp.Definition where
  toQName d = case d of
    CSharp.Definition_method m -> Glean.keyOf m >>= toQName
    CSharp.Definition_field f -> Glean.keyOf f >>= toQName
    CSharp.Definition_parameter p -> Glean.keyOf p >>= toQName
    CSharp.Definition_typeParameter p -> Glean.keyOf p >>= toQName
    CSharp.Definition_local l -> Glean.keyOf l >>= toQName
    CSharp.Definition_property p -> Glean.keyOf p >>= toQName
    CSharp.Definition_type t -> toQName t
    _ -> pure $ Left "ToQName: Unknown C# entity"

instance ToQName CSharp.AType where
  toQName d = case d of
    CSharp.AType_arrayType at -> Glean.keyOf at >>= toQName
    CSharp.AType_namedType nt -> toQName nt
    CSharp.AType_functionPointerType fpt -> Glean.keyOf fpt >>= toQName
    CSharp.AType_pointerType pt -> Glean.keyOf pt >>= toQName
    CSharp.AType_typeParameter tp -> Glean.keyOf tp >>= toQName
    CSharp.AType_EMPTY -> pure $ Left "ToQName: Unknown C# AType"

instance ToQName CSharp.ArrayType_key where
  toQName (CSharp.ArrayType_key at _rank) = toQName at

instance ToQName CSharp.FunctionPointerType_key where
  toQName (CSharp.FunctionPointerType_key name _sig) = do
    CSharp.FullName_key name _ns <- Glean.keyOf name
    nameStr <- Glean.keyOf name
    -- TODO: method signature
    return $ Right (Name nameStr, Name mempty)

instance ToQName CSharp.PointerType_key where
  toQName (CSharp.PointerType_key at) = toQName at

instance ToQName CSharp.NamedType where
  toQName nt = case nt of
    CSharp.NamedType_class_ c -> Glean.keyOf c >>= toQName
    CSharp.NamedType_interface_ i -> Glean.keyOf i >>= toQName
    CSharp.NamedType_record_ r -> Glean.keyOf r >>= toQName
    CSharp.NamedType_struct_ s -> Glean.keyOf s >>= toQName
    CSharp.NamedType_EMPTY -> pure $ Left "ToQName: Unknown C# NamedType"

instance ToQName CSharp.Struct_key where
  toQName CSharp.Struct_key{..} = do
    (n, ns) <- flattenFullName =<< Glean.keyOf struct_key_name
    return $ Right (Name n, Name ns)

instance ToQName CSharp.Record_key where
  toQName CSharp.Record_key{..} = do
    (n, ns) <- flattenFullName =<< Glean.keyOf record_key_name
    return $ Right (Name n, Name ns)

instance ToQName CSharp.Interface_key where
  toQName CSharp.Interface_key{..} = do
    (n, ns) <- flattenFullName =<< Glean.keyOf interface_key_name
    return $ Right (Name n, Name ns)

instance ToQName CSharp.Class_key where
  toQName CSharp.Class_key{..} = do
    (n, ns) <- flattenFullName =<< Glean.keyOf class_key_name
    return $ Right (Name n, Name ns)

flattenNamespace :: CSharp.Namespace_key -> Glean.RepoHaxl u w [Text]
flattenNamespace CSharp.Namespace_key{..} = do
  nameStr <- Glean.keyOf namespace_key_name
  case namespace_key_containingNamespace of
    Nothing -> return [nameStr]
    Just ns -> do
      ms <- flattenNamespace =<< Glean.keyOf ns
      return (ms ++ [nameStr])

flattenFullName :: CSharp.FullName_key -> Glean.RepoHaxl u w (Text, Text)
flattenFullName (CSharp.FullName_key name ns) = do
  names <- flattenNamespace =<< Glean.keyOf ns
  nameStr <- Glean.keyOf name
  return (nameStr, Text.intercalate "." names)

instance ToQName CSharp.Local_key where
  toQName CSharp.Local_key{..} = do
    nameStr <- Glean.keyOf local_key_name
    return $ Right (Name nameStr, Name mempty)

instance ToQName CSharp.TypeParameter_key where
  toQName CSharp.TypeParameter_key{..} = do
    nameStr <- Glean.keyOf typeParameter_key_name
    return $ Right (Name nameStr, Name mempty)

instance ToQName CSharp.Parameter_key where
  toQName CSharp.Parameter_key{..} = do
    nameStr <- Glean.keyOf parameter_key_name -- parent method
    return $ Right (Name nameStr, Name mempty)

instance ToQName CSharp.Property_key where
  toQName CSharp.Property_key{..} = do
    nameStr <- Glean.keyOf property_key_name
    container <- toQName property_key_containingType
    return $ Right $ case container of
      Left{} -> (Name nameStr, Name mempty)
      Right (Name n, Name ns) -> (Name nameStr, Name (ns <> "." <> n))

instance ToQName CSharp.Field_key where
  toQName CSharp.Field_key{..} = do
    nameStr <- Glean.keyOf field_key_name
    container <- toQName field_key_containingType
    return $ Right $ case container of
      Left{} -> (Name nameStr, Name mempty)
      Right (Name n, Name ns) -> (Name nameStr, Name (ns <> "." <> n))

instance ToQName CSharp.Method_key where
  toQName CSharp.Method_key{..} = do
    nameStr <- Glean.keyOf method_key_name
    container <- toQName method_key_containingType
    return $ Right $ case container of
      Left{} -> (Name nameStr, Name mempty)
      Right (Name n, Name ns) -> (Name nameStr, Name (ns <> "." <> n))
