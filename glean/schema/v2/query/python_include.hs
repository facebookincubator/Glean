-- @generated
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DataKinds #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import qualified Data.ByteString
import qualified Data.Default
import qualified Data.Text

import qualified Glean.Types as Glean
import qualified Glean.Typed as Glean
import qualified Glean.Query.Angle as Angle

import qualified Glean.Schema.Builtin.Types
import qualified Glean.Schema.Query.Builtin.Types

import qualified Glean.Schema.Src.Types
import qualified Glean.Schema.Query.Src.Types

import qualified Glean.Schema.Python.Types


type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ClassDefinition_key = Glean.Schema.Python.Types.ClassDefinition_key
type instance Glean.QueryOf Glean.Schema.Python.Types.ClassDefinition_key = Glean.Schema.Query.Python.Types.ClassDefinition_key

instance Glean.ToQuery Glean.Schema.Python.Types.ClassDefinition_key where
  toQuery (Glean.Schema.Python.Types.ClassDefinition_key x1 x2 x3 x4 x5) = Glean.Schema.Query.Python.Types.ClassDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Python.Types.classDefinition_bases_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Python.Types.classDefinition_bases_just = Prelude.Just ((Glean.Schema.Query.Python.Types.ClassDefinition_bases_just__array_exact . Prelude.map Glean.toQuery) x)})) x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Python.Types.classDefinition_keywords_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Python.Types.classDefinition_keywords_just = Prelude.Just ((Glean.Schema.Query.Python.Types.ClassDefinition_keywords_just__array_exact . Prelude.map Glean.toQuery) x)})) x3)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Python.Types.classDefinition_decorators_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Python.Types.classDefinition_decorators_just = Prelude.Just ((Glean.Schema.Query.Python.Types.ClassDefinition_decorators_just__array_exact . Prelude.map Glean.toQuery) x)})) x4)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Python.Types.classDefinition_docstring_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Python.Types.classDefinition_docstring_just = Prelude.Just (Glean.toQuery x)})) x5))

instance Glean.PredicateQuery Glean.Schema.Python.Types.ClassDefinition where
  toQueryId = Glean.Schema.Query.Python.Types.ClassDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.ClassDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ClassDefinition = Glean.Schema.Python.Types.ClassDefinition
type instance Glean.QueryOf Glean.Schema.Python.Types.ClassDefinition = Glean.Schema.Query.Python.Types.ClassDefinition

instance Glean.ToQuery Glean.Schema.Python.Types.ClassDefinition

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ImportStatementByName_key = Glean.Schema.Python.Types.ImportStatementByName_key
type instance Glean.QueryOf Glean.Schema.Python.Types.ImportStatementByName_key = Glean.Schema.Query.Python.Types.ImportStatementByName_key

instance Glean.ToQuery Glean.Schema.Python.Types.ImportStatementByName_key where
  toQuery (Glean.Schema.Python.Types.ImportStatementByName_key x1 x2) = Glean.Schema.Query.Python.Types.ImportStatementByName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Python.Types.ImportStatementByName where
  toQueryId = Glean.Schema.Query.Python.Types.ImportStatementByName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.ImportStatementByName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ImportStatementByName = Glean.Schema.Python.Types.ImportStatementByName
type instance Glean.QueryOf Glean.Schema.Python.Types.ImportStatementByName = Glean.Schema.Query.Python.Types.ImportStatementByName

instance Glean.ToQuery Glean.Schema.Python.Types.ImportStatementByName

instance Glean.PredicateQuery Glean.Schema.Python.Types.DeclarationToName where
  toQueryId = Glean.Schema.Query.Python.Types.DeclarationToName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.DeclarationToName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.DeclarationToName = Glean.Schema.Python.Types.DeclarationToName
type instance Glean.QueryOf Glean.Schema.Python.Types.DeclarationToName = Glean.Schema.Query.Python.Types.DeclarationToName

instance Glean.ToQuery Glean.Schema.Python.Types.DeclarationToName

instance Glean.PredicateQuery Glean.Schema.Python.Types.DeclarationByName where
  toQueryId = Glean.Schema.Query.Python.Types.DeclarationByName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.DeclarationByName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.DeclarationByName = Glean.Schema.Python.Types.DeclarationByName
type instance Glean.QueryOf Glean.Schema.Python.Types.DeclarationByName = Glean.Schema.Query.Python.Types.DeclarationByName

instance Glean.ToQuery Glean.Schema.Python.Types.DeclarationByName

instance Glean.PredicateQuery Glean.Schema.Python.Types.DocstringContent where
  toQueryId = Glean.Schema.Query.Python.Types.DocstringContent_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.DocstringContent_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.DocstringContent = Glean.Schema.Python.Types.DocstringContent
type instance Glean.QueryOf Glean.Schema.Python.Types.DocstringContent = Glean.Schema.Query.Python.Types.DocstringContent

instance Glean.ToQuery Glean.Schema.Python.Types.DocstringContent

instance Glean.PredicateQuery Glean.Schema.Python.Types.VariableBySName where
  toQueryId = Glean.Schema.Query.Python.Types.VariableBySName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.VariableBySName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.VariableBySName = Glean.Schema.Python.Types.VariableBySName
type instance Glean.QueryOf Glean.Schema.Python.Types.VariableBySName = Glean.Schema.Query.Python.Types.VariableBySName

instance Glean.ToQuery Glean.Schema.Python.Types.VariableBySName

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.FileDirectXRefs_key = Glean.Schema.Python.Types.FileDirectXRefs_key
type instance Glean.QueryOf Glean.Schema.Python.Types.FileDirectXRefs_key = Glean.Schema.Query.Python.Types.FileDirectXRefs_key

instance Glean.ToQuery Glean.Schema.Python.Types.FileDirectXRefs_key where
  toQuery (Glean.Schema.Python.Types.FileDirectXRefs_key x1 x2) = Glean.Schema.Query.Python.Types.FileDirectXRefs_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Python.Types.FileDirectXRefs_xrefs_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Python.Types.FileDirectXRefs where
  toQueryId = Glean.Schema.Query.Python.Types.FileDirectXRefs_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.FileDirectXRefs_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.FileDirectXRefs = Glean.Schema.Python.Types.FileDirectXRefs
type instance Glean.QueryOf Glean.Schema.Python.Types.FileDirectXRefs = Glean.Schema.Query.Python.Types.FileDirectXRefs

instance Glean.ToQuery Glean.Schema.Python.Types.FileDirectXRefs

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.FunctionDeclaration_key = Glean.Schema.Python.Types.FunctionDeclaration_key
type instance Glean.QueryOf Glean.Schema.Python.Types.FunctionDeclaration_key = Glean.Schema.Query.Python.Types.FunctionDeclaration_key

instance Glean.ToQuery Glean.Schema.Python.Types.FunctionDeclaration_key where
  toQuery (Glean.Schema.Python.Types.FunctionDeclaration_key x1) = Glean.Schema.Query.Python.Types.FunctionDeclaration_key (Prelude.Just (Glean.toQuery x1))

instance Glean.PredicateQuery Glean.Schema.Python.Types.FunctionDeclaration where
  toQueryId = Glean.Schema.Query.Python.Types.FunctionDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.FunctionDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.FunctionDeclaration = Glean.Schema.Python.Types.FunctionDeclaration
type instance Glean.QueryOf Glean.Schema.Python.Types.FunctionDeclaration = Glean.Schema.Query.Python.Types.FunctionDeclaration

instance Glean.ToQuery Glean.Schema.Python.Types.FunctionDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.DirectXRefsByFile_key = Glean.Schema.Python.Types.DirectXRefsByFile_key
type instance Glean.QueryOf Glean.Schema.Python.Types.DirectXRefsByFile_key = Glean.Schema.Query.Python.Types.DirectXRefsByFile_key

instance Glean.ToQuery Glean.Schema.Python.Types.DirectXRefsByFile_key where
  toQuery (Glean.Schema.Python.Types.DirectXRefsByFile_key x1 x2) = Glean.Schema.Query.Python.Types.DirectXRefsByFile_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Python.Types.DirectXRefsByFile where
  toQueryId = Glean.Schema.Query.Python.Types.DirectXRefsByFile_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.DirectXRefsByFile_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.DirectXRefsByFile = Glean.Schema.Python.Types.DirectXRefsByFile
type instance Glean.QueryOf Glean.Schema.Python.Types.DirectXRefsByFile = Glean.Schema.Query.Python.Types.DirectXRefsByFile

instance Glean.ToQuery Glean.Schema.Python.Types.DirectXRefsByFile

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.FunctionDefinition_key = Glean.Schema.Python.Types.FunctionDefinition_key
type instance Glean.QueryOf Glean.Schema.Python.Types.FunctionDefinition_key = Glean.Schema.Query.Python.Types.FunctionDefinition_key

instance Glean.ToQuery Glean.Schema.Python.Types.FunctionDefinition_key where
  toQuery (Glean.Schema.Python.Types.FunctionDefinition_key x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = Glean.Schema.Query.Python.Types.FunctionDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Python.Types.functionDefinition_returns_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Python.Types.functionDefinition_returns_just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just ((Glean.Schema.Query.Python.Types.FunctionDefinition_params_array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Python.Types.functionDefinition_posonly_params_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Python.Types.functionDefinition_posonly_params_just = Prelude.Just ((Glean.Schema.Query.Python.Types.FunctionDefinition_posonly_params_just__array_exact . Prelude.map Glean.toQuery) x)})) x5)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Python.Types.functionDefinition_kwonly_params_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Python.Types.functionDefinition_kwonly_params_just = Prelude.Just ((Glean.Schema.Query.Python.Types.FunctionDefinition_kwonly_params_just__array_exact . Prelude.map Glean.toQuery) x)})) x6)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Python.Types.functionDefinition_star_arg_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Python.Types.functionDefinition_star_arg_just = Prelude.Just (Glean.toQuery x)})) x7)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Python.Types.functionDefinition_star_kwarg_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Python.Types.functionDefinition_star_kwarg_just = Prelude.Just (Glean.toQuery x)})) x8)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Python.Types.functionDefinition_decorators_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Python.Types.functionDefinition_decorators_just = Prelude.Just ((Glean.Schema.Query.Python.Types.FunctionDefinition_decorators_just__array_exact . Prelude.map Glean.toQuery) x)})) x9)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Python.Types.functionDefinition_docstring_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Python.Types.functionDefinition_docstring_just = Prelude.Just (Glean.toQuery x)})) x10))

instance Glean.PredicateQuery Glean.Schema.Python.Types.FunctionDefinition where
  toQueryId = Glean.Schema.Query.Python.Types.FunctionDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.FunctionDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.FunctionDefinition = Glean.Schema.Python.Types.FunctionDefinition
type instance Glean.QueryOf Glean.Schema.Python.Types.FunctionDefinition = Glean.Schema.Query.Python.Types.FunctionDefinition

instance Glean.ToQuery Glean.Schema.Python.Types.FunctionDefinition

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.DeclarationUses_key = Glean.Schema.Python.Types.DeclarationUses_key
type instance Glean.QueryOf Glean.Schema.Python.Types.DeclarationUses_key = Glean.Schema.Query.Python.Types.DeclarationUses_key

instance Glean.ToQuery Glean.Schema.Python.Types.DeclarationUses_key where
  toQuery (Glean.Schema.Python.Types.DeclarationUses_key x1 x2 x3) = Glean.Schema.Query.Python.Types.DeclarationUses_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Python.Types.DeclarationUses where
  toQueryId = Glean.Schema.Query.Python.Types.DeclarationUses_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.DeclarationUses_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.DeclarationUses = Glean.Schema.Python.Types.DeclarationUses
type instance Glean.QueryOf Glean.Schema.Python.Types.DeclarationUses = Glean.Schema.Query.Python.Types.DeclarationUses

instance Glean.ToQuery Glean.Schema.Python.Types.DeclarationUses

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ImportStatement_key = Glean.Schema.Python.Types.ImportStatement_key
type instance Glean.QueryOf Glean.Schema.Python.Types.ImportStatement_key = Glean.Schema.Query.Python.Types.ImportStatement_key

instance Glean.ToQuery Glean.Schema.Python.Types.ImportStatement_key where
  toQuery (Glean.Schema.Python.Types.ImportStatement_key x1 x2) = Glean.Schema.Query.Python.Types.ImportStatement_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Python.Types.ImportStatement where
  toQueryId = Glean.Schema.Query.Python.Types.ImportStatement_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.ImportStatement_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ImportStatement = Glean.Schema.Python.Types.ImportStatement
type instance Glean.QueryOf Glean.Schema.Python.Types.ImportStatement = Glean.Schema.Query.Python.Types.ImportStatement

instance Glean.ToQuery Glean.Schema.Python.Types.ImportStatement

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ImportStatement_1_key = Glean.Schema.Python.Types.ImportStatement_1_key
type instance Glean.QueryOf Glean.Schema.Python.Types.ImportStatement_1_key = Glean.Schema.Query.Python.Types.ImportStatement_1_key

instance Glean.ToQuery Glean.Schema.Python.Types.ImportStatement_1_key where
  toQuery (Glean.Schema.Python.Types.ImportStatement_1_key x1 x2 x3) = Glean.Schema.Query.Python.Types.ImportStatement_1_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Python.Types.importStatement_1_as_name_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Python.Types.importStatement_1_as_name_just = Prelude.Just (Glean.toQuery x)})) x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Python.Types.ImportStatement_1 where
  toQueryId = Glean.Schema.Query.Python.Types.ImportStatement_1_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.ImportStatement_1_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ImportStatement_1 = Glean.Schema.Python.Types.ImportStatement_1
type instance Glean.QueryOf Glean.Schema.Python.Types.ImportStatement_1 = Glean.Schema.Query.Python.Types.ImportStatement_1

instance Glean.ToQuery Glean.Schema.Python.Types.ImportStatement_1

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.DeclarationLocation_key = Glean.Schema.Python.Types.DeclarationLocation_key
type instance Glean.QueryOf Glean.Schema.Python.Types.DeclarationLocation_key = Glean.Schema.Query.Python.Types.DeclarationLocation_key

instance Glean.ToQuery Glean.Schema.Python.Types.DeclarationLocation_key where
  toQuery (Glean.Schema.Python.Types.DeclarationLocation_key x1 x2 x3) = Glean.Schema.Query.Python.Types.DeclarationLocation_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Python.Types.DeclarationLocation where
  toQueryId = Glean.Schema.Query.Python.Types.DeclarationLocation_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.DeclarationLocation_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.DeclarationLocation = Glean.Schema.Python.Types.DeclarationLocation
type instance Glean.QueryOf Glean.Schema.Python.Types.DeclarationLocation = Glean.Schema.Query.Python.Types.DeclarationLocation

instance Glean.ToQuery Glean.Schema.Python.Types.DeclarationLocation

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.DeclarationLocation_1_key = Glean.Schema.Python.Types.DeclarationLocation_1_key
type instance Glean.QueryOf Glean.Schema.Python.Types.DeclarationLocation_1_key = Glean.Schema.Query.Python.Types.DeclarationLocation_1_key

instance Glean.ToQuery Glean.Schema.Python.Types.DeclarationLocation_1_key where
  toQuery (Glean.Schema.Python.Types.DeclarationLocation_1_key x1 x2 x3) = Glean.Schema.Query.Python.Types.DeclarationLocation_1_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Python.Types.DeclarationLocation_1 where
  toQueryId = Glean.Schema.Query.Python.Types.DeclarationLocation_1_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.DeclarationLocation_1_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.DeclarationLocation_1 = Glean.Schema.Python.Types.DeclarationLocation_1
type instance Glean.QueryOf Glean.Schema.Python.Types.DeclarationLocation_1 = Glean.Schema.Query.Python.Types.DeclarationLocation_1

instance Glean.ToQuery Glean.Schema.Python.Types.DeclarationLocation_1

instance Glean.PredicateQuery Glean.Schema.Python.Types.ModuleBySName where
  toQueryId = Glean.Schema.Query.Python.Types.ModuleBySName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.ModuleBySName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ModuleBySName = Glean.Schema.Python.Types.ModuleBySName
type instance Glean.QueryOf Glean.Schema.Python.Types.ModuleBySName = Glean.Schema.Query.Python.Types.ModuleBySName

instance Glean.ToQuery Glean.Schema.Python.Types.ModuleBySName

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ClassDeclaration_key = Glean.Schema.Python.Types.ClassDeclaration_key
type instance Glean.QueryOf Glean.Schema.Python.Types.ClassDeclaration_key = Glean.Schema.Query.Python.Types.ClassDeclaration_key

instance Glean.ToQuery Glean.Schema.Python.Types.ClassDeclaration_key where
  toQuery (Glean.Schema.Python.Types.ClassDeclaration_key x1 x2) = Glean.Schema.Query.Python.Types.ClassDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Python.Types.classDeclaration_bases_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Python.Types.classDeclaration_bases_just = Prelude.Just ((Glean.Schema.Query.Python.Types.ClassDeclaration_bases_just__array_exact . Prelude.map Glean.toQuery) x)})) x2))

instance Glean.PredicateQuery Glean.Schema.Python.Types.ClassDeclaration where
  toQueryId = Glean.Schema.Query.Python.Types.ClassDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.ClassDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ClassDeclaration = Glean.Schema.Python.Types.ClassDeclaration
type instance Glean.QueryOf Glean.Schema.Python.Types.ClassDeclaration = Glean.Schema.Query.Python.Types.ClassDeclaration

instance Glean.ToQuery Glean.Schema.Python.Types.ClassDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.VariableDefinition_key = Glean.Schema.Python.Types.VariableDefinition_key
type instance Glean.QueryOf Glean.Schema.Python.Types.VariableDefinition_key = Glean.Schema.Query.Python.Types.VariableDefinition_key

instance Glean.ToQuery Glean.Schema.Python.Types.VariableDefinition_key where
  toQuery (Glean.Schema.Python.Types.VariableDefinition_key x1 x2) = Glean.Schema.Query.Python.Types.VariableDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Python.Types.variableDefinition_type_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Python.Types.variableDefinition_type_just = Prelude.Just (Glean.toQuery x)})) x2))

instance Glean.PredicateQuery Glean.Schema.Python.Types.VariableDefinition where
  toQueryId = Glean.Schema.Query.Python.Types.VariableDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.VariableDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.VariableDefinition = Glean.Schema.Python.Types.VariableDefinition
type instance Glean.QueryOf Glean.Schema.Python.Types.VariableDefinition = Glean.Schema.Query.Python.Types.VariableDefinition

instance Glean.ToQuery Glean.Schema.Python.Types.VariableDefinition

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.FileXRefs_key = Glean.Schema.Python.Types.FileXRefs_key
type instance Glean.QueryOf Glean.Schema.Python.Types.FileXRefs_key = Glean.Schema.Query.Python.Types.FileXRefs_key

instance Glean.ToQuery Glean.Schema.Python.Types.FileXRefs_key where
  toQuery (Glean.Schema.Python.Types.FileXRefs_key x1 x2) = Glean.Schema.Query.Python.Types.FileXRefs_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Python.Types.FileXRefs_xrefs_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Python.Types.FileXRefs where
  toQueryId = Glean.Schema.Query.Python.Types.FileXRefs_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.FileXRefs_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.FileXRefs = Glean.Schema.Python.Types.FileXRefs
type instance Glean.QueryOf Glean.Schema.Python.Types.FileXRefs = Glean.Schema.Query.Python.Types.FileXRefs

instance Glean.ToQuery Glean.Schema.Python.Types.FileXRefs

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.FileXRefs_1_key = Glean.Schema.Python.Types.FileXRefs_1_key
type instance Glean.QueryOf Glean.Schema.Python.Types.FileXRefs_1_key = Glean.Schema.Query.Python.Types.FileXRefs_1_key

instance Glean.ToQuery Glean.Schema.Python.Types.FileXRefs_1_key where
  toQuery (Glean.Schema.Python.Types.FileXRefs_1_key x1 x2) = Glean.Schema.Query.Python.Types.FileXRefs_1_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Python.Types.FileXRefs_1_xrefs_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Python.Types.FileXRefs_1 where
  toQueryId = Glean.Schema.Query.Python.Types.FileXRefs_1_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.FileXRefs_1_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.FileXRefs_1 = Glean.Schema.Python.Types.FileXRefs_1
type instance Glean.QueryOf Glean.Schema.Python.Types.FileXRefs_1 = Glean.Schema.Query.Python.Types.FileXRefs_1

instance Glean.ToQuery Glean.Schema.Python.Types.FileXRefs_1

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.SName_key = Glean.Schema.Python.Types.SName_key
type instance Glean.QueryOf Glean.Schema.Python.Types.SName_key = Glean.Schema.Query.Python.Types.SName_key

instance Glean.ToQuery Glean.Schema.Python.Types.SName_key where
  toQuery (Glean.Schema.Python.Types.SName_key x1 x2) = Glean.Schema.Query.Python.Types.SName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Python.Types.sName_parent_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Python.Types.sName_parent_just = Prelude.Just (Glean.toQuery x)})) x2))

instance Glean.PredicateQuery Glean.Schema.Python.Types.SName where
  toQueryId = Glean.Schema.Query.Python.Types.SName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.SName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.SName = Glean.Schema.Python.Types.SName
type instance Glean.QueryOf Glean.Schema.Python.Types.SName = Glean.Schema.Query.Python.Types.SName

instance Glean.ToQuery Glean.Schema.Python.Types.SName

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.TargetUses_key = Glean.Schema.Python.Types.TargetUses_key
type instance Glean.QueryOf Glean.Schema.Python.Types.TargetUses_key = Glean.Schema.Query.Python.Types.TargetUses_key

instance Glean.ToQuery Glean.Schema.Python.Types.TargetUses_key where
  toQuery (Glean.Schema.Python.Types.TargetUses_key x1 x2 x3) = Glean.Schema.Query.Python.Types.TargetUses_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Glean.Schema.Query.Src.Types.ByteSpans_exact . Prelude.map Glean.toQuery) x3))

instance Glean.PredicateQuery Glean.Schema.Python.Types.TargetUses where
  toQueryId = Glean.Schema.Query.Python.Types.TargetUses_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.TargetUses_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.TargetUses = Glean.Schema.Python.Types.TargetUses
type instance Glean.QueryOf Glean.Schema.Python.Types.TargetUses = Glean.Schema.Query.Python.Types.TargetUses

instance Glean.ToQuery Glean.Schema.Python.Types.TargetUses

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.TargetUses_1_key = Glean.Schema.Python.Types.TargetUses_1_key
type instance Glean.QueryOf Glean.Schema.Python.Types.TargetUses_1_key = Glean.Schema.Query.Python.Types.TargetUses_1_key

instance Glean.ToQuery Glean.Schema.Python.Types.TargetUses_1_key where
  toQuery (Glean.Schema.Python.Types.TargetUses_1_key x1 x2 x3) = Glean.Schema.Query.Python.Types.TargetUses_1_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Glean.Schema.Query.Src.Types.ByteSpans_exact . Prelude.map Glean.toQuery) x3))

instance Glean.PredicateQuery Glean.Schema.Python.Types.TargetUses_1 where
  toQueryId = Glean.Schema.Query.Python.Types.TargetUses_1_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.TargetUses_1_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.TargetUses_1 = Glean.Schema.Python.Types.TargetUses_1
type instance Glean.QueryOf Glean.Schema.Python.Types.TargetUses_1 = Glean.Schema.Query.Python.Types.TargetUses_1

instance Glean.ToQuery Glean.Schema.Python.Types.TargetUses_1

instance Glean.PredicateQuery Glean.Schema.Python.Types.ClassBySName where
  toQueryId = Glean.Schema.Query.Python.Types.ClassBySName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.ClassBySName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ClassBySName = Glean.Schema.Python.Types.ClassBySName
type instance Glean.QueryOf Glean.Schema.Python.Types.ClassBySName = Glean.Schema.Query.Python.Types.ClassBySName

instance Glean.ToQuery Glean.Schema.Python.Types.ClassBySName

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.XRefIndirectTarget_key = Glean.Schema.Python.Types.XRefIndirectTarget_key
type instance Glean.QueryOf Glean.Schema.Python.Types.XRefIndirectTarget_key = Glean.Schema.Query.Python.Types.XRefIndirectTarget_key

instance Glean.ToQuery Glean.Schema.Python.Types.XRefIndirectTarget_key where
  toQuery (Glean.Schema.Python.Types.XRefIndirectTarget_key x1 x2) = Glean.Schema.Query.Python.Types.XRefIndirectTarget_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Python.Types.XRefIndirectTarget where
  toQueryId = Glean.Schema.Query.Python.Types.XRefIndirectTarget_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.XRefIndirectTarget_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.XRefIndirectTarget = Glean.Schema.Python.Types.XRefIndirectTarget
type instance Glean.QueryOf Glean.Schema.Python.Types.XRefIndirectTarget = Glean.Schema.Query.Python.Types.XRefIndirectTarget

instance Glean.ToQuery Glean.Schema.Python.Types.XRefIndirectTarget

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.XRefIndirectTarget_1_key = Glean.Schema.Python.Types.XRefIndirectTarget_1_key
type instance Glean.QueryOf Glean.Schema.Python.Types.XRefIndirectTarget_1_key = Glean.Schema.Query.Python.Types.XRefIndirectTarget_1_key

instance Glean.ToQuery Glean.Schema.Python.Types.XRefIndirectTarget_1_key where
  toQuery (Glean.Schema.Python.Types.XRefIndirectTarget_1_key x1 x2) = Glean.Schema.Query.Python.Types.XRefIndirectTarget_1_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Python.Types.XRefIndirectTarget_1 where
  toQueryId = Glean.Schema.Query.Python.Types.XRefIndirectTarget_1_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.XRefIndirectTarget_1_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.XRefIndirectTarget_1 = Glean.Schema.Python.Types.XRefIndirectTarget_1
type instance Glean.QueryOf Glean.Schema.Python.Types.XRefIndirectTarget_1 = Glean.Schema.Query.Python.Types.XRefIndirectTarget_1

instance Glean.ToQuery Glean.Schema.Python.Types.XRefIndirectTarget_1

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.Module_key = Glean.Schema.Python.Types.Module_key
type instance Glean.QueryOf Glean.Schema.Python.Types.Module_key = Glean.Schema.Query.Python.Types.Module_key

instance Glean.ToQuery Glean.Schema.Python.Types.Module_key where
  toQuery (Glean.Schema.Python.Types.Module_key x1) = Glean.Schema.Query.Python.Types.Module_key (Prelude.Just (Glean.toQuery x1))

instance Glean.PredicateQuery Glean.Schema.Python.Types.Module where
  toQueryId = Glean.Schema.Query.Python.Types.Module_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.Module_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.Module = Glean.Schema.Python.Types.Module
type instance Glean.QueryOf Glean.Schema.Python.Types.Module = Glean.Schema.Query.Python.Types.Module

instance Glean.ToQuery Glean.Schema.Python.Types.Module

instance Glean.PredicateQuery Glean.Schema.Python.Types.DeclarationBySName where
  toQueryId = Glean.Schema.Query.Python.Types.DeclarationBySName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.DeclarationBySName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.DeclarationBySName = Glean.Schema.Python.Types.DeclarationBySName
type instance Glean.QueryOf Glean.Schema.Python.Types.DeclarationBySName = Glean.Schema.Query.Python.Types.DeclarationBySName

instance Glean.ToQuery Glean.Schema.Python.Types.DeclarationBySName

instance Glean.PredicateQuery Glean.Schema.Python.Types.Name where
  toQueryId = Glean.Schema.Query.Python.Types.Name_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.Name_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.Name = Glean.Schema.Python.Types.Name
type instance Glean.QueryOf Glean.Schema.Python.Types.Name = Glean.Schema.Query.Python.Types.Name

instance Glean.ToQuery Glean.Schema.Python.Types.Name

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ModuleDefinition_key = Glean.Schema.Python.Types.ModuleDefinition_key
type instance Glean.QueryOf Glean.Schema.Python.Types.ModuleDefinition_key = Glean.Schema.Query.Python.Types.ModuleDefinition_key

instance Glean.ToQuery Glean.Schema.Python.Types.ModuleDefinition_key where
  toQuery (Glean.Schema.Python.Types.ModuleDefinition_key x1 x2) = Glean.Schema.Query.Python.Types.ModuleDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Python.Types.moduleDefinition_docstring_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Python.Types.moduleDefinition_docstring_just = Prelude.Just (Glean.toQuery x)})) x2))

instance Glean.PredicateQuery Glean.Schema.Python.Types.ModuleDefinition where
  toQueryId = Glean.Schema.Query.Python.Types.ModuleDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.ModuleDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ModuleDefinition = Glean.Schema.Python.Types.ModuleDefinition
type instance Glean.QueryOf Glean.Schema.Python.Types.ModuleDefinition = Glean.Schema.Query.Python.Types.ModuleDefinition

instance Glean.ToQuery Glean.Schema.Python.Types.ModuleDefinition

instance Glean.PredicateQuery Glean.Schema.Python.Types.SNameToName where
  toQueryId = Glean.Schema.Query.Python.Types.SNameToName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.SNameToName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.SNameToName = Glean.Schema.Python.Types.SNameToName
type instance Glean.QueryOf Glean.Schema.Python.Types.SNameToName = Glean.Schema.Query.Python.Types.SNameToName

instance Glean.ToQuery Glean.Schema.Python.Types.SNameToName

instance Glean.PredicateQuery Glean.Schema.Python.Types.NameToSName where
  toQueryId = Glean.Schema.Query.Python.Types.NameToSName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.NameToSName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.NameToSName = Glean.Schema.Python.Types.NameToSName
type instance Glean.QueryOf Glean.Schema.Python.Types.NameToSName = Glean.Schema.Query.Python.Types.NameToSName

instance Glean.ToQuery Glean.Schema.Python.Types.NameToSName

instance Glean.PredicateQuery Glean.Schema.Python.Types.Type where
  toQueryId = Glean.Schema.Query.Python.Types.Type__with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.Type__with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.Type_ = Glean.Schema.Python.Types.Type
type instance Glean.QueryOf Glean.Schema.Python.Types.Type = Glean.Schema.Query.Python.Types.Type_

instance Glean.ToQuery Glean.Schema.Python.Types.Type

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.DeclarationWithName_key = Glean.Schema.Python.Types.DeclarationWithName_key
type instance Glean.QueryOf Glean.Schema.Python.Types.DeclarationWithName_key = Glean.Schema.Query.Python.Types.DeclarationWithName_key

instance Glean.ToQuery Glean.Schema.Python.Types.DeclarationWithName_key where
  toQuery (Glean.Schema.Python.Types.DeclarationWithName_key x1 x2) = Glean.Schema.Query.Python.Types.DeclarationWithName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Python.Types.DeclarationWithName where
  toQueryId = Glean.Schema.Query.Python.Types.DeclarationWithName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.DeclarationWithName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.DeclarationWithName = Glean.Schema.Python.Types.DeclarationWithName
type instance Glean.QueryOf Glean.Schema.Python.Types.DeclarationWithName = Glean.Schema.Query.Python.Types.DeclarationWithName

instance Glean.ToQuery Glean.Schema.Python.Types.DeclarationWithName

instance Glean.PredicateQuery Glean.Schema.Python.Types.FunctionBySName where
  toQueryId = Glean.Schema.Query.Python.Types.FunctionBySName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.FunctionBySName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.FunctionBySName = Glean.Schema.Python.Types.FunctionBySName
type instance Glean.QueryOf Glean.Schema.Python.Types.FunctionBySName = Glean.Schema.Query.Python.Types.FunctionBySName

instance Glean.ToQuery Glean.Schema.Python.Types.FunctionBySName

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ImportStatementByAsSName_key = Glean.Schema.Python.Types.ImportStatementByAsSName_key
type instance Glean.QueryOf Glean.Schema.Python.Types.ImportStatementByAsSName_key = Glean.Schema.Query.Python.Types.ImportStatementByAsSName_key

instance Glean.ToQuery Glean.Schema.Python.Types.ImportStatementByAsSName_key where
  toQuery (Glean.Schema.Python.Types.ImportStatementByAsSName_key x1 x2) = Glean.Schema.Query.Python.Types.ImportStatementByAsSName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Python.Types.ImportStatementByAsSName where
  toQueryId = Glean.Schema.Query.Python.Types.ImportStatementByAsSName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.ImportStatementByAsSName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ImportStatementByAsSName = Glean.Schema.Python.Types.ImportStatementByAsSName
type instance Glean.QueryOf Glean.Schema.Python.Types.ImportStatementByAsSName = Glean.Schema.Query.Python.Types.ImportStatementByAsSName

instance Glean.ToQuery Glean.Schema.Python.Types.ImportStatementByAsSName

instance Glean.PredicateQuery Glean.Schema.Python.Types.ImportStatementByAsSName_2 where
  toQueryId = Glean.Schema.Query.Python.Types.ImportStatementByAsSName_2_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.ImportStatementByAsSName_2_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ImportStatementByAsSName_2 = Glean.Schema.Python.Types.ImportStatementByAsSName_2
type instance Glean.QueryOf Glean.Schema.Python.Types.ImportStatementByAsSName_2 = Glean.Schema.Query.Python.Types.ImportStatementByAsSName_2

instance Glean.ToQuery Glean.Schema.Python.Types.ImportStatementByAsSName_2

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ContainingTopLevelDeclaration_key = Glean.Schema.Python.Types.ContainingTopLevelDeclaration_key
type instance Glean.QueryOf Glean.Schema.Python.Types.ContainingTopLevelDeclaration_key = Glean.Schema.Query.Python.Types.ContainingTopLevelDeclaration_key

instance Glean.ToQuery Glean.Schema.Python.Types.ContainingTopLevelDeclaration_key where
  toQuery (Glean.Schema.Python.Types.ContainingTopLevelDeclaration_key x1 x2) = Glean.Schema.Query.Python.Types.ContainingTopLevelDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Python.Types.ContainingTopLevelDeclaration where
  toQueryId = Glean.Schema.Query.Python.Types.ContainingTopLevelDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.ContainingTopLevelDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ContainingTopLevelDeclaration = Glean.Schema.Python.Types.ContainingTopLevelDeclaration
type instance Glean.QueryOf Glean.Schema.Python.Types.ContainingTopLevelDeclaration = Glean.Schema.Query.Python.Types.ContainingTopLevelDeclaration

instance Glean.ToQuery Glean.Schema.Python.Types.ContainingTopLevelDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.VariableDeclaration_key = Glean.Schema.Python.Types.VariableDeclaration_key
type instance Glean.QueryOf Glean.Schema.Python.Types.VariableDeclaration_key = Glean.Schema.Query.Python.Types.VariableDeclaration_key

instance Glean.ToQuery Glean.Schema.Python.Types.VariableDeclaration_key where
  toQuery (Glean.Schema.Python.Types.VariableDeclaration_key x1) = Glean.Schema.Query.Python.Types.VariableDeclaration_key (Prelude.Just (Glean.toQuery x1))

instance Glean.PredicateQuery Glean.Schema.Python.Types.VariableDeclaration where
  toQueryId = Glean.Schema.Query.Python.Types.VariableDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.VariableDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.VariableDeclaration = Glean.Schema.Python.Types.VariableDeclaration
type instance Glean.QueryOf Glean.Schema.Python.Types.VariableDeclaration = Glean.Schema.Query.Python.Types.VariableDeclaration

instance Glean.ToQuery Glean.Schema.Python.Types.VariableDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.DeclarationsByFile_key = Glean.Schema.Python.Types.DeclarationsByFile_key
type instance Glean.QueryOf Glean.Schema.Python.Types.DeclarationsByFile_key = Glean.Schema.Query.Python.Types.DeclarationsByFile_key

instance Glean.ToQuery Glean.Schema.Python.Types.DeclarationsByFile_key where
  toQuery (Glean.Schema.Python.Types.DeclarationsByFile_key x1 x2 x3) = Glean.Schema.Query.Python.Types.DeclarationsByFile_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Python.Types.DeclarationsByFile where
  toQueryId = Glean.Schema.Query.Python.Types.DeclarationsByFile_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.DeclarationsByFile_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.DeclarationsByFile = Glean.Schema.Python.Types.DeclarationsByFile
type instance Glean.QueryOf Glean.Schema.Python.Types.DeclarationsByFile = Glean.Schema.Query.Python.Types.DeclarationsByFile

instance Glean.ToQuery Glean.Schema.Python.Types.DeclarationsByFile

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.XRefsViaNameByFile_key = Glean.Schema.Python.Types.XRefsViaNameByFile_key
type instance Glean.QueryOf Glean.Schema.Python.Types.XRefsViaNameByFile_key = Glean.Schema.Query.Python.Types.XRefsViaNameByFile_key

instance Glean.ToQuery Glean.Schema.Python.Types.XRefsViaNameByFile_key where
  toQuery (Glean.Schema.Python.Types.XRefsViaNameByFile_key x1 x2) = Glean.Schema.Query.Python.Types.XRefsViaNameByFile_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Python.Types.XRefsViaNameByFile_xrefs_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Python.Types.XRefsViaNameByFile where
  toQueryId = Glean.Schema.Query.Python.Types.XRefsViaNameByFile_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.XRefsViaNameByFile_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.XRefsViaNameByFile = Glean.Schema.Python.Types.XRefsViaNameByFile
type instance Glean.QueryOf Glean.Schema.Python.Types.XRefsViaNameByFile = Glean.Schema.Query.Python.Types.XRefsViaNameByFile

instance Glean.ToQuery Glean.Schema.Python.Types.XRefsViaNameByFile

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ImportStatementByAsName_key = Glean.Schema.Python.Types.ImportStatementByAsName_key
type instance Glean.QueryOf Glean.Schema.Python.Types.ImportStatementByAsName_key = Glean.Schema.Query.Python.Types.ImportStatementByAsName_key

instance Glean.ToQuery Glean.Schema.Python.Types.ImportStatementByAsName_key where
  toQuery (Glean.Schema.Python.Types.ImportStatementByAsName_key x1 x2) = Glean.Schema.Query.Python.Types.ImportStatementByAsName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Python.Types.ImportStatementByAsName where
  toQueryId = Glean.Schema.Query.Python.Types.ImportStatementByAsName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.ImportStatementByAsName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ImportStatementByAsName = Glean.Schema.Python.Types.ImportStatementByAsName
type instance Glean.QueryOf Glean.Schema.Python.Types.ImportStatementByAsName = Glean.Schema.Query.Python.Types.ImportStatementByAsName

instance Glean.ToQuery Glean.Schema.Python.Types.ImportStatementByAsName

instance Glean.PredicateQuery Glean.Schema.Python.Types.ImportStatementByAsName_2 where
  toQueryId = Glean.Schema.Query.Python.Types.ImportStatementByAsName_2_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Python.Types.ImportStatementByAsName_2_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.ImportStatementByAsName_2 = Glean.Schema.Python.Types.ImportStatementByAsName_2
type instance Glean.QueryOf Glean.Schema.Python.Types.ImportStatementByAsName_2 = Glean.Schema.Query.Python.Types.ImportStatementByAsName_2

instance Glean.ToQuery Glean.Schema.Python.Types.ImportStatementByAsName_2

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.DirectXRef = Glean.Schema.Python.Types.DirectXRef
type instance Glean.QueryOf Glean.Schema.Python.Types.DirectXRef = Glean.Schema.Query.Python.Types.DirectXRef

instance Glean.ToQuery Glean.Schema.Python.Types.DirectXRef where
  toQuery (Glean.Schema.Python.Types.DirectXRef x1 x2) = Glean.Schema.Query.Python.Types.DirectXRef (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.XRef = Glean.Schema.Python.Types.XRef
type instance Glean.QueryOf Glean.Schema.Python.Types.XRef = Glean.Schema.Query.Python.Types.XRef

instance Glean.ToQuery Glean.Schema.Python.Types.XRef where
  toQuery (Glean.Schema.Python.Types.XRef x1 x2) = Glean.Schema.Query.Python.Types.XRef (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.XRef_1 = Glean.Schema.Python.Types.XRef_1
type instance Glean.QueryOf Glean.Schema.Python.Types.XRef_1 = Glean.Schema.Query.Python.Types.XRef_1

instance Glean.ToQuery Glean.Schema.Python.Types.XRef_1 where
  toQuery (Glean.Schema.Python.Types.XRef_1 x1 x2) = Glean.Schema.Query.Python.Types.XRef_1 (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.Declaration = Glean.Schema.Python.Types.Declaration
type instance Glean.QueryOf Glean.Schema.Python.Types.Declaration = Glean.Schema.Query.Python.Types.Declaration

instance Glean.ToQuery Glean.Schema.Python.Types.Declaration where
  toQuery (Glean.Schema.Python.Types.Declaration_cls x) = Data.Default.def { Glean.Schema.Query.Python.Types.declaration_cls = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Python.Types.Declaration_func x) = Data.Default.def { Glean.Schema.Query.Python.Types.declaration_func = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Python.Types.Declaration_variable x) = Data.Default.def { Glean.Schema.Query.Python.Types.declaration_variable = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Python.Types.Declaration_imp x) = Data.Default.def { Glean.Schema.Query.Python.Types.declaration_imp = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Python.Types.Declaration_module x) = Data.Default.def { Glean.Schema.Query.Python.Types.declaration_module = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Python.Types.ClassDeclaration Glean.Schema.Query.Python.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Python.Types.declaration_cls = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Python.Types.FunctionDeclaration Glean.Schema.Query.Python.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Python.Types.declaration_func = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Python.Types.VariableDeclaration Glean.Schema.Query.Python.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Python.Types.declaration_variable = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Python.Types.ImportStatement Glean.Schema.Query.Python.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Python.Types.declaration_imp = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Python.Types.Module Glean.Schema.Query.Python.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Python.Types.declaration_module = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.Declaration_1 = Glean.Schema.Python.Types.Declaration_1
type instance Glean.QueryOf Glean.Schema.Python.Types.Declaration_1 = Glean.Schema.Query.Python.Types.Declaration_1

instance Glean.ToQuery Glean.Schema.Python.Types.Declaration_1 where
  toQuery (Glean.Schema.Python.Types.Declaration_1_cls x) = Data.Default.def { Glean.Schema.Query.Python.Types.declaration_1_cls = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Python.Types.Declaration_1_func x) = Data.Default.def { Glean.Schema.Query.Python.Types.declaration_1_func = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Python.Types.Declaration_1_variable x) = Data.Default.def { Glean.Schema.Query.Python.Types.declaration_1_variable = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Python.Types.ClassDeclaration Glean.Schema.Query.Python.Types.Declaration_1 where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Python.Types.declaration_1_cls = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Python.Types.FunctionDeclaration Glean.Schema.Query.Python.Types.Declaration_1 where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Python.Types.declaration_1_func = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Python.Types.VariableDeclaration Glean.Schema.Query.Python.Types.Declaration_1 where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Python.Types.declaration_1_variable = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.XRefViaName = Glean.Schema.Python.Types.XRefViaName
type instance Glean.QueryOf Glean.Schema.Python.Types.XRefViaName = Glean.Schema.Query.Python.Types.XRefViaName

instance Glean.ToQuery Glean.Schema.Python.Types.XRefViaName where
  toQuery (Glean.Schema.Python.Types.XRefViaName x1 x2) = Glean.Schema.Query.Python.Types.XRefViaName (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.Docstring = Glean.Schema.Python.Types.Docstring
type instance Glean.QueryOf Glean.Schema.Python.Types.Docstring = Glean.Schema.Query.Python.Types.Docstring

instance Glean.ToQuery Glean.Schema.Python.Types.Docstring where
  toQuery (Glean.Schema.Python.Types.Docstring x1) = Glean.Schema.Query.Python.Types.Docstring (Prelude.Just (Glean.toQuery x1))

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.Parameter = Glean.Schema.Python.Types.Parameter
type instance Glean.QueryOf Glean.Schema.Python.Types.Parameter = Glean.Schema.Query.Python.Types.Parameter

instance Glean.ToQuery Glean.Schema.Python.Types.Parameter where
  toQuery (Glean.Schema.Python.Types.Parameter x1 x2 x3) = Glean.Schema.Query.Python.Types.Parameter (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Python.Types.parameter_type_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Python.Types.parameter_type_just = Prelude.Just (Glean.toQuery x)})) x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Python.Types.parameter_value_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Python.Types.parameter_value_just = Prelude.Just (Glean.toQuery x)})) x3))

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.XRefTarget = Glean.Schema.Python.Types.XRefTarget
type instance Glean.QueryOf Glean.Schema.Python.Types.XRefTarget = Glean.Schema.Query.Python.Types.XRefTarget

instance Glean.ToQuery Glean.Schema.Python.Types.XRefTarget where
  toQuery (Glean.Schema.Python.Types.XRefTarget_declaration x) = Data.Default.def { Glean.Schema.Query.Python.Types.xRefTarget_declaration = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Python.Types.XRefTarget_indirect x) = Data.Default.def { Glean.Schema.Query.Python.Types.xRefTarget_indirect = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Python.Types.XRefTarget_unknown x) = Data.Default.def { Glean.Schema.Query.Python.Types.xRefTarget_unknown = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Python.Types.Declaration Glean.Schema.Query.Python.Types.XRefTarget where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Python.Types.xRefTarget_declaration = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Python.Types.XRefIndirectTarget Glean.Schema.Query.Python.Types.XRefTarget where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Python.Types.xRefTarget_indirect = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Builtin.Types.Unit Glean.Schema.Query.Python.Types.XRefTarget where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Python.Types.xRefTarget_unknown = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Python.Types.XRefTarget_1 = Glean.Schema.Python.Types.XRefTarget_1
type instance Glean.QueryOf Glean.Schema.Python.Types.XRefTarget_1 = Glean.Schema.Query.Python.Types.XRefTarget_1

instance Glean.ToQuery Glean.Schema.Python.Types.XRefTarget_1 where
  toQuery (Glean.Schema.Python.Types.XRefTarget_1_declaration x) = Data.Default.def { Glean.Schema.Query.Python.Types.xRefTarget_1_declaration = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Python.Types.XRefTarget_1_module x) = Data.Default.def { Glean.Schema.Query.Python.Types.xRefTarget_1_module = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Python.Types.XRefTarget_1_indirect x) = Data.Default.def { Glean.Schema.Query.Python.Types.xRefTarget_1_indirect = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Python.Types.XRefTarget_1_unknown x) = Data.Default.def { Glean.Schema.Query.Python.Types.xRefTarget_1_unknown = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Python.Types.Declaration_1 Glean.Schema.Query.Python.Types.XRefTarget_1 where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Python.Types.xRefTarget_1_declaration = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Python.Types.Module Glean.Schema.Query.Python.Types.XRefTarget_1 where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Python.Types.xRefTarget_1_module = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Python.Types.XRefIndirectTarget_1 Glean.Schema.Query.Python.Types.XRefTarget_1 where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Python.Types.xRefTarget_1_indirect = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Builtin.Types.Unit Glean.Schema.Query.Python.Types.XRefTarget_1 where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Python.Types.xRefTarget_1_unknown = Prelude.Just q }
