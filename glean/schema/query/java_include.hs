-- @generated
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DataKinds #-}
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

import qualified Glean.Schema.Java.Types


type instance Glean.QueryResult Glean.Schema.Query.Java.Types.XRef_key = Glean.Schema.Java.Types.XRef_key
type instance Glean.QueryOf Glean.Schema.Java.Types.XRef_key = Glean.Schema.Query.Java.Types.XRef_key

instance Glean.ToQuery Glean.Schema.Java.Types.XRef_key where
  toQuery (Glean.Schema.Java.Types.XRef_key x1 x2) = Glean.Schema.Query.Java.Types.XRef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Java.Types.XRef_ranges_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Java.Types.XRef where
  toQueryId = Glean.Schema.Query.Java.Types.XRef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.XRef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.XRef = Glean.Schema.Java.Types.XRef
type instance Glean.QueryOf Glean.Schema.Java.Types.XRef = Glean.Schema.Query.Java.Types.XRef

instance Glean.ToQuery Glean.Schema.Java.Types.XRef

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.TypeParam_key = Glean.Schema.Java.Types.TypeParam_key
type instance Glean.QueryOf Glean.Schema.Java.Types.TypeParam_key = Glean.Schema.Query.Java.Types.TypeParam_key

instance Glean.ToQuery Glean.Schema.Java.Types.TypeParam_key where
  toQuery (Glean.Schema.Java.Types.TypeParam_key x1 x2) = Glean.Schema.Query.Java.Types.TypeParam_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Java.Types.TypeParam_extends__array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Java.Types.TypeParam where
  toQueryId = Glean.Schema.Query.Java.Types.TypeParam_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.TypeParam_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.TypeParam = Glean.Schema.Java.Types.TypeParam
type instance Glean.QueryOf Glean.Schema.Java.Types.TypeParam = Glean.Schema.Query.Java.Types.TypeParam

instance Glean.ToQuery Glean.Schema.Java.Types.TypeParam

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.Annotation_key = Glean.Schema.Java.Types.Annotation_key
type instance Glean.QueryOf Glean.Schema.Java.Types.Annotation_key = Glean.Schema.Query.Java.Types.Annotation_key

instance Glean.ToQuery Glean.Schema.Java.Types.Annotation_key where
  toQuery (Glean.Schema.Java.Types.Annotation_key x1) = Glean.Schema.Query.Java.Types.Annotation_key (Prelude.Just (Glean.toQuery x1))

instance Glean.PredicateQuery Glean.Schema.Java.Types.Annotation where
  toQueryId = Glean.Schema.Query.Java.Types.Annotation_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.Annotation_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.Annotation = Glean.Schema.Java.Types.Annotation
type instance Glean.QueryOf Glean.Schema.Java.Types.Annotation = Glean.Schema.Query.Java.Types.Annotation

instance Glean.ToQuery Glean.Schema.Java.Types.Annotation

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.MethodDeclaration_key = Glean.Schema.Java.Types.MethodDeclaration_key
type instance Glean.QueryOf Glean.Schema.Java.Types.MethodDeclaration_key = Glean.Schema.Query.Java.Types.MethodDeclaration_key

instance Glean.ToQuery Glean.Schema.Java.Types.MethodDeclaration_key where
  toQuery (Glean.Schema.Java.Types.MethodDeclaration_key x1 x2 x3 x4 x5 x6 x7) = Glean.Schema.Query.Java.Types.MethodDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Java.Types.MethodDeclaration_parameters_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just ((Glean.Schema.Query.Java.Types.MethodDeclaration_annotations_array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just ((Glean.Schema.Query.Java.Types.MethodDeclaration_modifiers_array_exact . Prelude.map Glean.toQuery) x5)) (Prelude.Just ((Glean.Schema.Query.Java.Types.MethodDeclaration_typeParams_array_exact . Prelude.map Glean.toQuery) x6)) (Prelude.Just (Glean.toQuery x7))

instance Glean.PredicateQuery Glean.Schema.Java.Types.MethodDeclaration where
  toQueryId = Glean.Schema.Query.Java.Types.MethodDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.MethodDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.MethodDeclaration = Glean.Schema.Java.Types.MethodDeclaration
type instance Glean.QueryOf Glean.Schema.Java.Types.MethodDeclaration = Glean.Schema.Query.Java.Types.MethodDeclaration

instance Glean.ToQuery Glean.Schema.Java.Types.MethodDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.MethodDeclaration_3_key = Glean.Schema.Java.Types.MethodDeclaration_3_key
type instance Glean.QueryOf Glean.Schema.Java.Types.MethodDeclaration_3_key = Glean.Schema.Query.Java.Types.MethodDeclaration_3_key

instance Glean.ToQuery Glean.Schema.Java.Types.MethodDeclaration_3_key where
  toQuery (Glean.Schema.Java.Types.MethodDeclaration_3_key x1 x2 x3 x4 x5 x6) = Glean.Schema.Query.Java.Types.MethodDeclaration_3_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Java.Types.MethodDeclaration_3_parameters_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just ((Glean.Schema.Query.Java.Types.MethodDeclaration_3_modifiers_array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just ((Glean.Schema.Query.Java.Types.MethodDeclaration_3_typeParams_array_exact . Prelude.map Glean.toQuery) x5)) (Prelude.Just (Glean.toQuery x6))

instance Glean.PredicateQuery Glean.Schema.Java.Types.MethodDeclaration_3 where
  toQueryId = Glean.Schema.Query.Java.Types.MethodDeclaration_3_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.MethodDeclaration_3_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.MethodDeclaration_3 = Glean.Schema.Java.Types.MethodDeclaration_3
type instance Glean.QueryOf Glean.Schema.Java.Types.MethodDeclaration_3 = Glean.Schema.Query.Java.Types.MethodDeclaration_3

instance Glean.ToQuery Glean.Schema.Java.Types.MethodDeclaration_3

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.MethodDeclaration_2_key = Glean.Schema.Java.Types.MethodDeclaration_2_key
type instance Glean.QueryOf Glean.Schema.Java.Types.MethodDeclaration_2_key = Glean.Schema.Query.Java.Types.MethodDeclaration_2_key

instance Glean.ToQuery Glean.Schema.Java.Types.MethodDeclaration_2_key where
  toQuery (Glean.Schema.Java.Types.MethodDeclaration_2_key x1 x2 x3 x4 x5) = Glean.Schema.Query.Java.Types.MethodDeclaration_2_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Java.Types.MethodDeclaration_2_parameters_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just ((Glean.Schema.Query.Java.Types.MethodDeclaration_2_modifiers_array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just (Glean.toQuery x5))

instance Glean.PredicateQuery Glean.Schema.Java.Types.MethodDeclaration_2 where
  toQueryId = Glean.Schema.Query.Java.Types.MethodDeclaration_2_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.MethodDeclaration_2_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.MethodDeclaration_2 = Glean.Schema.Java.Types.MethodDeclaration_2
type instance Glean.QueryOf Glean.Schema.Java.Types.MethodDeclaration_2 = Glean.Schema.Query.Java.Types.MethodDeclaration_2

instance Glean.ToQuery Glean.Schema.Java.Types.MethodDeclaration_2

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.AnnotatedClass_key = Glean.Schema.Java.Types.AnnotatedClass_key
type instance Glean.QueryOf Glean.Schema.Java.Types.AnnotatedClass_key = Glean.Schema.Query.Java.Types.AnnotatedClass_key

instance Glean.ToQuery Glean.Schema.Java.Types.AnnotatedClass_key where
  toQuery (Glean.Schema.Java.Types.AnnotatedClass_key x1 x2) = Glean.Schema.Query.Java.Types.AnnotatedClass_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Java.Types.AnnotatedClass where
  toQueryId = Glean.Schema.Query.Java.Types.AnnotatedClass_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.AnnotatedClass_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.AnnotatedClass = Glean.Schema.Java.Types.AnnotatedClass
type instance Glean.QueryOf Glean.Schema.Java.Types.AnnotatedClass = Glean.Schema.Query.Java.Types.AnnotatedClass

instance Glean.ToQuery Glean.Schema.Java.Types.AnnotatedClass

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.InterfaceDeclaration_key = Glean.Schema.Java.Types.InterfaceDeclaration_key
type instance Glean.QueryOf Glean.Schema.Java.Types.InterfaceDeclaration_key = Glean.Schema.Query.Java.Types.InterfaceDeclaration_key

instance Glean.ToQuery Glean.Schema.Java.Types.InterfaceDeclaration_key where
  toQuery (Glean.Schema.Java.Types.InterfaceDeclaration_key x1 x2 x3 x4 x5 x6 x7) = Glean.Schema.Query.Java.Types.InterfaceDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Java.Types.InterfaceDeclaration_annotations_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Java.Types.InterfaceDeclaration_modifiers_array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just ((Glean.Schema.Query.Java.Types.InterfaceDeclaration_extends__array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just ((Glean.Schema.Query.Java.Types.InterfaceDeclaration_methods_array_exact . Prelude.map Glean.toQuery) x5)) (Prelude.Just ((Glean.Schema.Query.Java.Types.InterfaceDeclaration_typeParams_array_exact . Prelude.map Glean.toQuery) x6)) (Prelude.Just (Glean.toQuery x7))

instance Glean.PredicateQuery Glean.Schema.Java.Types.InterfaceDeclaration where
  toQueryId = Glean.Schema.Query.Java.Types.InterfaceDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.InterfaceDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.InterfaceDeclaration = Glean.Schema.Java.Types.InterfaceDeclaration
type instance Glean.QueryOf Glean.Schema.Java.Types.InterfaceDeclaration = Glean.Schema.Query.Java.Types.InterfaceDeclaration

instance Glean.ToQuery Glean.Schema.Java.Types.InterfaceDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.InterfaceDeclaration_3_key = Glean.Schema.Java.Types.InterfaceDeclaration_3_key
type instance Glean.QueryOf Glean.Schema.Java.Types.InterfaceDeclaration_3_key = Glean.Schema.Query.Java.Types.InterfaceDeclaration_3_key

instance Glean.ToQuery Glean.Schema.Java.Types.InterfaceDeclaration_3_key where
  toQuery (Glean.Schema.Java.Types.InterfaceDeclaration_3_key x1 x2 x3 x4 x5 x6) = Glean.Schema.Query.Java.Types.InterfaceDeclaration_3_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Java.Types.InterfaceDeclaration_3_modifiers_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Java.Types.InterfaceDeclaration_3_extends__array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just ((Glean.Schema.Query.Java.Types.InterfaceDeclaration_3_methods_array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just ((Glean.Schema.Query.Java.Types.InterfaceDeclaration_3_typeParams_array_exact . Prelude.map Glean.toQuery) x5)) (Prelude.Just (Glean.toQuery x6))

instance Glean.PredicateQuery Glean.Schema.Java.Types.InterfaceDeclaration_3 where
  toQueryId = Glean.Schema.Query.Java.Types.InterfaceDeclaration_3_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.InterfaceDeclaration_3_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.InterfaceDeclaration_3 = Glean.Schema.Java.Types.InterfaceDeclaration_3
type instance Glean.QueryOf Glean.Schema.Java.Types.InterfaceDeclaration_3 = Glean.Schema.Query.Java.Types.InterfaceDeclaration_3

instance Glean.ToQuery Glean.Schema.Java.Types.InterfaceDeclaration_3

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.InterfaceDeclaration_2_key = Glean.Schema.Java.Types.InterfaceDeclaration_2_key
type instance Glean.QueryOf Glean.Schema.Java.Types.InterfaceDeclaration_2_key = Glean.Schema.Query.Java.Types.InterfaceDeclaration_2_key

instance Glean.ToQuery Glean.Schema.Java.Types.InterfaceDeclaration_2_key where
  toQuery (Glean.Schema.Java.Types.InterfaceDeclaration_2_key x1 x2 x3 x4 x5) = Glean.Schema.Query.Java.Types.InterfaceDeclaration_2_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Java.Types.InterfaceDeclaration_2_modifiers_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Java.Types.InterfaceDeclaration_2_extends__array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just ((Glean.Schema.Query.Java.Types.InterfaceDeclaration_2_methods_array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just (Glean.toQuery x5))

instance Glean.PredicateQuery Glean.Schema.Java.Types.InterfaceDeclaration_2 where
  toQueryId = Glean.Schema.Query.Java.Types.InterfaceDeclaration_2_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.InterfaceDeclaration_2_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.InterfaceDeclaration_2 = Glean.Schema.Java.Types.InterfaceDeclaration_2
type instance Glean.QueryOf Glean.Schema.Java.Types.InterfaceDeclaration_2 = Glean.Schema.Query.Java.Types.InterfaceDeclaration_2

instance Glean.ToQuery Glean.Schema.Java.Types.InterfaceDeclaration_2

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.ClassDeclaration_key = Glean.Schema.Java.Types.ClassDeclaration_key
type instance Glean.QueryOf Glean.Schema.Java.Types.ClassDeclaration_key = Glean.Schema.Query.Java.Types.ClassDeclaration_key

instance Glean.ToQuery Glean.Schema.Java.Types.ClassDeclaration_key where
  toQuery (Glean.Schema.Java.Types.ClassDeclaration_key x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = Glean.Schema.Query.Java.Types.ClassDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ClassDeclaration_modifiers_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Java.Types.classDeclaration_extends__nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Java.Types.classDeclaration_extends__just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ClassDeclaration_implements__array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ClassDeclaration_annotations_array_exact . Prelude.map Glean.toQuery) x5)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ClassDeclaration_variables_array_exact . Prelude.map Glean.toQuery) x6)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ClassDeclaration_constructors_array_exact . Prelude.map Glean.toQuery) x7)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ClassDeclaration_methods_array_exact . Prelude.map Glean.toQuery) x8)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ClassDeclaration_typeParams_array_exact . Prelude.map Glean.toQuery) x9)) (Prelude.Just (Glean.toQuery x10))

instance Glean.PredicateQuery Glean.Schema.Java.Types.ClassDeclaration where
  toQueryId = Glean.Schema.Query.Java.Types.ClassDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.ClassDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.ClassDeclaration = Glean.Schema.Java.Types.ClassDeclaration
type instance Glean.QueryOf Glean.Schema.Java.Types.ClassDeclaration = Glean.Schema.Query.Java.Types.ClassDeclaration

instance Glean.ToQuery Glean.Schema.Java.Types.ClassDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.ClassDeclaration_3_key = Glean.Schema.Java.Types.ClassDeclaration_3_key
type instance Glean.QueryOf Glean.Schema.Java.Types.ClassDeclaration_3_key = Glean.Schema.Query.Java.Types.ClassDeclaration_3_key

instance Glean.ToQuery Glean.Schema.Java.Types.ClassDeclaration_3_key where
  toQuery (Glean.Schema.Java.Types.ClassDeclaration_3_key x1 x2 x3 x4 x5 x6 x7 x8 x9) = Glean.Schema.Query.Java.Types.ClassDeclaration_3_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ClassDeclaration_3_modifiers_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Java.Types.classDeclaration_3_extends__nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Java.Types.classDeclaration_3_extends__just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ClassDeclaration_3_implements__array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ClassDeclaration_3_variables_array_exact . Prelude.map Glean.toQuery) x5)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ClassDeclaration_3_constructors_array_exact . Prelude.map Glean.toQuery) x6)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ClassDeclaration_3_methods_array_exact . Prelude.map Glean.toQuery) x7)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ClassDeclaration_3_typeParams_array_exact . Prelude.map Glean.toQuery) x8)) (Prelude.Just (Glean.toQuery x9))

instance Glean.PredicateQuery Glean.Schema.Java.Types.ClassDeclaration_3 where
  toQueryId = Glean.Schema.Query.Java.Types.ClassDeclaration_3_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.ClassDeclaration_3_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.ClassDeclaration_3 = Glean.Schema.Java.Types.ClassDeclaration_3
type instance Glean.QueryOf Glean.Schema.Java.Types.ClassDeclaration_3 = Glean.Schema.Query.Java.Types.ClassDeclaration_3

instance Glean.ToQuery Glean.Schema.Java.Types.ClassDeclaration_3

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.ClassDeclaration_2_key = Glean.Schema.Java.Types.ClassDeclaration_2_key
type instance Glean.QueryOf Glean.Schema.Java.Types.ClassDeclaration_2_key = Glean.Schema.Query.Java.Types.ClassDeclaration_2_key

instance Glean.ToQuery Glean.Schema.Java.Types.ClassDeclaration_2_key where
  toQuery (Glean.Schema.Java.Types.ClassDeclaration_2_key x1 x2 x3 x4 x5 x6 x7 x8) = Glean.Schema.Query.Java.Types.ClassDeclaration_2_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ClassDeclaration_2_modifiers_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Java.Types.classDeclaration_2_extends__nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Java.Types.classDeclaration_2_extends__just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ClassDeclaration_2_implements__array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ClassDeclaration_2_variables_array_exact . Prelude.map Glean.toQuery) x5)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ClassDeclaration_2_constructors_array_exact . Prelude.map Glean.toQuery) x6)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ClassDeclaration_2_methods_array_exact . Prelude.map Glean.toQuery) x7)) (Prelude.Just (Glean.toQuery x8))

instance Glean.PredicateQuery Glean.Schema.Java.Types.ClassDeclaration_2 where
  toQueryId = Glean.Schema.Query.Java.Types.ClassDeclaration_2_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.ClassDeclaration_2_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.ClassDeclaration_2 = Glean.Schema.Java.Types.ClassDeclaration_2
type instance Glean.QueryOf Glean.Schema.Java.Types.ClassDeclaration_2 = Glean.Schema.Query.Java.Types.ClassDeclaration_2

instance Glean.ToQuery Glean.Schema.Java.Types.ClassDeclaration_2

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.FileXRefs_key = Glean.Schema.Java.Types.FileXRefs_key
type instance Glean.QueryOf Glean.Schema.Java.Types.FileXRefs_key = Glean.Schema.Query.Java.Types.FileXRefs_key

instance Glean.ToQuery Glean.Schema.Java.Types.FileXRefs_key where
  toQuery (Glean.Schema.Java.Types.FileXRefs_key x1 x2) = Glean.Schema.Query.Java.Types.FileXRefs_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Java.Types.FileXRefs_xrefs_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Java.Types.FileXRefs where
  toQueryId = Glean.Schema.Query.Java.Types.FileXRefs_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.FileXRefs_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.FileXRefs = Glean.Schema.Java.Types.FileXRefs
type instance Glean.QueryOf Glean.Schema.Java.Types.FileXRefs = Glean.Schema.Query.Java.Types.FileXRefs

instance Glean.ToQuery Glean.Schema.Java.Types.FileXRefs

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.TypeArg_key = Glean.Schema.Java.Types.TypeArg_key
type instance Glean.QueryOf Glean.Schema.Java.Types.TypeArg_key = Glean.Schema.Query.Java.Types.TypeArg_key

instance Glean.ToQuery Glean.Schema.Java.Types.TypeArg_key where
  toQuery (Glean.Schema.Java.Types.TypeArg_key_type x) = Data.Default.def { Glean.Schema.Query.Java.Types.typeArg_key_type = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Java.Types.TypeArg_key_wildcard x) = Data.Default.def { Glean.Schema.Query.Java.Types.typeArg_key_wildcard = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Java.Types.Type_ Glean.Schema.Query.Java.Types.TypeArg_key where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Java.Types.typeArg_key_type = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Java.Types.Wildcard Glean.Schema.Query.Java.Types.TypeArg_key where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Java.Types.typeArg_key_wildcard = Prelude.Just q }

instance Glean.PredicateQuery Glean.Schema.Java.Types.TypeArg where
  toQueryId = Glean.Schema.Query.Java.Types.TypeArg_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.TypeArg_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.TypeArg = Glean.Schema.Java.Types.TypeArg
type instance Glean.QueryOf Glean.Schema.Java.Types.TypeArg = Glean.Schema.Query.Java.Types.TypeArg

instance Glean.ToQuery Glean.Schema.Java.Types.TypeArg

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.Inheritance_key = Glean.Schema.Java.Types.Inheritance_key
type instance Glean.QueryOf Glean.Schema.Java.Types.Inheritance_key = Glean.Schema.Query.Java.Types.Inheritance_key

instance Glean.ToQuery Glean.Schema.Java.Types.Inheritance_key where
  toQuery (Glean.Schema.Java.Types.Inheritance_key x1 x2) = Glean.Schema.Query.Java.Types.Inheritance_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Java.Types.Inheritance where
  toQueryId = Glean.Schema.Query.Java.Types.Inheritance_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.Inheritance_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.Inheritance = Glean.Schema.Java.Types.Inheritance
type instance Glean.QueryOf Glean.Schema.Java.Types.Inheritance = Glean.Schema.Query.Java.Types.Inheritance

instance Glean.ToQuery Glean.Schema.Java.Types.Inheritance

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.Inheritance_3_key = Glean.Schema.Java.Types.Inheritance_3_key
type instance Glean.QueryOf Glean.Schema.Java.Types.Inheritance_3_key = Glean.Schema.Query.Java.Types.Inheritance_3_key

instance Glean.ToQuery Glean.Schema.Java.Types.Inheritance_3_key where
  toQuery (Glean.Schema.Java.Types.Inheritance_3_key x1 x2) = Glean.Schema.Query.Java.Types.Inheritance_3_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Java.Types.Inheritance_3 where
  toQueryId = Glean.Schema.Query.Java.Types.Inheritance_3_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.Inheritance_3_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.Inheritance_3 = Glean.Schema.Java.Types.Inheritance_3
type instance Glean.QueryOf Glean.Schema.Java.Types.Inheritance_3 = Glean.Schema.Query.Java.Types.Inheritance_3

instance Glean.ToQuery Glean.Schema.Java.Types.Inheritance_3

instance Glean.PredicateQuery Glean.Schema.Java.Types.Name where
  toQueryId = Glean.Schema.Query.Java.Types.Name_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.Name_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.Name = Glean.Schema.Java.Types.Name
type instance Glean.QueryOf Glean.Schema.Java.Types.Name = Glean.Schema.Query.Java.Types.Name

instance Glean.ToQuery Glean.Schema.Java.Types.Name

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.ConstructorDeclaration_key = Glean.Schema.Java.Types.ConstructorDeclaration_key
type instance Glean.QueryOf Glean.Schema.Java.Types.ConstructorDeclaration_key = Glean.Schema.Query.Java.Types.ConstructorDeclaration_key

instance Glean.ToQuery Glean.Schema.Java.Types.ConstructorDeclaration_key where
  toQuery (Glean.Schema.Java.Types.ConstructorDeclaration_key x1 x2 x3 x4 x5 x6) = Glean.Schema.Query.Java.Types.ConstructorDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ConstructorDeclaration_parameters_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ConstructorDeclaration_annotations_array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ConstructorDeclaration_modifiers_array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ConstructorDeclaration_typeParams_array_exact . Prelude.map Glean.toQuery) x5)) (Prelude.Just (Glean.toQuery x6))

instance Glean.PredicateQuery Glean.Schema.Java.Types.ConstructorDeclaration where
  toQueryId = Glean.Schema.Query.Java.Types.ConstructorDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.ConstructorDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.ConstructorDeclaration = Glean.Schema.Java.Types.ConstructorDeclaration
type instance Glean.QueryOf Glean.Schema.Java.Types.ConstructorDeclaration = Glean.Schema.Query.Java.Types.ConstructorDeclaration

instance Glean.ToQuery Glean.Schema.Java.Types.ConstructorDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.ConstructorDeclaration_3_key = Glean.Schema.Java.Types.ConstructorDeclaration_3_key
type instance Glean.QueryOf Glean.Schema.Java.Types.ConstructorDeclaration_3_key = Glean.Schema.Query.Java.Types.ConstructorDeclaration_3_key

instance Glean.ToQuery Glean.Schema.Java.Types.ConstructorDeclaration_3_key where
  toQuery (Glean.Schema.Java.Types.ConstructorDeclaration_3_key x1 x2 x3 x4 x5) = Glean.Schema.Query.Java.Types.ConstructorDeclaration_3_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ConstructorDeclaration_3_parameters_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ConstructorDeclaration_3_modifiers_array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ConstructorDeclaration_3_typeParams_array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just (Glean.toQuery x5))

instance Glean.PredicateQuery Glean.Schema.Java.Types.ConstructorDeclaration_3 where
  toQueryId = Glean.Schema.Query.Java.Types.ConstructorDeclaration_3_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.ConstructorDeclaration_3_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.ConstructorDeclaration_3 = Glean.Schema.Java.Types.ConstructorDeclaration_3
type instance Glean.QueryOf Glean.Schema.Java.Types.ConstructorDeclaration_3 = Glean.Schema.Query.Java.Types.ConstructorDeclaration_3

instance Glean.ToQuery Glean.Schema.Java.Types.ConstructorDeclaration_3

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.ConstructorDeclaration_2_key = Glean.Schema.Java.Types.ConstructorDeclaration_2_key
type instance Glean.QueryOf Glean.Schema.Java.Types.ConstructorDeclaration_2_key = Glean.Schema.Query.Java.Types.ConstructorDeclaration_2_key

instance Glean.ToQuery Glean.Schema.Java.Types.ConstructorDeclaration_2_key where
  toQuery (Glean.Schema.Java.Types.ConstructorDeclaration_2_key x1 x2 x3) = Glean.Schema.Query.Java.Types.ConstructorDeclaration_2_key (Prelude.Just ((Glean.Schema.Query.Java.Types.ConstructorDeclaration_2_parameters_array_exact . Prelude.map Glean.toQuery) x1)) (Prelude.Just ((Glean.Schema.Query.Java.Types.ConstructorDeclaration_2_modifiers_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Java.Types.ConstructorDeclaration_2 where
  toQueryId = Glean.Schema.Query.Java.Types.ConstructorDeclaration_2_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.ConstructorDeclaration_2_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.ConstructorDeclaration_2 = Glean.Schema.Java.Types.ConstructorDeclaration_2
type instance Glean.QueryOf Glean.Schema.Java.Types.ConstructorDeclaration_2 = Glean.Schema.Query.Java.Types.ConstructorDeclaration_2

instance Glean.ToQuery Glean.Schema.Java.Types.ConstructorDeclaration_2

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.Type_key = Glean.Schema.Java.Types.Type_key
type instance Glean.QueryOf Glean.Schema.Java.Types.Type_key = Glean.Schema.Query.Java.Types.Type_key

instance Glean.ToQuery Glean.Schema.Java.Types.Type_key where
  toQuery (Glean.Schema.Java.Types.Type_key x1 x2) = Glean.Schema.Query.Java.Types.Type_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Java.Types.Type_typeArgs_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Java.Types.Type where
  toQueryId = Glean.Schema.Query.Java.Types.Type__with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.Type__with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.Type_ = Glean.Schema.Java.Types.Type
type instance Glean.QueryOf Glean.Schema.Java.Types.Type = Glean.Schema.Query.Java.Types.Type_

instance Glean.ToQuery Glean.Schema.Java.Types.Type

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.Type_2_key = Glean.Schema.Java.Types.Type_2_key
type instance Glean.QueryOf Glean.Schema.Java.Types.Type_2_key = Glean.Schema.Query.Java.Types.Type_2_key

instance Glean.ToQuery Glean.Schema.Java.Types.Type_2_key where
  toQuery (Glean.Schema.Java.Types.Type_2_key x1) = Glean.Schema.Query.Java.Types.Type_2_key (Prelude.Just (Glean.toQuery x1))

instance Glean.PredicateQuery Glean.Schema.Java.Types.Type_2 where
  toQueryId = Glean.Schema.Query.Java.Types.Type_2_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.Type_2_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.Type_2 = Glean.Schema.Java.Types.Type_2
type instance Glean.QueryOf Glean.Schema.Java.Types.Type_2 = Glean.Schema.Query.Java.Types.Type_2

instance Glean.ToQuery Glean.Schema.Java.Types.Type_2

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.VariableDeclaration_key = Glean.Schema.Java.Types.VariableDeclaration_key
type instance Glean.QueryOf Glean.Schema.Java.Types.VariableDeclaration_key = Glean.Schema.Query.Java.Types.VariableDeclaration_key

instance Glean.ToQuery Glean.Schema.Java.Types.VariableDeclaration_key where
  toQuery (Glean.Schema.Java.Types.VariableDeclaration_key x1 x2 x3 x4 x5) = Glean.Schema.Query.Java.Types.VariableDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Glean.Schema.Query.Java.Types.VariableDeclaration_annotations_array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just ((Glean.Schema.Query.Java.Types.VariableDeclaration_modifiers_array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just (Glean.toQuery x5))

instance Glean.PredicateQuery Glean.Schema.Java.Types.VariableDeclaration where
  toQueryId = Glean.Schema.Query.Java.Types.VariableDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.VariableDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.VariableDeclaration = Glean.Schema.Java.Types.VariableDeclaration
type instance Glean.QueryOf Glean.Schema.Java.Types.VariableDeclaration = Glean.Schema.Query.Java.Types.VariableDeclaration

instance Glean.ToQuery Glean.Schema.Java.Types.VariableDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.VariableDeclaration_3_key = Glean.Schema.Java.Types.VariableDeclaration_3_key
type instance Glean.QueryOf Glean.Schema.Java.Types.VariableDeclaration_3_key = Glean.Schema.Query.Java.Types.VariableDeclaration_3_key

instance Glean.ToQuery Glean.Schema.Java.Types.VariableDeclaration_3_key where
  toQuery (Glean.Schema.Java.Types.VariableDeclaration_3_key x1 x2 x3 x4) = Glean.Schema.Query.Java.Types.VariableDeclaration_3_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Glean.Schema.Query.Java.Types.VariableDeclaration_3_modifiers_array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.Java.Types.VariableDeclaration_3 where
  toQueryId = Glean.Schema.Query.Java.Types.VariableDeclaration_3_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.VariableDeclaration_3_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.VariableDeclaration_3 = Glean.Schema.Java.Types.VariableDeclaration_3
type instance Glean.QueryOf Glean.Schema.Java.Types.VariableDeclaration_3 = Glean.Schema.Query.Java.Types.VariableDeclaration_3

instance Glean.ToQuery Glean.Schema.Java.Types.VariableDeclaration_3

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.VariableDeclaration_2_key = Glean.Schema.Java.Types.VariableDeclaration_2_key
type instance Glean.QueryOf Glean.Schema.Java.Types.VariableDeclaration_2_key = Glean.Schema.Query.Java.Types.VariableDeclaration_2_key

instance Glean.ToQuery Glean.Schema.Java.Types.VariableDeclaration_2_key where
  toQuery (Glean.Schema.Java.Types.VariableDeclaration_2_key x1 x2 x3 x4) = Glean.Schema.Query.Java.Types.VariableDeclaration_2_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Glean.Schema.Query.Java.Types.VariableDeclaration_2_modifiers_array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.Java.Types.VariableDeclaration_2 where
  toQueryId = Glean.Schema.Query.Java.Types.VariableDeclaration_2_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.VariableDeclaration_2_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.VariableDeclaration_2 = Glean.Schema.Java.Types.VariableDeclaration_2
type instance Glean.QueryOf Glean.Schema.Java.Types.VariableDeclaration_2 = Glean.Schema.Query.Java.Types.VariableDeclaration_2

instance Glean.ToQuery Glean.Schema.Java.Types.VariableDeclaration_2

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.QName_key = Glean.Schema.Java.Types.QName_key
type instance Glean.QueryOf Glean.Schema.Java.Types.QName_key = Glean.Schema.Query.Java.Types.QName_key

instance Glean.ToQuery Glean.Schema.Java.Types.QName_key where
  toQuery (Glean.Schema.Java.Types.QName_key x1 x2 x3) = Glean.Schema.Query.Java.Types.QName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Java.Types.qName_fqn_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Java.Types.qName_fqn_just = Prelude.Just (Glean.toQuery x)})) x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Java.Types.qName_signature_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Java.Types.qName_signature_just = Prelude.Just (Glean.toQuery x)})) x3))

instance Glean.PredicateQuery Glean.Schema.Java.Types.QName where
  toQueryId = Glean.Schema.Query.Java.Types.QName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Java.Types.QName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.QName = Glean.Schema.Java.Types.QName
type instance Glean.QueryOf Glean.Schema.Java.Types.QName = Glean.Schema.Query.Java.Types.QName

instance Glean.ToQuery Glean.Schema.Java.Types.QName

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.Modifier = Glean.Schema.Java.Types.Modifier
type instance Glean.QueryOf Glean.Schema.Java.Types.Modifier = Glean.Schema.Query.Java.Types.Modifier

instance Glean.ToQuery Glean.Schema.Java.Types.Modifier where
  toQuery Glean.Schema.Java.Types.Modifier_abstract_ = Glean.Schema.Query.Java.Types.Modifier_abstract_
  toQuery Glean.Schema.Java.Types.Modifier_default_ = Glean.Schema.Query.Java.Types.Modifier_default_
  toQuery Glean.Schema.Java.Types.Modifier_final_ = Glean.Schema.Query.Java.Types.Modifier_final_
  toQuery Glean.Schema.Java.Types.Modifier_native_ = Glean.Schema.Query.Java.Types.Modifier_native_
  toQuery Glean.Schema.Java.Types.Modifier_private_ = Glean.Schema.Query.Java.Types.Modifier_private_
  toQuery Glean.Schema.Java.Types.Modifier_protected_ = Glean.Schema.Query.Java.Types.Modifier_protected_
  toQuery Glean.Schema.Java.Types.Modifier_public_ = Glean.Schema.Query.Java.Types.Modifier_public_
  toQuery Glean.Schema.Java.Types.Modifier_static_ = Glean.Schema.Query.Java.Types.Modifier_static_
  toQuery Glean.Schema.Java.Types.Modifier_strictfp_ = Glean.Schema.Query.Java.Types.Modifier_strictfp_
  toQuery Glean.Schema.Java.Types.Modifier_synchronized_ = Glean.Schema.Query.Java.Types.Modifier_synchronized_
  toQuery Glean.Schema.Java.Types.Modifier_transient_ = Glean.Schema.Query.Java.Types.Modifier_transient_
  toQuery Glean.Schema.Java.Types.Modifier_volatile_ = Glean.Schema.Query.Java.Types.Modifier_volatile_

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.Wildcard = Glean.Schema.Java.Types.Wildcard
type instance Glean.QueryOf Glean.Schema.Java.Types.Wildcard = Glean.Schema.Query.Java.Types.Wildcard

instance Glean.ToQuery Glean.Schema.Java.Types.Wildcard where
  toQuery (Glean.Schema.Java.Types.Wildcard_extends_ x) = Data.Default.def { Glean.Schema.Query.Java.Types.wildcard_extends_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Java.Types.Wildcard_super_ x) = Data.Default.def { Glean.Schema.Query.Java.Types.wildcard_super_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Java.Types.Wildcard_unbounded x) = Data.Default.def { Glean.Schema.Query.Java.Types.wildcard_unbounded = Prelude.Just (Glean.toQuery x) }

type instance Glean.QueryResult Glean.Schema.Query.Java.Types.XRefTarget = Glean.Schema.Java.Types.XRefTarget
type instance Glean.QueryOf Glean.Schema.Java.Types.XRefTarget = Glean.Schema.Query.Java.Types.XRefTarget

instance Glean.ToQuery Glean.Schema.Java.Types.XRefTarget where
  toQuery (Glean.Schema.Java.Types.XRefTarget_class_or_interface_ x) = Data.Default.def { Glean.Schema.Query.Java.Types.xRefTarget_class_or_interface_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Java.Types.XRefTarget_ctor_ x) = Data.Default.def { Glean.Schema.Query.Java.Types.xRefTarget_ctor_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Java.Types.XRefTarget_method_ x) = Data.Default.def { Glean.Schema.Query.Java.Types.xRefTarget_method_ = Prelude.Just (Glean.toQuery x) }
