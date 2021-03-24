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
import qualified Glean.Schema.Src.Types


instance Glean.Type Glean.Schema.Java.Types.XRef_key where
  buildRtsValue b (Glean.Schema.Java.Types.XRef_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Java.Types.XRef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.XRef_key = 'Angle.TField "target" (Glean.Schema.Java.Types.XRefTarget) ('Angle.TField "ranges" ([Glean.Schema.Src.Types.ByteSpan]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Java.Types.XRef where
  type KeyType Glean.Schema.Java.Types.XRef = Glean.Schema.Java.Types.XRef_key
  getName _proxy  = Glean.PredicateRef "java.XRef"3
  getIndex _proxy  = 434
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.xRef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.XRef x k
  getFactKey = Glean.Schema.Java.Types.xRef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.XRef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.TypeParam_key where
  buildRtsValue b (Glean.Schema.Java.Types.TypeParam_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Java.Types.TypeParam_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.TypeParam_key = 'Angle.TField "name" (Data.Text.Text) ('Angle.TField "extends_" ([Glean.KeyType Glean.Schema.Java.Types.Type]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Java.Types.TypeParam where
  type KeyType Glean.Schema.Java.Types.TypeParam =
    Glean.Schema.Java.Types.TypeParam_key
  getName _proxy  = Glean.PredicateRef "java.TypeParam"3
  getIndex _proxy  = 390
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.typeParam_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.TypeParam x k
  getFactKey = Glean.Schema.Java.Types.typeParam_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.TypeParam where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.Annotation_key where
  buildRtsValue b (Glean.Schema.Java.Types.Annotation_key x1) = do
    Glean.buildRtsValue b x1
  decodeRtsValue = Glean.Schema.Java.Types.Annotation_key
    <$> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.Annotation_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Java.Types.QName) ('Angle.TNoFields)

instance Glean.Predicate Glean.Schema.Java.Types.Annotation where
  type KeyType Glean.Schema.Java.Types.Annotation =
    Glean.Schema.Java.Types.Annotation_key
  getName _proxy  = Glean.PredicateRef "java.Annotation"4
  getIndex _proxy  = 387
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.annotation_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.Annotation x k
  getFactKey = Glean.Schema.Java.Types.annotation_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.Annotation where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.MethodDeclaration_key where
  buildRtsValue b (Glean.Schema.Java.Types.MethodDeclaration_key x1 x2 x3 x4 x5 x6 x7) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
    Glean.buildRtsValue b x7
  decodeRtsValue = Glean.Schema.Java.Types.MethodDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.MethodDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Java.Types.QName) ('Angle.TField "parameters" ([Glean.KeyType Glean.Schema.Java.Types.VariableDeclaration]) ('Angle.TField "returnType" (Glean.KeyType Glean.Schema.Java.Types.Type) ('Angle.TField "annotations" ([Glean.KeyType Glean.Schema.Java.Types.Annotation]) ('Angle.TField "modifiers" ([Glean.Schema.Java.Types.Modifier]) ('Angle.TField "typeParams" ([Glean.KeyType Glean.Schema.Java.Types.TypeParam]) ('Angle.TField "loc" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields)))))))

instance Glean.Predicate Glean.Schema.Java.Types.MethodDeclaration where
  type KeyType Glean.Schema.Java.Types.MethodDeclaration =
    Glean.Schema.Java.Types.MethodDeclaration_key
  getName _proxy  = Glean.PredicateRef "java.MethodDeclaration"4
  getIndex _proxy  = 383
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.methodDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.MethodDeclaration x k
  getFactKey = Glean.Schema.Java.Types.methodDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.MethodDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.MethodDeclaration_3_key where
  buildRtsValue b (Glean.Schema.Java.Types.MethodDeclaration_3_key x1 x2 x3 x4 x5 x6) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
  decodeRtsValue = Glean.Schema.Java.Types.MethodDeclaration_3_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.MethodDeclaration_3_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Java.Types.QName) ('Angle.TField "parameters" ([Glean.KeyType Glean.Schema.Java.Types.VariableDeclaration_3]) ('Angle.TField "returnType" (Glean.KeyType Glean.Schema.Java.Types.Type) ('Angle.TField "modifiers" ([Glean.Schema.Java.Types.Modifier]) ('Angle.TField "typeParams" ([Glean.KeyType Glean.Schema.Java.Types.TypeParam]) ('Angle.TField "loc" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields))))))

instance Glean.Predicate Glean.Schema.Java.Types.MethodDeclaration_3 where
  type KeyType Glean.Schema.Java.Types.MethodDeclaration_3 =
    Glean.Schema.Java.Types.MethodDeclaration_3_key
  getName _proxy  = Glean.PredicateRef "java.MethodDeclaration"3
  getIndex _proxy  = 382
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.methodDeclaration_3_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.MethodDeclaration_3 x k
  getFactKey = Glean.Schema.Java.Types.methodDeclaration_3_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.MethodDeclaration_3 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.MethodDeclaration_2_key where
  buildRtsValue b (Glean.Schema.Java.Types.MethodDeclaration_2_key x1 x2 x3 x4 x5) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
  decodeRtsValue = Glean.Schema.Java.Types.MethodDeclaration_2_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.MethodDeclaration_2_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Java.Types.Name) ('Angle.TField "parameters" ([Glean.KeyType Glean.Schema.Java.Types.VariableDeclaration_2]) ('Angle.TField "returnType" (Glean.KeyType Glean.Schema.Java.Types.Type_2) ('Angle.TField "modifiers" ([Glean.Schema.Java.Types.Modifier]) ('Angle.TField "loc" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields)))))

instance Glean.Predicate Glean.Schema.Java.Types.MethodDeclaration_2 where
  type KeyType Glean.Schema.Java.Types.MethodDeclaration_2 =
    Glean.Schema.Java.Types.MethodDeclaration_2_key
  getName _proxy  = Glean.PredicateRef "java.MethodDeclaration"2
  getIndex _proxy  = 381
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.methodDeclaration_2_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.MethodDeclaration_2 x k
  getFactKey = Glean.Schema.Java.Types.methodDeclaration_2_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.MethodDeclaration_2 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.AnnotatedClass_key where
  buildRtsValue b (Glean.Schema.Java.Types.AnnotatedClass_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Java.Types.AnnotatedClass_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.AnnotatedClass_key = 'Angle.TField "annotation" (Glean.KeyType Glean.Schema.Java.Types.Annotation) ('Angle.TField "class_" (Glean.KeyType Glean.Schema.Java.Types.ClassDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Java.Types.AnnotatedClass where
  type KeyType Glean.Schema.Java.Types.AnnotatedClass =
    Glean.Schema.Java.Types.AnnotatedClass_key
  getName _proxy  = Glean.PredicateRef "java.AnnotatedClass"4
  getIndex _proxy  = 355
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.annotatedClass_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.AnnotatedClass x k
  getFactKey = Glean.Schema.Java.Types.annotatedClass_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.AnnotatedClass where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.InterfaceDeclaration_key where
  buildRtsValue b (Glean.Schema.Java.Types.InterfaceDeclaration_key x1 x2 x3 x4 x5 x6 x7) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
    Glean.buildRtsValue b x7
  decodeRtsValue = Glean.Schema.Java.Types.InterfaceDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.InterfaceDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Java.Types.QName) ('Angle.TField "annotations" ([Glean.KeyType Glean.Schema.Java.Types.Annotation]) ('Angle.TField "modifiers" ([Glean.Schema.Java.Types.Modifier]) ('Angle.TField "extends_" ([Glean.KeyType Glean.Schema.Java.Types.Type]) ('Angle.TField "methods" ([Glean.KeyType Glean.Schema.Java.Types.MethodDeclaration]) ('Angle.TField "typeParams" ([Glean.KeyType Glean.Schema.Java.Types.TypeParam]) ('Angle.TField "loc" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields)))))))

instance Glean.Predicate Glean.Schema.Java.Types.InterfaceDeclaration where
  type KeyType Glean.Schema.Java.Types.InterfaceDeclaration =
    Glean.Schema.Java.Types.InterfaceDeclaration_key
  getName _proxy  = Glean.PredicateRef "java.InterfaceDeclaration"4
  getIndex _proxy  = 314
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.interfaceDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.InterfaceDeclaration x k
  getFactKey = Glean.Schema.Java.Types.interfaceDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.InterfaceDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.InterfaceDeclaration_3_key where
  buildRtsValue b (Glean.Schema.Java.Types.InterfaceDeclaration_3_key x1 x2 x3 x4 x5 x6) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
  decodeRtsValue = Glean.Schema.Java.Types.InterfaceDeclaration_3_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.InterfaceDeclaration_3_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Java.Types.QName) ('Angle.TField "modifiers" ([Glean.Schema.Java.Types.Modifier]) ('Angle.TField "extends_" ([Glean.KeyType Glean.Schema.Java.Types.Type]) ('Angle.TField "methods" ([Glean.KeyType Glean.Schema.Java.Types.MethodDeclaration_3]) ('Angle.TField "typeParams" ([Glean.KeyType Glean.Schema.Java.Types.TypeParam]) ('Angle.TField "loc" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields))))))

instance Glean.Predicate Glean.Schema.Java.Types.InterfaceDeclaration_3 where
  type KeyType Glean.Schema.Java.Types.InterfaceDeclaration_3 =
    Glean.Schema.Java.Types.InterfaceDeclaration_3_key
  getName _proxy  = Glean.PredicateRef "java.InterfaceDeclaration"3
  getIndex _proxy  = 313
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.interfaceDeclaration_3_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.InterfaceDeclaration_3 x k
  getFactKey = Glean.Schema.Java.Types.interfaceDeclaration_3_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.InterfaceDeclaration_3 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.InterfaceDeclaration_2_key where
  buildRtsValue b (Glean.Schema.Java.Types.InterfaceDeclaration_2_key x1 x2 x3 x4 x5) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
  decodeRtsValue = Glean.Schema.Java.Types.InterfaceDeclaration_2_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.InterfaceDeclaration_2_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Java.Types.Name) ('Angle.TField "modifiers" ([Glean.Schema.Java.Types.Modifier]) ('Angle.TField "extends_" ([Glean.KeyType Glean.Schema.Java.Types.Type_2]) ('Angle.TField "methods" ([Glean.KeyType Glean.Schema.Java.Types.MethodDeclaration_2]) ('Angle.TField "loc" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields)))))

instance Glean.Predicate Glean.Schema.Java.Types.InterfaceDeclaration_2 where
  type KeyType Glean.Schema.Java.Types.InterfaceDeclaration_2 =
    Glean.Schema.Java.Types.InterfaceDeclaration_2_key
  getName _proxy  = Glean.PredicateRef "java.InterfaceDeclaration"2
  getIndex _proxy  = 312
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.interfaceDeclaration_2_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.InterfaceDeclaration_2 x k
  getFactKey = Glean.Schema.Java.Types.interfaceDeclaration_2_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.InterfaceDeclaration_2 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.ClassDeclaration_key where
  buildRtsValue b (Glean.Schema.Java.Types.ClassDeclaration_key x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
    Glean.buildRtsValue b x7
    Glean.buildRtsValue b x8
    Glean.buildRtsValue b x9
    Glean.buildRtsValue b x10
  decodeRtsValue = Glean.Schema.Java.Types.ClassDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.ClassDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Java.Types.QName) ('Angle.TField "modifiers" ([Glean.Schema.Java.Types.Modifier]) ('Angle.TField "extends_" (Prelude.Maybe (Glean.KeyType Glean.Schema.Java.Types.Type)) ('Angle.TField "implements_" ([Glean.KeyType Glean.Schema.Java.Types.Type]) ('Angle.TField "annotations" ([Glean.KeyType Glean.Schema.Java.Types.Annotation]) ('Angle.TField "variables" ([Glean.KeyType Glean.Schema.Java.Types.VariableDeclaration]) ('Angle.TField "constructors" ([Glean.KeyType Glean.Schema.Java.Types.ConstructorDeclaration]) ('Angle.TField "methods" ([Glean.KeyType Glean.Schema.Java.Types.MethodDeclaration]) ('Angle.TField "typeParams" ([Glean.KeyType Glean.Schema.Java.Types.TypeParam]) ('Angle.TField "loc" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields))))))))))

instance Glean.Predicate Glean.Schema.Java.Types.ClassDeclaration where
  type KeyType Glean.Schema.Java.Types.ClassDeclaration =
    Glean.Schema.Java.Types.ClassDeclaration_key
  getName _proxy  = Glean.PredicateRef "java.ClassDeclaration"4
  getIndex _proxy  = 279
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.classDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.ClassDeclaration x k
  getFactKey = Glean.Schema.Java.Types.classDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.ClassDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.ClassDeclaration_3_key where
  buildRtsValue b (Glean.Schema.Java.Types.ClassDeclaration_3_key x1 x2 x3 x4 x5 x6 x7 x8 x9) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
    Glean.buildRtsValue b x7
    Glean.buildRtsValue b x8
    Glean.buildRtsValue b x9
  decodeRtsValue = Glean.Schema.Java.Types.ClassDeclaration_3_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.ClassDeclaration_3_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Java.Types.QName) ('Angle.TField "modifiers" ([Glean.Schema.Java.Types.Modifier]) ('Angle.TField "extends_" (Prelude.Maybe (Glean.KeyType Glean.Schema.Java.Types.Type)) ('Angle.TField "implements_" ([Glean.KeyType Glean.Schema.Java.Types.Type]) ('Angle.TField "variables" ([Glean.KeyType Glean.Schema.Java.Types.VariableDeclaration_3]) ('Angle.TField "constructors" ([Glean.KeyType Glean.Schema.Java.Types.ConstructorDeclaration_3]) ('Angle.TField "methods" ([Glean.KeyType Glean.Schema.Java.Types.MethodDeclaration_3]) ('Angle.TField "typeParams" ([Glean.KeyType Glean.Schema.Java.Types.TypeParam]) ('Angle.TField "loc" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields)))))))))

instance Glean.Predicate Glean.Schema.Java.Types.ClassDeclaration_3 where
  type KeyType Glean.Schema.Java.Types.ClassDeclaration_3 =
    Glean.Schema.Java.Types.ClassDeclaration_3_key
  getName _proxy  = Glean.PredicateRef "java.ClassDeclaration"3
  getIndex _proxy  = 278
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.classDeclaration_3_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.ClassDeclaration_3 x k
  getFactKey = Glean.Schema.Java.Types.classDeclaration_3_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.ClassDeclaration_3 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.ClassDeclaration_2_key where
  buildRtsValue b (Glean.Schema.Java.Types.ClassDeclaration_2_key x1 x2 x3 x4 x5 x6 x7 x8) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
    Glean.buildRtsValue b x7
    Glean.buildRtsValue b x8
  decodeRtsValue = Glean.Schema.Java.Types.ClassDeclaration_2_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.ClassDeclaration_2_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Java.Types.Name) ('Angle.TField "modifiers" ([Glean.Schema.Java.Types.Modifier]) ('Angle.TField "extends_" (Prelude.Maybe (Glean.KeyType Glean.Schema.Java.Types.Type_2)) ('Angle.TField "implements_" ([Glean.KeyType Glean.Schema.Java.Types.Type_2]) ('Angle.TField "variables" ([Glean.KeyType Glean.Schema.Java.Types.VariableDeclaration_2]) ('Angle.TField "constructors" ([Glean.KeyType Glean.Schema.Java.Types.ConstructorDeclaration_2]) ('Angle.TField "methods" ([Glean.KeyType Glean.Schema.Java.Types.MethodDeclaration_2]) ('Angle.TField "loc" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields))))))))

instance Glean.Predicate Glean.Schema.Java.Types.ClassDeclaration_2 where
  type KeyType Glean.Schema.Java.Types.ClassDeclaration_2 =
    Glean.Schema.Java.Types.ClassDeclaration_2_key
  getName _proxy  = Glean.PredicateRef "java.ClassDeclaration"2
  getIndex _proxy  = 277
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.classDeclaration_2_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.ClassDeclaration_2 x k
  getFactKey = Glean.Schema.Java.Types.classDeclaration_2_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.ClassDeclaration_2 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.FileXRefs_key where
  buildRtsValue b (Glean.Schema.Java.Types.FileXRefs_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Java.Types.FileXRefs_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.FileXRefs_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "xrefs" ([Glean.KeyType Glean.Schema.Java.Types.XRef]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Java.Types.FileXRefs where
  type KeyType Glean.Schema.Java.Types.FileXRefs =
    Glean.Schema.Java.Types.FileXRefs_key
  getName _proxy  = Glean.PredicateRef "java.FileXRefs"3
  getIndex _proxy  = 246
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.fileXRefs_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.FileXRefs x k
  getFactKey = Glean.Schema.Java.Types.fileXRefs_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.FileXRefs where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.TypeArg_key where
  buildRtsValue b (Glean.Schema.Java.Types.TypeArg_key_type x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Java.Types.TypeArg_key_wildcard x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Java.Types.TypeArg_key_type
    , Glean.mapD Glean.Schema.Java.Types.TypeArg_key_wildcard
    ]

type instance Angle.SumFields Glean.Schema.Java.Types.TypeArg_key = 'Angle.TField "type" (Glean.KeyType Glean.Schema.Java.Types.Type) ('Angle.TField "wildcard" (Glean.Schema.Java.Types.Wildcard) ('Angle.TNoFields))

instance Glean.SumBranches Glean.Schema.Java.Types.Type Glean.Schema.Java.Types.TypeArg_key where
  injectBranch = Glean.Schema.Java.Types.TypeArg_key_type
  projectBranch (Glean.Schema.Java.Types.TypeArg_key_type x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Java.Types.Wildcard Glean.Schema.Java.Types.TypeArg_key where
  injectBranch = Glean.Schema.Java.Types.TypeArg_key_wildcard
  projectBranch (Glean.Schema.Java.Types.TypeArg_key_wildcard x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Predicate Glean.Schema.Java.Types.TypeArg where
  type KeyType Glean.Schema.Java.Types.TypeArg =
    Glean.Schema.Java.Types.TypeArg_key
  getName _proxy  = Glean.PredicateRef "java.TypeArg"3
  getIndex _proxy  = 237
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.typeArg_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.TypeArg x k
  getFactKey = Glean.Schema.Java.Types.typeArg_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.TypeArg where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.Inheritance_key where
  buildRtsValue b (Glean.Schema.Java.Types.Inheritance_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Java.Types.Inheritance_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.Inheritance_key = 'Angle.TField "base" (Glean.KeyType Glean.Schema.Java.Types.Type) ('Angle.TField "subclass" (Glean.KeyType Glean.Schema.Java.Types.ClassDeclaration) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Java.Types.Inheritance where
  type KeyType Glean.Schema.Java.Types.Inheritance =
    Glean.Schema.Java.Types.Inheritance_key
  getName _proxy  = Glean.PredicateRef "java.Inheritance"4
  getIndex _proxy  = 223
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.inheritance_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.Inheritance x k
  getFactKey = Glean.Schema.Java.Types.inheritance_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.Inheritance where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.Inheritance_3_key where
  buildRtsValue b (Glean.Schema.Java.Types.Inheritance_3_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Java.Types.Inheritance_3_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.Inheritance_3_key = 'Angle.TField "base" (Glean.KeyType Glean.Schema.Java.Types.Type) ('Angle.TField "subclass" (Glean.KeyType Glean.Schema.Java.Types.ClassDeclaration_3) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Java.Types.Inheritance_3 where
  type KeyType Glean.Schema.Java.Types.Inheritance_3 =
    Glean.Schema.Java.Types.Inheritance_3_key
  getName _proxy  = Glean.PredicateRef "java.Inheritance"3
  getIndex _proxy  = 222
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.inheritance_3_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.Inheritance_3 x k
  getFactKey = Glean.Schema.Java.Types.inheritance_3_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.Inheritance_3 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Java.Types.Name where
  type KeyType Glean.Schema.Java.Types.Name = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "java.Name"2
  getIndex _proxy  = 132
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.name_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.Name x k
  getFactKey = Glean.Schema.Java.Types.name_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.Name where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.ConstructorDeclaration_key where
  buildRtsValue b (Glean.Schema.Java.Types.ConstructorDeclaration_key x1 x2 x3 x4 x5 x6) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
  decodeRtsValue = Glean.Schema.Java.Types.ConstructorDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.ConstructorDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Java.Types.QName) ('Angle.TField "parameters" ([Glean.KeyType Glean.Schema.Java.Types.VariableDeclaration]) ('Angle.TField "annotations" ([Glean.KeyType Glean.Schema.Java.Types.Annotation]) ('Angle.TField "modifiers" ([Glean.Schema.Java.Types.Modifier]) ('Angle.TField "typeParams" ([Glean.KeyType Glean.Schema.Java.Types.TypeParam]) ('Angle.TField "loc" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields))))))

instance Glean.Predicate Glean.Schema.Java.Types.ConstructorDeclaration where
  type KeyType Glean.Schema.Java.Types.ConstructorDeclaration =
    Glean.Schema.Java.Types.ConstructorDeclaration_key
  getName _proxy  = Glean.PredicateRef "java.ConstructorDeclaration"4
  getIndex _proxy  = 119
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.constructorDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.ConstructorDeclaration x k
  getFactKey = Glean.Schema.Java.Types.constructorDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.ConstructorDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.ConstructorDeclaration_3_key where
  buildRtsValue b (Glean.Schema.Java.Types.ConstructorDeclaration_3_key x1 x2 x3 x4 x5) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
  decodeRtsValue = Glean.Schema.Java.Types.ConstructorDeclaration_3_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.ConstructorDeclaration_3_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Java.Types.QName) ('Angle.TField "parameters" ([Glean.KeyType Glean.Schema.Java.Types.VariableDeclaration_3]) ('Angle.TField "modifiers" ([Glean.Schema.Java.Types.Modifier]) ('Angle.TField "typeParams" ([Glean.KeyType Glean.Schema.Java.Types.TypeParam]) ('Angle.TField "loc" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields)))))

instance Glean.Predicate Glean.Schema.Java.Types.ConstructorDeclaration_3 where
  type KeyType Glean.Schema.Java.Types.ConstructorDeclaration_3 =
    Glean.Schema.Java.Types.ConstructorDeclaration_3_key
  getName _proxy  = Glean.PredicateRef "java.ConstructorDeclaration"3
  getIndex _proxy  = 118
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.constructorDeclaration_3_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.ConstructorDeclaration_3 x k
  getFactKey = Glean.Schema.Java.Types.constructorDeclaration_3_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.ConstructorDeclaration_3 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.ConstructorDeclaration_2_key where
  buildRtsValue b (Glean.Schema.Java.Types.ConstructorDeclaration_2_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Java.Types.ConstructorDeclaration_2_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.ConstructorDeclaration_2_key = 'Angle.TField "parameters" ([Glean.KeyType Glean.Schema.Java.Types.VariableDeclaration_2]) ('Angle.TField "modifiers" ([Glean.Schema.Java.Types.Modifier]) ('Angle.TField "loc" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Java.Types.ConstructorDeclaration_2 where
  type KeyType Glean.Schema.Java.Types.ConstructorDeclaration_2 =
    Glean.Schema.Java.Types.ConstructorDeclaration_2_key
  getName _proxy  = Glean.PredicateRef "java.ConstructorDeclaration"2
  getIndex _proxy  = 117
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.constructorDeclaration_2_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.ConstructorDeclaration_2 x k
  getFactKey = Glean.Schema.Java.Types.constructorDeclaration_2_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.ConstructorDeclaration_2 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.Type_key where
  buildRtsValue b (Glean.Schema.Java.Types.Type_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Java.Types.Type_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.Type_key = 'Angle.TField "type" (Data.Text.Text) ('Angle.TField "typeArgs" ([Glean.KeyType Glean.Schema.Java.Types.TypeArg]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Java.Types.Type where
  type KeyType Glean.Schema.Java.Types.Type = Glean.Schema.Java.Types.Type_key
  getName _proxy  = Glean.PredicateRef "java.Type"3
  getIndex _proxy  = 83
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.type_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.Type x k
  getFactKey = Glean.Schema.Java.Types.type_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.Type where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.Type_2_key where
  buildRtsValue b (Glean.Schema.Java.Types.Type_2_key x1) = do
    Glean.buildRtsValue b x1
  decodeRtsValue = Glean.Schema.Java.Types.Type_2_key
    <$> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.Type_2_key = 'Angle.TField "type" (Glean.KeyType Glean.Schema.Java.Types.Name) ('Angle.TNoFields)

instance Glean.Predicate Glean.Schema.Java.Types.Type_2 where
  type KeyType Glean.Schema.Java.Types.Type_2 =
    Glean.Schema.Java.Types.Type_2_key
  getName _proxy  = Glean.PredicateRef "java.Type"2
  getIndex _proxy  = 82
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.type_2_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.Type_2 x k
  getFactKey = Glean.Schema.Java.Types.type_2_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.Type_2 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.VariableDeclaration_key where
  buildRtsValue b (Glean.Schema.Java.Types.VariableDeclaration_key x1 x2 x3 x4 x5) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
  decodeRtsValue = Glean.Schema.Java.Types.VariableDeclaration_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.VariableDeclaration_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Java.Types.QName) ('Angle.TField "type" (Glean.KeyType Glean.Schema.Java.Types.Type) ('Angle.TField "annotations" ([Glean.KeyType Glean.Schema.Java.Types.Annotation]) ('Angle.TField "modifiers" ([Glean.Schema.Java.Types.Modifier]) ('Angle.TField "loc" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields)))))

instance Glean.Predicate Glean.Schema.Java.Types.VariableDeclaration where
  type KeyType Glean.Schema.Java.Types.VariableDeclaration =
    Glean.Schema.Java.Types.VariableDeclaration_key
  getName _proxy  = Glean.PredicateRef "java.VariableDeclaration"4
  getIndex _proxy  = 26
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.variableDeclaration_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.VariableDeclaration x k
  getFactKey = Glean.Schema.Java.Types.variableDeclaration_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.VariableDeclaration where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.VariableDeclaration_3_key where
  buildRtsValue b (Glean.Schema.Java.Types.VariableDeclaration_3_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Java.Types.VariableDeclaration_3_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.VariableDeclaration_3_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Java.Types.QName) ('Angle.TField "type" (Glean.KeyType Glean.Schema.Java.Types.Type) ('Angle.TField "modifiers" ([Glean.Schema.Java.Types.Modifier]) ('Angle.TField "loc" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Java.Types.VariableDeclaration_3 where
  type KeyType Glean.Schema.Java.Types.VariableDeclaration_3 =
    Glean.Schema.Java.Types.VariableDeclaration_3_key
  getName _proxy  = Glean.PredicateRef "java.VariableDeclaration"3
  getIndex _proxy  = 25
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.variableDeclaration_3_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.VariableDeclaration_3 x k
  getFactKey = Glean.Schema.Java.Types.variableDeclaration_3_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.VariableDeclaration_3 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.VariableDeclaration_2_key where
  buildRtsValue b (Glean.Schema.Java.Types.VariableDeclaration_2_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Java.Types.VariableDeclaration_2_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.VariableDeclaration_2_key = 'Angle.TField "type" (Glean.KeyType Glean.Schema.Java.Types.Type_2) ('Angle.TField "name" (Glean.KeyType Glean.Schema.Java.Types.Name) ('Angle.TField "modifiers" ([Glean.Schema.Java.Types.Modifier]) ('Angle.TField "loc" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Java.Types.VariableDeclaration_2 where
  type KeyType Glean.Schema.Java.Types.VariableDeclaration_2 =
    Glean.Schema.Java.Types.VariableDeclaration_2_key
  getName _proxy  = Glean.PredicateRef "java.VariableDeclaration"2
  getIndex _proxy  = 24
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.variableDeclaration_2_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.VariableDeclaration_2 x k
  getFactKey = Glean.Schema.Java.Types.variableDeclaration_2_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.VariableDeclaration_2 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.QName_key where
  buildRtsValue b (Glean.Schema.Java.Types.QName_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Java.Types.QName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Java.Types.QName_key = 'Angle.TField "name" (Data.Text.Text) ('Angle.TField "fqn" (Prelude.Maybe Data.Text.Text) ('Angle.TField "signature" (Prelude.Maybe Data.Text.Text) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Java.Types.QName where
  type KeyType Glean.Schema.Java.Types.QName =
    Glean.Schema.Java.Types.QName_key
  getName _proxy  = Glean.PredicateRef "java.QName"3
  getIndex _proxy  = 3
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Java.Types.qName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Java.Types.QName x k
  getFactKey = Glean.Schema.Java.Types.qName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Java.Types.QName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Java.Types.Modifier where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Java.Types.Modifier = 'Angle.TField "abstract_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "default_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "final_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "native_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "private_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "protected_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "public_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "static_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "strictfp_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "synchronized_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "transient_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "volatile_" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields))))))))))))

instance Glean.Type Glean.Schema.Java.Types.Wildcard where
  buildRtsValue b (Glean.Schema.Java.Types.Wildcard_extends_ x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Java.Types.Wildcard_super_ x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Java.Types.Wildcard_unbounded x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Java.Types.Wildcard_extends_
    , Glean.mapD Glean.Schema.Java.Types.Wildcard_super_
    , Glean.mapD Glean.Schema.Java.Types.Wildcard_unbounded
    ]

type instance Angle.SumFields Glean.Schema.Java.Types.Wildcard = 'Angle.TField "extends_" (Glean.KeyType Glean.Schema.Java.Types.Type) ('Angle.TField "super_" (Glean.KeyType Glean.Schema.Java.Types.Type) ('Angle.TField "unbounded" (Prelude.Bool) ('Angle.TNoFields)))

instance Glean.Type Glean.Schema.Java.Types.XRefTarget where
  buildRtsValue b (Glean.Schema.Java.Types.XRefTarget_class_or_interface_ x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Java.Types.XRefTarget_ctor_ x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Java.Types.XRefTarget_method_ x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Java.Types.XRefTarget_class_or_interface_
    , Glean.mapD Glean.Schema.Java.Types.XRefTarget_ctor_
    , Glean.mapD Glean.Schema.Java.Types.XRefTarget_method_
    ]

type instance Angle.SumFields Glean.Schema.Java.Types.XRefTarget = 'Angle.TField "class_or_interface_" (Glean.KeyType Glean.Schema.Java.Types.QName) ('Angle.TField "ctor_" (Glean.KeyType Glean.Schema.Java.Types.QName) ('Angle.TField "method_" (Glean.KeyType Glean.Schema.Java.Types.QName) ('Angle.TNoFields)))
