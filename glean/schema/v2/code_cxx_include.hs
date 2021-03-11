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
import qualified Glean.Schema.Cxx1.Types


instance Glean.Type Glean.Schema.CodeCxx.Types.DeclToDef_key where
  buildRtsValue b (Glean.Schema.CodeCxx.Types.DeclToDef_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.CodeCxx.Types.DeclToDef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.CodeCxx.Types.DeclToDef_key = 'Angle.TField "decl" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TField "defn" (Glean.Schema.CodeCxx.Types.Definition) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.CodeCxx.Types.DeclToDef where
  type KeyType Glean.Schema.CodeCxx.Types.DeclToDef =
    Glean.Schema.CodeCxx.Types.DeclToDef_key
  getName _proxy  = Glean.PredicateRef "code.cxx.DeclToDef"3
  getIndex _proxy  = 11
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.CodeCxx.Types.declToDef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.CodeCxx.Types.DeclToDef x k
  getFactKey = Glean.Schema.CodeCxx.Types.declToDef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.CodeCxx.Types.DeclToDef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.CodeCxx.Types.Entity where
  buildRtsValue b (Glean.Schema.CodeCxx.Types.Entity_decl x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.CodeCxx.Types.Entity_defn x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.CodeCxx.Types.Entity_enumerator x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.CodeCxx.Types.Entity_decl
    , Glean.mapD Glean.Schema.CodeCxx.Types.Entity_defn
    , Glean.mapD Glean.Schema.CodeCxx.Types.Entity_enumerator
    ]

type instance Angle.SumFields Glean.Schema.CodeCxx.Types.Entity = 'Angle.TField "decl" (Glean.Schema.Cxx1.Types.Declaration) ('Angle.TField "defn" (Glean.Schema.CodeCxx.Types.Definition) ('Angle.TField "enumerator" (Glean.KeyType Glean.Schema.Cxx1.Types.Enumerator) ('Angle.TNoFields)))

instance Glean.SumBranches Glean.Schema.Cxx1.Types.Declaration Glean.Schema.CodeCxx.Types.Entity where
  injectBranch = Glean.Schema.CodeCxx.Types.Entity_decl
  projectBranch (Glean.Schema.CodeCxx.Types.Entity_decl x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.CodeCxx.Types.Definition Glean.Schema.CodeCxx.Types.Entity where
  injectBranch = Glean.Schema.CodeCxx.Types.Entity_defn
  projectBranch (Glean.Schema.CodeCxx.Types.Entity_defn x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.Enumerator Glean.Schema.CodeCxx.Types.Entity where
  injectBranch = Glean.Schema.CodeCxx.Types.Entity_enumerator
  projectBranch (Glean.Schema.CodeCxx.Types.Entity_enumerator x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.CodeCxx.Types.Definition where
  buildRtsValue b (Glean.Schema.CodeCxx.Types.Definition_record_ x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.CodeCxx.Types.Definition_function_ x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.CodeCxx.Types.Definition_enum_ x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.CodeCxx.Types.Definition_objcMethod x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.CodeCxx.Types.Definition_objcContainer x) = do
    Glean.buildRtsSelector b 4
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.CodeCxx.Types.Definition_variable x) = do
    Glean.buildRtsSelector b 5
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.CodeCxx.Types.Definition_namespace_ x) = do
    Glean.buildRtsSelector b 6
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.CodeCxx.Types.Definition_record_
    , Glean.mapD Glean.Schema.CodeCxx.Types.Definition_function_
    , Glean.mapD Glean.Schema.CodeCxx.Types.Definition_enum_
    , Glean.mapD Glean.Schema.CodeCxx.Types.Definition_objcMethod
    , Glean.mapD Glean.Schema.CodeCxx.Types.Definition_objcContainer
    , Glean.mapD Glean.Schema.CodeCxx.Types.Definition_variable
    , Glean.mapD Glean.Schema.CodeCxx.Types.Definition_namespace_
    ]

type instance Angle.SumFields Glean.Schema.CodeCxx.Types.Definition = 'Angle.TField "record_" (Glean.KeyType Glean.Schema.Cxx1.Types.RecordDefinition) ('Angle.TField "function_" (Glean.KeyType Glean.Schema.Cxx1.Types.FunctionDefinition) ('Angle.TField "enum_" (Glean.KeyType Glean.Schema.Cxx1.Types.EnumDefinition) ('Angle.TField "objcMethod" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcMethodDefinition) ('Angle.TField "objcContainer" (Glean.KeyType Glean.Schema.Cxx1.Types.ObjcContainerDefinition) ('Angle.TField "variable" (Glean.KeyType Glean.Schema.Cxx1.Types.VariableDeclaration) ('Angle.TField "namespace_" (Glean.KeyType Glean.Schema.Cxx1.Types.NamespaceDefinition) ('Angle.TNoFields)))))))

instance Glean.SumBranches Glean.Schema.Cxx1.Types.RecordDefinition Glean.Schema.CodeCxx.Types.Definition where
  injectBranch = Glean.Schema.CodeCxx.Types.Definition_record_
  projectBranch (Glean.Schema.CodeCxx.Types.Definition_record_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.FunctionDefinition Glean.Schema.CodeCxx.Types.Definition where
  injectBranch = Glean.Schema.CodeCxx.Types.Definition_function_
  projectBranch (Glean.Schema.CodeCxx.Types.Definition_function_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.EnumDefinition Glean.Schema.CodeCxx.Types.Definition where
  injectBranch = Glean.Schema.CodeCxx.Types.Definition_enum_
  projectBranch (Glean.Schema.CodeCxx.Types.Definition_enum_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.ObjcMethodDefinition Glean.Schema.CodeCxx.Types.Definition where
  injectBranch = Glean.Schema.CodeCxx.Types.Definition_objcMethod
  projectBranch (Glean.Schema.CodeCxx.Types.Definition_objcMethod x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.ObjcContainerDefinition Glean.Schema.CodeCxx.Types.Definition where
  injectBranch = Glean.Schema.CodeCxx.Types.Definition_objcContainer
  projectBranch (Glean.Schema.CodeCxx.Types.Definition_objcContainer x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.VariableDeclaration Glean.Schema.CodeCxx.Types.Definition where
  injectBranch = Glean.Schema.CodeCxx.Types.Definition_variable
  projectBranch (Glean.Schema.CodeCxx.Types.Definition_variable x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Cxx1.Types.NamespaceDefinition Glean.Schema.CodeCxx.Types.Definition where
  injectBranch = Glean.Schema.CodeCxx.Types.Definition_namespace_
  projectBranch (Glean.Schema.CodeCxx.Types.Definition_namespace_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing
