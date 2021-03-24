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


instance Glean.Type Glean.Schema.Graphql.Types.VariableDef_key where
  buildRtsValue b (Glean.Schema.Graphql.Types.VariableDef_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Graphql.Types.VariableDef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Graphql.Types.VariableDef_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "type" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "directives" ([Glean.KeyType Glean.Schema.Graphql.Types.Directive]) ('Angle.TField "defaultValue" (Prelude.Maybe (Glean.KeyType Glean.Schema.Graphql.Types.Value)) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Graphql.Types.VariableDef where
  type KeyType Glean.Schema.Graphql.Types.VariableDef =
    Glean.Schema.Graphql.Types.VariableDef_key
  getName _proxy  = Glean.PredicateRef "graphql.VariableDef"1
  getIndex _proxy  = 458
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Graphql.Types.variableDef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Graphql.Types.VariableDef x k
  getFactKey = Glean.Schema.Graphql.Types.variableDef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Graphql.Types.VariableDef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Graphql.Types.ScalarTypeDef_key where
  buildRtsValue b (Glean.Schema.Graphql.Types.ScalarTypeDef_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Graphql.Types.ScalarTypeDef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Graphql.Types.ScalarTypeDef_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "directives" ([Glean.KeyType Glean.Schema.Graphql.Types.Directive]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Graphql.Types.ScalarTypeDef where
  type KeyType Glean.Schema.Graphql.Types.ScalarTypeDef =
    Glean.Schema.Graphql.Types.ScalarTypeDef_key
  getName _proxy  = Glean.PredicateRef "graphql.ScalarTypeDef"1
  getIndex _proxy  = 449
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Graphql.Types.scalarTypeDef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Graphql.Types.ScalarTypeDef x k
  getFactKey = Glean.Schema.Graphql.Types.scalarTypeDef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Graphql.Types.ScalarTypeDef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Graphql.Types.InterfaceTypeDef_key where
  buildRtsValue b (Glean.Schema.Graphql.Types.InterfaceTypeDef_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Graphql.Types.InterfaceTypeDef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Graphql.Types.InterfaceTypeDef_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "fields" ([Glean.KeyType Glean.Schema.Graphql.Types.FieldDef]) ('Angle.TField "directives" ([Glean.KeyType Glean.Schema.Graphql.Types.Directive]) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Graphql.Types.InterfaceTypeDef where
  type KeyType Glean.Schema.Graphql.Types.InterfaceTypeDef =
    Glean.Schema.Graphql.Types.InterfaceTypeDef_key
  getName _proxy  = Glean.PredicateRef "graphql.InterfaceTypeDef"1
  getIndex _proxy  = 389
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Graphql.Types.interfaceTypeDef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Graphql.Types.InterfaceTypeDef x k
  getFactKey = Glean.Schema.Graphql.Types.interfaceTypeDef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Graphql.Types.InterfaceTypeDef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Graphql.Types.InputValueDef_key where
  buildRtsValue b (Glean.Schema.Graphql.Types.InputValueDef_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Graphql.Types.InputValueDef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Graphql.Types.InputValueDef_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "type" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "directives" ([Glean.KeyType Glean.Schema.Graphql.Types.Directive]) ('Angle.TField "defaultValue" (Prelude.Maybe (Glean.KeyType Glean.Schema.Graphql.Types.Value)) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Graphql.Types.InputValueDef where
  type KeyType Glean.Schema.Graphql.Types.InputValueDef =
    Glean.Schema.Graphql.Types.InputValueDef_key
  getName _proxy  = Glean.PredicateRef "graphql.InputValueDef"1
  getIndex _proxy  = 385
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Graphql.Types.inputValueDef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Graphql.Types.InputValueDef x k
  getFactKey = Glean.Schema.Graphql.Types.inputValueDef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Graphql.Types.InputValueDef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Graphql.Types.Query_key where
  buildRtsValue b (Glean.Schema.Graphql.Types.Query_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Graphql.Types.Query_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Graphql.Types.Query_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "directives" ([Glean.KeyType Glean.Schema.Graphql.Types.Directive]) ('Angle.TField "variableDefs" ([Glean.KeyType Glean.Schema.Graphql.Types.VariableDef]) ('Angle.TField "selectionSet" (Glean.Schema.Graphql.Types.SelectionSet) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Graphql.Types.Query where
  type KeyType Glean.Schema.Graphql.Types.Query =
    Glean.Schema.Graphql.Types.Query_key
  getName _proxy  = Glean.PredicateRef "graphql.Query"1
  getIndex _proxy  = 384
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Graphql.Types.query_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Graphql.Types.Query x k
  getFactKey = Glean.Schema.Graphql.Types.query_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Graphql.Types.Query where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Graphql.Types.DirectiveDef_key where
  buildRtsValue b (Glean.Schema.Graphql.Types.DirectiveDef_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Graphql.Types.DirectiveDef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Graphql.Types.DirectiveDef_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "argumentDefs" ([Glean.KeyType Glean.Schema.Graphql.Types.InputValueDef]) ('Angle.TField "locations" ([Glean.Schema.Graphql.Types.DirectiveDefLocation]) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Graphql.Types.DirectiveDef where
  type KeyType Glean.Schema.Graphql.Types.DirectiveDef =
    Glean.Schema.Graphql.Types.DirectiveDef_key
  getName _proxy  = Glean.PredicateRef "graphql.DirectiveDef"1
  getIndex _proxy  = 328
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Graphql.Types.directiveDef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Graphql.Types.DirectiveDef x k
  getFactKey = Glean.Schema.Graphql.Types.directiveDef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Graphql.Types.DirectiveDef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Graphql.Types.Fragment_key where
  buildRtsValue b (Glean.Schema.Graphql.Types.Fragment_key x1 x2 x3 x4 x5) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
  decodeRtsValue = Glean.Schema.Graphql.Types.Fragment_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Graphql.Types.Fragment_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "typeCondition" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "variableDefs" ([Glean.KeyType Glean.Schema.Graphql.Types.VariableDef]) ('Angle.TField "directives" ([Glean.KeyType Glean.Schema.Graphql.Types.Directive]) ('Angle.TField "selectionSet" (Glean.Schema.Graphql.Types.SelectionSet) ('Angle.TNoFields)))))

instance Glean.Predicate Glean.Schema.Graphql.Types.Fragment where
  type KeyType Glean.Schema.Graphql.Types.Fragment =
    Glean.Schema.Graphql.Types.Fragment_key
  getName _proxy  = Glean.PredicateRef "graphql.Fragment"1
  getIndex _proxy  = 307
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Graphql.Types.fragment_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Graphql.Types.Fragment x k
  getFactKey = Glean.Schema.Graphql.Types.fragment_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Graphql.Types.Fragment where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Graphql.Types.Argument_key where
  buildRtsValue b (Glean.Schema.Graphql.Types.Argument_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Graphql.Types.Argument_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Graphql.Types.Argument_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "value" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Graphql.Types.Argument where
  type KeyType Glean.Schema.Graphql.Types.Argument =
    Glean.Schema.Graphql.Types.Argument_key
  getName _proxy  = Glean.PredicateRef "graphql.Argument"1
  getIndex _proxy  = 271
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Graphql.Types.argument_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Graphql.Types.Argument x k
  getFactKey = Glean.Schema.Graphql.Types.argument_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Graphql.Types.Argument where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Graphql.Types.InlineFragment_key where
  buildRtsValue b (Glean.Schema.Graphql.Types.InlineFragment_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Graphql.Types.InlineFragment_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Graphql.Types.InlineFragment_key = 'Angle.TField "inferredTypeCondition" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "directives" ([Glean.KeyType Glean.Schema.Graphql.Types.Directive]) ('Angle.TField "selectionSet" (Glean.Schema.Graphql.Types.SelectionSet) ('Angle.TField "typeCondition" (Prelude.Maybe (Glean.KeyType Glean.Schema.Graphql.Types.Value)) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Graphql.Types.InlineFragment where
  type KeyType Glean.Schema.Graphql.Types.InlineFragment =
    Glean.Schema.Graphql.Types.InlineFragment_key
  getName _proxy  = Glean.PredicateRef "graphql.InlineFragment"1
  getIndex _proxy  = 239
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Graphql.Types.inlineFragment_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Graphql.Types.InlineFragment x k
  getFactKey = Glean.Schema.Graphql.Types.inlineFragment_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Graphql.Types.InlineFragment where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Graphql.Types.EnumTypeDef_key where
  buildRtsValue b (Glean.Schema.Graphql.Types.EnumTypeDef_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Graphql.Types.EnumTypeDef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Graphql.Types.EnumTypeDef_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "values" ([Glean.KeyType Glean.Schema.Graphql.Types.Value]) ('Angle.TField "directives" ([Glean.KeyType Glean.Schema.Graphql.Types.Directive]) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Graphql.Types.EnumTypeDef where
  type KeyType Glean.Schema.Graphql.Types.EnumTypeDef =
    Glean.Schema.Graphql.Types.EnumTypeDef_key
  getName _proxy  = Glean.PredicateRef "graphql.EnumTypeDef"1
  getIndex _proxy  = 210
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Graphql.Types.enumTypeDef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Graphql.Types.EnumTypeDef x k
  getFactKey = Glean.Schema.Graphql.Types.enumTypeDef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Graphql.Types.EnumTypeDef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Graphql.Types.UnionTypeDef_key where
  buildRtsValue b (Glean.Schema.Graphql.Types.UnionTypeDef_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Graphql.Types.UnionTypeDef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Graphql.Types.UnionTypeDef_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "types" ([Glean.KeyType Glean.Schema.Graphql.Types.Value]) ('Angle.TField "directives" ([Glean.KeyType Glean.Schema.Graphql.Types.Directive]) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Graphql.Types.UnionTypeDef where
  type KeyType Glean.Schema.Graphql.Types.UnionTypeDef =
    Glean.Schema.Graphql.Types.UnionTypeDef_key
  getName _proxy  = Glean.PredicateRef "graphql.UnionTypeDef"1
  getIndex _proxy  = 200
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Graphql.Types.unionTypeDef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Graphql.Types.UnionTypeDef x k
  getFactKey = Glean.Schema.Graphql.Types.unionTypeDef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Graphql.Types.UnionTypeDef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Graphql.Types.InputObjectTypeDef_key where
  buildRtsValue b (Glean.Schema.Graphql.Types.InputObjectTypeDef_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Graphql.Types.InputObjectTypeDef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Graphql.Types.InputObjectTypeDef_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "fields" ([Glean.KeyType Glean.Schema.Graphql.Types.InputValueDef]) ('Angle.TField "directives" ([Glean.KeyType Glean.Schema.Graphql.Types.Directive]) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Graphql.Types.InputObjectTypeDef where
  type KeyType Glean.Schema.Graphql.Types.InputObjectTypeDef =
    Glean.Schema.Graphql.Types.InputObjectTypeDef_key
  getName _proxy  = Glean.PredicateRef "graphql.InputObjectTypeDef"1
  getIndex _proxy  = 197
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Graphql.Types.inputObjectTypeDef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Graphql.Types.InputObjectTypeDef x k
  getFactKey = Glean.Schema.Graphql.Types.inputObjectTypeDef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Graphql.Types.InputObjectTypeDef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Graphql.Types.Directive_key where
  buildRtsValue b (Glean.Schema.Graphql.Types.Directive_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Graphql.Types.Directive_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Graphql.Types.Directive_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "arguments" ([Glean.KeyType Glean.Schema.Graphql.Types.Argument]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Graphql.Types.Directive where
  type KeyType Glean.Schema.Graphql.Types.Directive =
    Glean.Schema.Graphql.Types.Directive_key
  getName _proxy  = Glean.PredicateRef "graphql.Directive"1
  getIndex _proxy  = 153
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Graphql.Types.directive_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Graphql.Types.Directive x k
  getFactKey = Glean.Schema.Graphql.Types.directive_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Graphql.Types.Directive where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Graphql.Types.ObjectTypeDef_key where
  buildRtsValue b (Glean.Schema.Graphql.Types.ObjectTypeDef_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Graphql.Types.ObjectTypeDef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Graphql.Types.ObjectTypeDef_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "interfaces" ([Glean.KeyType Glean.Schema.Graphql.Types.Value]) ('Angle.TField "fields" ([Glean.KeyType Glean.Schema.Graphql.Types.FieldDef]) ('Angle.TField "directives" ([Glean.KeyType Glean.Schema.Graphql.Types.Directive]) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Graphql.Types.ObjectTypeDef where
  type KeyType Glean.Schema.Graphql.Types.ObjectTypeDef =
    Glean.Schema.Graphql.Types.ObjectTypeDef_key
  getName _proxy  = Glean.PredicateRef "graphql.ObjectTypeDef"1
  getIndex _proxy  = 144
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Graphql.Types.objectTypeDef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Graphql.Types.ObjectTypeDef x k
  getFactKey = Glean.Schema.Graphql.Types.objectTypeDef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Graphql.Types.ObjectTypeDef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Graphql.Types.Field_key where
  buildRtsValue b (Glean.Schema.Graphql.Types.Field_key x1 x2 x3 x4 x5 x6) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
    Glean.buildRtsValue b x5
    Glean.buildRtsValue b x6
  decodeRtsValue = Glean.Schema.Graphql.Types.Field_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Graphql.Types.Field_key = 'Angle.TField "type" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "name" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "directives" ([Glean.KeyType Glean.Schema.Graphql.Types.Directive]) ('Angle.TField "selectionSet" (Glean.Schema.Graphql.Types.SelectionSet) ('Angle.TField "arguments" ([Glean.KeyType Glean.Schema.Graphql.Types.Argument]) ('Angle.TField "alias" (Prelude.Maybe (Glean.KeyType Glean.Schema.Graphql.Types.Value)) ('Angle.TNoFields))))))

instance Glean.Predicate Glean.Schema.Graphql.Types.Field where
  type KeyType Glean.Schema.Graphql.Types.Field =
    Glean.Schema.Graphql.Types.Field_key
  getName _proxy  = Glean.PredicateRef "graphql.Field"1
  getIndex _proxy  = 130
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Graphql.Types.field_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Graphql.Types.Field x k
  getFactKey = Glean.Schema.Graphql.Types.field_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Graphql.Types.Field where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Graphql.Types.FieldDef_key where
  buildRtsValue b (Glean.Schema.Graphql.Types.FieldDef_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Graphql.Types.FieldDef_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Graphql.Types.FieldDef_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "type" (Glean.KeyType Glean.Schema.Graphql.Types.Value) ('Angle.TField "argumentDefs" ([Glean.KeyType Glean.Schema.Graphql.Types.InputValueDef]) ('Angle.TField "directives" ([Glean.KeyType Glean.Schema.Graphql.Types.Directive]) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Graphql.Types.FieldDef where
  type KeyType Glean.Schema.Graphql.Types.FieldDef =
    Glean.Schema.Graphql.Types.FieldDef_key
  getName _proxy  = Glean.PredicateRef "graphql.FieldDef"1
  getIndex _proxy  = 102
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Graphql.Types.fieldDef_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Graphql.Types.FieldDef x k
  getFactKey = Glean.Schema.Graphql.Types.fieldDef_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Graphql.Types.FieldDef where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Graphql.Types.Value where
  type KeyType Glean.Schema.Graphql.Types.Value = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "graphql.Value"1
  getIndex _proxy  = 42
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Graphql.Types.value_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Graphql.Types.Value x k
  getFactKey = Glean.Schema.Graphql.Types.value_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Graphql.Types.Value where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Graphql.Types.DirectiveDefLocation where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Graphql.Types.DirectiveDefLocation = 'Angle.TField "QUERY" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "MUTATION" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "SUBSCRIPTION" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "FIELD" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "FRAGMENT_DEFINITION" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "FRAGMENT_SPREAD" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "INLINE_FRAGMENT" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "SCHEMA" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "SCALAR" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "OBJECT" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "FIELD_DEFINITION" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "ARGUMENT_DEFINITION" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "INTERFACE" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "UNION" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "ENUM" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "ENUM_VALUE" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "INPUT_OBJECT" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "INPUT_FIELD_DEFINITION" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields))))))))))))))))))

instance Glean.Type Glean.Schema.Graphql.Types.SelectionSet where
  buildRtsValue b (Glean.Schema.Graphql.Types.SelectionSet x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Graphql.Types.SelectionSet
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Graphql.Types.SelectionSet = 'Angle.TField "fields" ([Glean.KeyType Glean.Schema.Graphql.Types.Field]) ('Angle.TField "inlineFragments" ([Glean.KeyType Glean.Schema.Graphql.Types.InlineFragment]) ('Angle.TField "fragmentSpreads" ([Glean.KeyType Glean.Schema.Graphql.Types.Value]) ('Angle.TNoFields)))
