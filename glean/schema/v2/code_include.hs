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
import qualified Glean.Schema.CodeCxx.Types
import qualified Glean.Schema.CodeFlow.Types
import qualified Glean.Schema.CodeHack.Types
import qualified Glean.Schema.CodeHs.Types
import qualified Glean.Schema.CodeJava.Types
import qualified Glean.Schema.CodePython.Types
import qualified Glean.Schema.Pp1.Types


instance Glean.Type Glean.Schema.Code.Types.Entity where
  buildRtsValue b (Glean.Schema.Code.Types.Entity_cxx x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Code.Types.Entity_pp x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Code.Types.Entity_java x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Code.Types.Entity_hs x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Code.Types.Entity_python x) = do
    Glean.buildRtsSelector b 4
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Code.Types.Entity_hack x) = do
    Glean.buildRtsSelector b 5
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Code.Types.Entity_flow x) = do
    Glean.buildRtsSelector b 6
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Code.Types.Entity_cxx
    , Glean.mapD Glean.Schema.Code.Types.Entity_pp
    , Glean.mapD Glean.Schema.Code.Types.Entity_java
    , Glean.mapD Glean.Schema.Code.Types.Entity_hs
    , Glean.mapD Glean.Schema.Code.Types.Entity_python
    , Glean.mapD Glean.Schema.Code.Types.Entity_hack
    , Glean.mapD Glean.Schema.Code.Types.Entity_flow
    ]

type instance Angle.SumFields Glean.Schema.Code.Types.Entity = 'Angle.TField "cxx" (Glean.Schema.CodeCxx.Types.Entity) ('Angle.TField "pp" (Glean.KeyType Glean.Schema.Pp1.Types.Define) ('Angle.TField "java" (Glean.Schema.CodeJava.Types.Entity) ('Angle.TField "hs" (Glean.Schema.CodeHs.Types.Entity) ('Angle.TField "python" (Glean.Schema.CodePython.Types.Entity) ('Angle.TField "hack" (Glean.Schema.CodeHack.Types.Entity) ('Angle.TField "flow" (Glean.Schema.CodeFlow.Types.Entity) ('Angle.TNoFields)))))))

instance Glean.SumBranches Glean.Schema.CodeCxx.Types.Entity Glean.Schema.Code.Types.Entity where
  injectBranch = Glean.Schema.Code.Types.Entity_cxx
  projectBranch (Glean.Schema.Code.Types.Entity_cxx x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Pp1.Types.Define Glean.Schema.Code.Types.Entity where
  injectBranch = Glean.Schema.Code.Types.Entity_pp
  projectBranch (Glean.Schema.Code.Types.Entity_pp x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.CodeJava.Types.Entity Glean.Schema.Code.Types.Entity where
  injectBranch = Glean.Schema.Code.Types.Entity_java
  projectBranch (Glean.Schema.Code.Types.Entity_java x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.CodeHs.Types.Entity Glean.Schema.Code.Types.Entity where
  injectBranch = Glean.Schema.Code.Types.Entity_hs
  projectBranch (Glean.Schema.Code.Types.Entity_hs x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.CodePython.Types.Entity Glean.Schema.Code.Types.Entity where
  injectBranch = Glean.Schema.Code.Types.Entity_python
  projectBranch (Glean.Schema.Code.Types.Entity_python x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.CodeHack.Types.Entity Glean.Schema.Code.Types.Entity where
  injectBranch = Glean.Schema.Code.Types.Entity_hack
  projectBranch (Glean.Schema.Code.Types.Entity_hack x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.CodeFlow.Types.Entity Glean.Schema.Code.Types.Entity where
  injectBranch = Glean.Schema.Code.Types.Entity_flow
  projectBranch (Glean.Schema.Code.Types.Entity_flow x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Code.Types.Entity_8 where
  buildRtsValue b (Glean.Schema.Code.Types.Entity_8_cxx x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Code.Types.Entity_8_pp x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Code.Types.Entity_8_java x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Code.Types.Entity_8_hs x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Code.Types.Entity_8_python x) = do
    Glean.buildRtsSelector b 4
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Code.Types.Entity_8_hack x) = do
    Glean.buildRtsSelector b 5
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Code.Types.Entity_8_cxx
    , Glean.mapD Glean.Schema.Code.Types.Entity_8_pp
    , Glean.mapD Glean.Schema.Code.Types.Entity_8_java
    , Glean.mapD Glean.Schema.Code.Types.Entity_8_hs
    , Glean.mapD Glean.Schema.Code.Types.Entity_8_python
    , Glean.mapD Glean.Schema.Code.Types.Entity_8_hack
    ]

type instance Angle.SumFields Glean.Schema.Code.Types.Entity_8 = 'Angle.TField "cxx" (Glean.Schema.CodeCxx.Types.Entity) ('Angle.TField "pp" (Glean.KeyType Glean.Schema.Pp1.Types.Define) ('Angle.TField "java" (Glean.Schema.CodeJava.Types.Entity_2) ('Angle.TField "hs" (Glean.Schema.CodeHs.Types.Entity) ('Angle.TField "python" (Glean.Schema.CodePython.Types.Entity) ('Angle.TField "hack" (Glean.Schema.CodeHack.Types.Entity) ('Angle.TNoFields))))))

instance Glean.SumBranches Glean.Schema.CodeCxx.Types.Entity Glean.Schema.Code.Types.Entity_8 where
  injectBranch = Glean.Schema.Code.Types.Entity_8_cxx
  projectBranch (Glean.Schema.Code.Types.Entity_8_cxx x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Pp1.Types.Define Glean.Schema.Code.Types.Entity_8 where
  injectBranch = Glean.Schema.Code.Types.Entity_8_pp
  projectBranch (Glean.Schema.Code.Types.Entity_8_pp x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.CodeJava.Types.Entity_2 Glean.Schema.Code.Types.Entity_8 where
  injectBranch = Glean.Schema.Code.Types.Entity_8_java
  projectBranch (Glean.Schema.Code.Types.Entity_8_java x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.CodeHs.Types.Entity Glean.Schema.Code.Types.Entity_8 where
  injectBranch = Glean.Schema.Code.Types.Entity_8_hs
  projectBranch (Glean.Schema.Code.Types.Entity_8_hs x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.CodePython.Types.Entity Glean.Schema.Code.Types.Entity_8 where
  injectBranch = Glean.Schema.Code.Types.Entity_8_python
  projectBranch (Glean.Schema.Code.Types.Entity_8_python x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.CodeHack.Types.Entity Glean.Schema.Code.Types.Entity_8 where
  injectBranch = Glean.Schema.Code.Types.Entity_8_hack
  projectBranch (Glean.Schema.Code.Types.Entity_8_hack x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing
