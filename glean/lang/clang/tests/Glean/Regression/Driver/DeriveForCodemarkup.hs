{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Driver.DeriveForCodemarkup (
    main,
    codemarkupDerivePasses
  ) where

import Derive.Lib as Lib
import Glean.Clang.Test.DerivePass

--
-- Derive things we will need for codemarkup.* calls
--
main :: IO ()
main = testDeriver codemarkupDerivePasses

--
-- Standard things we need to derive (also Glass) for full code search/nav
--
codemarkupDerivePasses :: [DerivePass]
codemarkupDerivePasses =
  Lib.allManualPasses ++
  [DeriveGeneric "cxx1.FunctionDeclAttribute"
  ,DeriveGeneric "cxx1.DeclarationToUSR"
  ,DeriveGeneric "cxx1.RecordDerived"
  ,DeriveGeneric "cxx1.ThriftToCxx"
  -- name search
  ,DeriveGeneric "cxx1.EnumDeclarationByName"
  ,DeriveGeneric "cxx1.EnumeratorByName"
  ,DeriveGeneric "cxx1.FunctionDeclarationByNameScope"
  ,DeriveGeneric "cxx1.NamespaceDeclarationByName"
  ,DeriveGeneric "cxx1.ObjcContainerDeclarationInterface"
  ,DeriveGeneric "cxx1.RecordDeclarationClass"
  ,DeriveGeneric "cxx1.RecordDeclarationStruct"
  ,DeriveGeneric "cxx1.RecordDeclarationUnion"
  ,DeriveGeneric "cxx1.TypeAliasDeclarationByName"
  ,DeriveGeneric "cxx1.VariableDeclarationNonLocalByName"
  -- lowercase name search
  ,DeriveGeneric "pp1.DefineLowerCase"
  ,DeriveGeneric "cxx1.EnumLowerCase"
  ,DeriveGeneric "cxx1.EnumeratorLowerCase"
  ,DeriveGeneric "cxx1.FunctionLowerCase"
  ,DeriveGeneric "cxx1.NamespaceLowerCase"
  ,DeriveGeneric "cxx1.ObjcContainerInterfaceLowerCase"
  ,DeriveGeneric "cxx1.RecordClassLowerCase"
  ,DeriveGeneric "cxx1.RecordStructLowerCase"
  ,DeriveGeneric "cxx1.RecordUnionLowerCase"
  ,DeriveGeneric "cxx1.TypeAliasLowerCase"
  ,DeriveGeneric "cxx1.VariableLowerCase"
  -- objc
  ,DeriveGeneric "cxx1.ObjcContainerInheritance"
  ,DeriveGeneric "cxx1.ObjcInterfaceToImplementation"
  ]
