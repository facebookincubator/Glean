/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using System;

namespace Glean.Indexer.Schema
{
    public enum Predicate
    {
        Arbitrary,
        ArrayType,
        Class,
        DefinitionLocation,
        Field,
        File,
        FileLines,
        FullName,
        FunctionPointerType,
        Implements,
        Interface,
        Local,
        Method,
        MethodInvocationLocation,
        Name,
        Namespace,
        ObjectCreationLocation,
        Parameter,
        PointerType,
        Property,
        Record,
        Struct,
        TypeParameter,
        UnityPackage,
        UnityProjectSource,
        MSBuildProjectSource,
        Project,
        ProjectToSourceFile,
        Solution,
        SolutionToProject,
    }

    static class PredicateExtensions
    {
        public static (string Name, int Version) GetSchema(this Predicate predicate)
        {
            switch (predicate)
            {
                case Predicate.File:
                    return ("src", 1);
                case Predicate.Arbitrary:
                    return ("arbitrary", 0);
                default:
                    return ("csharp", 1);
            }
        }

        public static string GetFullName(this Predicate predicate)
        {
            var schema = predicate.GetSchema();
            return $"{schema.Name}.{predicate}.{schema.Version}";
        }
    }
}
