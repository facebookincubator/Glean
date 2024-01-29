// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

using System;

namespace Indexer.Schema
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
