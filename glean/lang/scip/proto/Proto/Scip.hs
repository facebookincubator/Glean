{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{- This file was auto-generated from scip.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Scip (
        Descriptor(), Descriptor'Suffix(..),
        Descriptor'Suffix(Descriptor'Package),
        Descriptor'Suffix'UnrecognizedValue, Diagnostic(),
        DiagnosticTag(..), DiagnosticTag(),
        DiagnosticTag'UnrecognizedValue, Document(), Index(), Language(..),
        Language(), Language'UnrecognizedValue, Metadata(), Occurrence(),
        Package(), PositionEncoding(..), PositionEncoding(),
        PositionEncoding'UnrecognizedValue, ProtocolVersion(..),
        ProtocolVersion(), ProtocolVersion'UnrecognizedValue,
        Relationship(), Severity(..), Severity(),
        Severity'UnrecognizedValue, Symbol(), SymbolInformation(),
        SymbolInformation'Kind(..), SymbolInformation'Kind(),
        SymbolInformation'Kind'UnrecognizedValue, SymbolRole(..),
        SymbolRole(), SymbolRole'UnrecognizedValue, SyntaxKind(..),
        SyntaxKind(IdentifierKeyword, IdentifierModule),
        SyntaxKind'UnrecognizedValue, TextEncoding(..), TextEncoding(),
        TextEncoding'UnrecognizedValue, ToolInfo()
    ) where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Prism as Data.ProtoLens.Prism
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens as Data.ProtoLens
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes as Data.ProtoLens.Encoding.Bytes
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing as Data.ProtoLens.Encoding.Growing
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2 as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8 as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read
{- | Fields :

         * 'Proto.Scip_Fields.name' @:: Lens' Descriptor Data.Text.Text@
         * 'Proto.Scip_Fields.disambiguator' @:: Lens' Descriptor Data.Text.Text@
         * 'Proto.Scip_Fields.suffix' @:: Lens' Descriptor Descriptor'Suffix@ -}
data Descriptor
  = Descriptor'_constructor {_Descriptor'name :: !Data.Text.Text,
                             _Descriptor'disambiguator :: !Data.Text.Text,
                             _Descriptor'suffix :: !Descriptor'Suffix,
                             _Descriptor'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Descriptor where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Descriptor "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Descriptor'name (\ x__ y__ -> x__ {_Descriptor'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Descriptor "disambiguator" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Descriptor'disambiguator
           (\ x__ y__ -> x__ {_Descriptor'disambiguator = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Descriptor "suffix" Descriptor'Suffix where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Descriptor'suffix (\ x__ y__ -> x__ {_Descriptor'suffix = y__}))
        Prelude.id
instance Data.ProtoLens.Message Descriptor where
  messageName _ = Data.Text.pack "scip.Descriptor"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \Descriptor\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2$\n\
      \\rdisambiguator\CAN\STX \SOH(\tR\rdisambiguator\DC2/\n\
      \\ACKsuffix\CAN\ETX \SOH(\SO2\ETB.scip.Descriptor.SuffixR\ACKsuffix\"\165\SOH\n\
      \\ACKSuffix\DC2\NAK\n\
      \\DC1UnspecifiedSuffix\DLE\NUL\DC2\r\n\
      \\tNamespace\DLE\SOH\DC2\SI\n\
      \\aPackage\DLE\SOH\SUB\STX\b\SOH\DC2\b\n\
      \\EOTType\DLE\STX\DC2\b\n\
      \\EOTTerm\DLE\ETX\DC2\n\
      \\n\
      \\ACKMethod\DLE\EOT\DC2\DC1\n\
      \\rTypeParameter\DLE\ENQ\DC2\r\n\
      \\tParameter\DLE\ACK\DC2\b\n\
      \\EOTMeta\DLE\a\DC2\t\n\
      \\ENQLocal\DLE\b\DC2\t\n\
      \\ENQMacro\DLE\t\SUB\STX\DLE\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor Descriptor
        disambiguator__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "disambiguator"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"disambiguator")) ::
              Data.ProtoLens.FieldDescriptor Descriptor
        suffix__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "suffix"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor Descriptor'Suffix)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"suffix")) ::
              Data.ProtoLens.FieldDescriptor Descriptor
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, name__field_descriptor),
           (Data.ProtoLens.Tag 2, disambiguator__field_descriptor),
           (Data.ProtoLens.Tag 3, suffix__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Descriptor'_unknownFields
        (\ x__ y__ -> x__ {_Descriptor'_unknownFields = y__})
  defMessage
    = Descriptor'_constructor
        {_Descriptor'name = Data.ProtoLens.fieldDefault,
         _Descriptor'disambiguator = Data.ProtoLens.fieldDefault,
         _Descriptor'suffix = Data.ProtoLens.fieldDefault,
         _Descriptor'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Descriptor -> Data.ProtoLens.Encoding.Bytes.Parser Descriptor
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "disambiguator"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"disambiguator") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "suffix"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"suffix") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Descriptor"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"disambiguator") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8 _v))
                ((Data.Monoid.<>)
                   (let
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"suffix") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               ((Prelude..)
                                  Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                               Prelude.fromEnum _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData Descriptor where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Descriptor'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Descriptor'name x__)
                (Control.DeepSeq.deepseq
                   (_Descriptor'disambiguator x__)
                   (Control.DeepSeq.deepseq (_Descriptor'suffix x__) ())))
newtype Descriptor'Suffix'UnrecognizedValue
  = Descriptor'Suffix'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data Descriptor'Suffix
  = Descriptor'UnspecifiedSuffix |
    Descriptor'Namespace |
    Descriptor'Type |
    Descriptor'Term |
    Descriptor'Method |
    Descriptor'TypeParameter |
    Descriptor'Parameter |
    Descriptor'Meta |
    Descriptor'Local |
    Descriptor'Macro |
    Descriptor'Suffix'Unrecognized !Descriptor'Suffix'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum Descriptor'Suffix where
  maybeToEnum 0 = Prelude.Just Descriptor'UnspecifiedSuffix
  maybeToEnum 1 = Prelude.Just Descriptor'Namespace
  maybeToEnum 2 = Prelude.Just Descriptor'Type
  maybeToEnum 3 = Prelude.Just Descriptor'Term
  maybeToEnum 4 = Prelude.Just Descriptor'Method
  maybeToEnum 5 = Prelude.Just Descriptor'TypeParameter
  maybeToEnum 6 = Prelude.Just Descriptor'Parameter
  maybeToEnum 7 = Prelude.Just Descriptor'Meta
  maybeToEnum 8 = Prelude.Just Descriptor'Local
  maybeToEnum 9 = Prelude.Just Descriptor'Macro
  maybeToEnum k
    = Prelude.Just
        (Descriptor'Suffix'Unrecognized
           (Descriptor'Suffix'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum Descriptor'UnspecifiedSuffix = "UnspecifiedSuffix"
  showEnum Descriptor'Namespace = "Namespace"
  showEnum Descriptor'Type = "Type"
  showEnum Descriptor'Term = "Term"
  showEnum Descriptor'Method = "Method"
  showEnum Descriptor'TypeParameter = "TypeParameter"
  showEnum Descriptor'Parameter = "Parameter"
  showEnum Descriptor'Meta = "Meta"
  showEnum Descriptor'Local = "Local"
  showEnum Descriptor'Macro = "Macro"
  showEnum
    (Descriptor'Suffix'Unrecognized (Descriptor'Suffix'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "UnspecifiedSuffix"
    = Prelude.Just Descriptor'UnspecifiedSuffix
    | (Prelude.==) k "Namespace" = Prelude.Just Descriptor'Namespace
    | (Prelude.==) k "Package" = Prelude.Just Descriptor'Package
    | (Prelude.==) k "Type" = Prelude.Just Descriptor'Type
    | (Prelude.==) k "Term" = Prelude.Just Descriptor'Term
    | (Prelude.==) k "Method" = Prelude.Just Descriptor'Method
    | (Prelude.==) k "TypeParameter"
    = Prelude.Just Descriptor'TypeParameter
    | (Prelude.==) k "Parameter" = Prelude.Just Descriptor'Parameter
    | (Prelude.==) k "Meta" = Prelude.Just Descriptor'Meta
    | (Prelude.==) k "Local" = Prelude.Just Descriptor'Local
    | (Prelude.==) k "Macro" = Prelude.Just Descriptor'Macro
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded Descriptor'Suffix where
  minBound = Descriptor'UnspecifiedSuffix
  maxBound = Descriptor'Macro
instance Prelude.Enum Descriptor'Suffix where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum Suffix: " (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum Descriptor'UnspecifiedSuffix = 0
  fromEnum Descriptor'Namespace = 1
  fromEnum Descriptor'Type = 2
  fromEnum Descriptor'Term = 3
  fromEnum Descriptor'Method = 4
  fromEnum Descriptor'TypeParameter = 5
  fromEnum Descriptor'Parameter = 6
  fromEnum Descriptor'Meta = 7
  fromEnum Descriptor'Local = 8
  fromEnum Descriptor'Macro = 9
  fromEnum
    (Descriptor'Suffix'Unrecognized (Descriptor'Suffix'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ Descriptor'Macro
    = Prelude.error
        "Descriptor'Suffix.succ: bad argument Descriptor'Macro. This value would be out of bounds."
  succ Descriptor'UnspecifiedSuffix = Descriptor'Namespace
  succ Descriptor'Namespace = Descriptor'Type
  succ Descriptor'Type = Descriptor'Term
  succ Descriptor'Term = Descriptor'Method
  succ Descriptor'Method = Descriptor'TypeParameter
  succ Descriptor'TypeParameter = Descriptor'Parameter
  succ Descriptor'Parameter = Descriptor'Meta
  succ Descriptor'Meta = Descriptor'Local
  succ Descriptor'Local = Descriptor'Macro
  succ (Descriptor'Suffix'Unrecognized _)
    = Prelude.error
        "Descriptor'Suffix.succ: bad argument: unrecognized value"
  pred Descriptor'UnspecifiedSuffix
    = Prelude.error
        "Descriptor'Suffix.pred: bad argument Descriptor'UnspecifiedSuffix. This value would be out of bounds."
  pred Descriptor'Namespace = Descriptor'UnspecifiedSuffix
  pred Descriptor'Type = Descriptor'Namespace
  pred Descriptor'Term = Descriptor'Type
  pred Descriptor'Method = Descriptor'Term
  pred Descriptor'TypeParameter = Descriptor'Method
  pred Descriptor'Parameter = Descriptor'TypeParameter
  pred Descriptor'Meta = Descriptor'Parameter
  pred Descriptor'Local = Descriptor'Meta
  pred Descriptor'Macro = Descriptor'Local
  pred (Descriptor'Suffix'Unrecognized _)
    = Prelude.error
        "Descriptor'Suffix.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault Descriptor'Suffix where
  fieldDefault = Descriptor'UnspecifiedSuffix
instance Control.DeepSeq.NFData Descriptor'Suffix where
  rnf x__ = Prelude.seq x__ ()
pattern Descriptor'Package :: Descriptor'Suffix
pattern Descriptor'Package = Descriptor'Namespace
{- | Fields :

         * 'Proto.Scip_Fields.severity' @:: Lens' Diagnostic Severity@
         * 'Proto.Scip_Fields.code' @:: Lens' Diagnostic Data.Text.Text@
         * 'Proto.Scip_Fields.message' @:: Lens' Diagnostic Data.Text.Text@
         * 'Proto.Scip_Fields.source' @:: Lens' Diagnostic Data.Text.Text@
         * 'Proto.Scip_Fields.tags' @:: Lens' Diagnostic [DiagnosticTag]@
         * 'Proto.Scip_Fields.vec'tags' @:: Lens' Diagnostic (Data.Vector.Vector DiagnosticTag)@ -}
data Diagnostic
  = Diagnostic'_constructor {_Diagnostic'severity :: !Severity,
                             _Diagnostic'code :: !Data.Text.Text,
                             _Diagnostic'message :: !Data.Text.Text,
                             _Diagnostic'source :: !Data.Text.Text,
                             _Diagnostic'tags :: !(Data.Vector.Vector DiagnosticTag),
                             _Diagnostic'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Diagnostic where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Diagnostic "severity" Severity where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Diagnostic'severity
           (\ x__ y__ -> x__ {_Diagnostic'severity = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Diagnostic "code" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Diagnostic'code (\ x__ y__ -> x__ {_Diagnostic'code = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Diagnostic "message" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Diagnostic'message (\ x__ y__ -> x__ {_Diagnostic'message = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Diagnostic "source" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Diagnostic'source (\ x__ y__ -> x__ {_Diagnostic'source = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Diagnostic "tags" [DiagnosticTag] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Diagnostic'tags (\ x__ y__ -> x__ {_Diagnostic'tags = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Diagnostic "vec'tags" (Data.Vector.Vector DiagnosticTag) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Diagnostic'tags (\ x__ y__ -> x__ {_Diagnostic'tags = y__}))
        Prelude.id
instance Data.ProtoLens.Message Diagnostic where
  messageName _ = Data.Text.pack "scip.Diagnostic"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \Diagnostic\DC2*\n\
      \\bseverity\CAN\SOH \SOH(\SO2\SO.scip.SeverityR\bseverity\DC2\DC2\n\
      \\EOTcode\CAN\STX \SOH(\tR\EOTcode\DC2\CAN\n\
      \\amessage\CAN\ETX \SOH(\tR\amessage\DC2\SYN\n\
      \\ACKsource\CAN\EOT \SOH(\tR\ACKsource\DC2'\n\
      \\EOTtags\CAN\ENQ \ETX(\SO2\DC3.scip.DiagnosticTagR\EOTtags"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        severity__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "severity"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor Severity)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"severity")) ::
              Data.ProtoLens.FieldDescriptor Diagnostic
        code__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "code"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"code")) ::
              Data.ProtoLens.FieldDescriptor Diagnostic
        message__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "message"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"message")) ::
              Data.ProtoLens.FieldDescriptor Diagnostic
        source__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"source")) ::
              Data.ProtoLens.FieldDescriptor Diagnostic
        tags__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "tags"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor DiagnosticTag)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed (Data.ProtoLens.Field.field @"tags")) ::
              Data.ProtoLens.FieldDescriptor Diagnostic
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, severity__field_descriptor),
           (Data.ProtoLens.Tag 2, code__field_descriptor),
           (Data.ProtoLens.Tag 3, message__field_descriptor),
           (Data.ProtoLens.Tag 4, source__field_descriptor),
           (Data.ProtoLens.Tag 5, tags__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Diagnostic'_unknownFields
        (\ x__ y__ -> x__ {_Diagnostic'_unknownFields = y__})
  defMessage
    = Diagnostic'_constructor
        {_Diagnostic'severity = Data.ProtoLens.fieldDefault,
         _Diagnostic'code = Data.ProtoLens.fieldDefault,
         _Diagnostic'message = Data.ProtoLens.fieldDefault,
         _Diagnostic'source = Data.ProtoLens.fieldDefault,
         _Diagnostic'tags = Data.Vector.Generic.empty,
         _Diagnostic'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Diagnostic
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld DiagnosticTag
             -> Data.ProtoLens.Encoding.Bytes.Parser Diagnostic
        loop x mutable'tags
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'tags <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'tags)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'tags") frozen'tags x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "severity"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"severity") y x)
                                  mutable'tags
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "code"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"code") y x)
                                  mutable'tags
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "message"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"message") y x)
                                  mutable'tags
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "source"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"source") y x)
                                  mutable'tags
                        40
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Prelude.toEnum
                                           (Prelude.fmap
                                              Prelude.fromIntegral
                                              Data.ProtoLens.Encoding.Bytes.getVarInt))
                                        "tags"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'tags y)
                                loop x v
                        42
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Prelude.toEnum
                                                                       (Prelude.fmap
                                                                          Prelude.fromIntegral
                                                                          Data.ProtoLens.Encoding.Bytes.getVarInt))
                                                                    "tags"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'tags)
                                loop x y
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'tags
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'tags <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'tags)
          "Diagnostic"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"severity") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                         Prelude.fromEnum _v))
             ((Data.Monoid.<>)
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"code") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8 _v))
                ((Data.Monoid.<>)
                   (let
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"message") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8 _v))
                   ((Data.Monoid.<>)
                      (let
                         _v = Lens.Family2.view (Data.ProtoLens.Field.field @"source") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                               ((Prelude..)
                                  (\ bs
                                     -> (Data.Monoid.<>)
                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                             (Prelude.fromIntegral (Data.ByteString.length bs)))
                                          (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                  Data.Text.Encoding.encodeUtf8 _v))
                      ((Data.Monoid.<>)
                         (let
                            p = Lens.Family2.view (Data.ProtoLens.Field.field @"vec'tags") _x
                          in
                            if Data.Vector.Generic.null p then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                                  ((\ bs
                                      -> (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                                           (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                     (Data.ProtoLens.Encoding.Bytes.runBuilder
                                        (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                           ((Prelude..)
                                              ((Prelude..)
                                                 Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 Prelude.fromIntegral)
                                              Prelude.fromEnum)
                                           p))))
                         (Data.ProtoLens.Encoding.Wire.buildFieldSet
                            (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))
instance Control.DeepSeq.NFData Diagnostic where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Diagnostic'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Diagnostic'severity x__)
                (Control.DeepSeq.deepseq
                   (_Diagnostic'code x__)
                   (Control.DeepSeq.deepseq
                      (_Diagnostic'message x__)
                      (Control.DeepSeq.deepseq
                         (_Diagnostic'source x__)
                         (Control.DeepSeq.deepseq (_Diagnostic'tags x__) ())))))
newtype DiagnosticTag'UnrecognizedValue
  = DiagnosticTag'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data DiagnosticTag
  = UnspecifiedDiagnosticTag |
    Unnecessary |
    Deprecated |
    DiagnosticTag'Unrecognized !DiagnosticTag'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum DiagnosticTag where
  maybeToEnum 0 = Prelude.Just UnspecifiedDiagnosticTag
  maybeToEnum 1 = Prelude.Just Unnecessary
  maybeToEnum 2 = Prelude.Just Deprecated
  maybeToEnum k
    = Prelude.Just
        (DiagnosticTag'Unrecognized
           (DiagnosticTag'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum UnspecifiedDiagnosticTag = "UnspecifiedDiagnosticTag"
  showEnum Unnecessary = "Unnecessary"
  showEnum Deprecated = "Deprecated"
  showEnum
    (DiagnosticTag'Unrecognized (DiagnosticTag'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "UnspecifiedDiagnosticTag"
    = Prelude.Just UnspecifiedDiagnosticTag
    | (Prelude.==) k "Unnecessary" = Prelude.Just Unnecessary
    | (Prelude.==) k "Deprecated" = Prelude.Just Deprecated
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded DiagnosticTag where
  minBound = UnspecifiedDiagnosticTag
  maxBound = Deprecated
instance Prelude.Enum DiagnosticTag where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum DiagnosticTag: "
              (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum UnspecifiedDiagnosticTag = 0
  fromEnum Unnecessary = 1
  fromEnum Deprecated = 2
  fromEnum
    (DiagnosticTag'Unrecognized (DiagnosticTag'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ Deprecated
    = Prelude.error
        "DiagnosticTag.succ: bad argument Deprecated. This value would be out of bounds."
  succ UnspecifiedDiagnosticTag = Unnecessary
  succ Unnecessary = Deprecated
  succ (DiagnosticTag'Unrecognized _)
    = Prelude.error
        "DiagnosticTag.succ: bad argument: unrecognized value"
  pred UnspecifiedDiagnosticTag
    = Prelude.error
        "DiagnosticTag.pred: bad argument UnspecifiedDiagnosticTag. This value would be out of bounds."
  pred Unnecessary = UnspecifiedDiagnosticTag
  pred Deprecated = Unnecessary
  pred (DiagnosticTag'Unrecognized _)
    = Prelude.error
        "DiagnosticTag.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault DiagnosticTag where
  fieldDefault = UnspecifiedDiagnosticTag
instance Control.DeepSeq.NFData DiagnosticTag where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :

         * 'Proto.Scip_Fields.language' @:: Lens' Document Data.Text.Text@
         * 'Proto.Scip_Fields.relativePath' @:: Lens' Document Data.Text.Text@
         * 'Proto.Scip_Fields.occurrences' @:: Lens' Document [Occurrence]@
         * 'Proto.Scip_Fields.vec'occurrences' @:: Lens' Document (Data.Vector.Vector Occurrence)@
         * 'Proto.Scip_Fields.symbols' @:: Lens' Document [SymbolInformation]@
         * 'Proto.Scip_Fields.vec'symbols' @:: Lens' Document (Data.Vector.Vector SymbolInformation)@
         * 'Proto.Scip_Fields.text' @:: Lens' Document Data.Text.Text@
         * 'Proto.Scip_Fields.positionEncoding' @:: Lens' Document PositionEncoding@ -}
data Document
  = Document'_constructor {_Document'language :: !Data.Text.Text,
                           _Document'relativePath :: !Data.Text.Text,
                           _Document'occurrences :: !(Data.Vector.Vector Occurrence),
                           _Document'symbols :: !(Data.Vector.Vector SymbolInformation),
                           _Document'text :: !Data.Text.Text,
                           _Document'positionEncoding :: !PositionEncoding,
                           _Document'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Document where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Document "language" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Document'language (\ x__ y__ -> x__ {_Document'language = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Document "relativePath" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Document'relativePath
           (\ x__ y__ -> x__ {_Document'relativePath = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Document "occurrences" [Occurrence] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Document'occurrences
           (\ x__ y__ -> x__ {_Document'occurrences = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Document "vec'occurrences" (Data.Vector.Vector Occurrence) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Document'occurrences
           (\ x__ y__ -> x__ {_Document'occurrences = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Document "symbols" [SymbolInformation] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Document'symbols (\ x__ y__ -> x__ {_Document'symbols = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Document "vec'symbols" (Data.Vector.Vector SymbolInformation) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Document'symbols (\ x__ y__ -> x__ {_Document'symbols = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Document "text" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Document'text (\ x__ y__ -> x__ {_Document'text = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Document "positionEncoding" PositionEncoding where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Document'positionEncoding
           (\ x__ y__ -> x__ {_Document'positionEncoding = y__}))
        Prelude.id
instance Data.ProtoLens.Message Document where
  messageName _ = Data.Text.pack "scip.Document"
  packedMessageDescriptor _
    = "\n\
      \\bDocument\DC2\SUB\n\
      \\blanguage\CAN\EOT \SOH(\tR\blanguage\DC2#\n\
      \\rrelative_path\CAN\SOH \SOH(\tR\frelativePath\DC22\n\
      \\voccurrences\CAN\STX \ETX(\v2\DLE.scip.OccurrenceR\voccurrences\DC21\n\
      \\asymbols\CAN\ETX \ETX(\v2\ETB.scip.SymbolInformationR\asymbols\DC2\DC2\n\
      \\EOTtext\CAN\ENQ \SOH(\tR\EOTtext\DC2C\n\
      \\DC1position_encoding\CAN\ACK \SOH(\SO2\SYN.scip.PositionEncodingR\DLEpositionEncoding"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        language__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "language"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"language")) ::
              Data.ProtoLens.FieldDescriptor Document
        relativePath__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "relative_path"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"relativePath")) ::
              Data.ProtoLens.FieldDescriptor Document
        occurrences__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "occurrences"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Occurrence)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"occurrences")) ::
              Data.ProtoLens.FieldDescriptor Document
        symbols__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "symbols"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SymbolInformation)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"symbols")) ::
              Data.ProtoLens.FieldDescriptor Document
        text__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "text"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"text")) ::
              Data.ProtoLens.FieldDescriptor Document
        positionEncoding__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "position_encoding"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor PositionEncoding)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"positionEncoding")) ::
              Data.ProtoLens.FieldDescriptor Document
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 4, language__field_descriptor),
           (Data.ProtoLens.Tag 1, relativePath__field_descriptor),
           (Data.ProtoLens.Tag 2, occurrences__field_descriptor),
           (Data.ProtoLens.Tag 3, symbols__field_descriptor),
           (Data.ProtoLens.Tag 5, text__field_descriptor),
           (Data.ProtoLens.Tag 6, positionEncoding__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Document'_unknownFields
        (\ x__ y__ -> x__ {_Document'_unknownFields = y__})
  defMessage
    = Document'_constructor
        {_Document'language = Data.ProtoLens.fieldDefault,
         _Document'relativePath = Data.ProtoLens.fieldDefault,
         _Document'occurrences = Data.Vector.Generic.empty,
         _Document'symbols = Data.Vector.Generic.empty,
         _Document'text = Data.ProtoLens.fieldDefault,
         _Document'positionEncoding = Data.ProtoLens.fieldDefault,
         _Document'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Document
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Occurrence
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld SymbolInformation
                -> Data.ProtoLens.Encoding.Bytes.Parser Document
        loop x mutable'occurrences mutable'symbols
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'occurrences <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                              (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                 mutable'occurrences)
                      frozen'symbols <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                             mutable'symbols)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'occurrences") frozen'occurrences
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'symbols") frozen'symbols x)))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "language"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"language") y x)
                                  mutable'occurrences mutable'symbols
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "relative_path"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"relativePath") y x)
                                  mutable'occurrences mutable'symbols
                        18
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "occurrences"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'occurrences y)
                                loop x v mutable'symbols
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "symbols"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'symbols y)
                                loop x mutable'occurrences v
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "text"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"text") y x)
                                  mutable'occurrences mutable'symbols
                        48
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "position_encoding"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"positionEncoding") y x)
                                  mutable'occurrences mutable'symbols
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'occurrences mutable'symbols
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'occurrences <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       Data.ProtoLens.Encoding.Growing.new
              mutable'symbols <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                   Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'occurrences mutable'symbols)
          "Document"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"language") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view (Data.ProtoLens.Field.field @"relativePath") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8 _v))
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage _v))
                      (Lens.Family2.view
                         (Data.ProtoLens.Field.field @"vec'occurrences") _x))
                   ((Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                         (\ _v
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                                 ((Prelude..)
                                    (\ bs
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                                            (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Data.ProtoLens.encodeMessage _v))
                         (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'symbols") _x))
                      ((Data.Monoid.<>)
                         (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"text") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                                  ((Prelude..)
                                     (\ bs
                                        -> (Data.Monoid.<>)
                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                (Prelude.fromIntegral (Data.ByteString.length bs)))
                                             (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                     Data.Text.Encoding.encodeUtf8 _v))
                         ((Data.Monoid.<>)
                            (let
                               _v
                                 = Lens.Family2.view
                                     (Data.ProtoLens.Field.field @"positionEncoding") _x
                             in
                               if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                   Data.Monoid.mempty
                               else
                                   (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt 48)
                                     ((Prelude..)
                                        ((Prelude..)
                                           Data.ProtoLens.Encoding.Bytes.putVarInt
                                           Prelude.fromIntegral)
                                        Prelude.fromEnum _v))
                            (Data.ProtoLens.Encoding.Wire.buildFieldSet
                               (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))))
instance Control.DeepSeq.NFData Document where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Document'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Document'language x__)
                (Control.DeepSeq.deepseq
                   (_Document'relativePath x__)
                   (Control.DeepSeq.deepseq
                      (_Document'occurrences x__)
                      (Control.DeepSeq.deepseq
                         (_Document'symbols x__)
                         (Control.DeepSeq.deepseq
                            (_Document'text x__)
                            (Control.DeepSeq.deepseq (_Document'positionEncoding x__) ()))))))
{- | Fields :

         * 'Proto.Scip_Fields.metadata' @:: Lens' Index Metadata@
         * 'Proto.Scip_Fields.maybe'metadata' @:: Lens' Index (Prelude.Maybe Metadata)@
         * 'Proto.Scip_Fields.documents' @:: Lens' Index [Document]@
         * 'Proto.Scip_Fields.vec'documents' @:: Lens' Index (Data.Vector.Vector Document)@
         * 'Proto.Scip_Fields.externalSymbols' @:: Lens' Index [SymbolInformation]@
         * 'Proto.Scip_Fields.vec'externalSymbols' @:: Lens' Index (Data.Vector.Vector SymbolInformation)@ -}
data Index
  = Index'_constructor {_Index'metadata :: !(Prelude.Maybe Metadata),
                        _Index'documents :: !(Data.Vector.Vector Document),
                        _Index'externalSymbols :: !(Data.Vector.Vector SymbolInformation),
                        _Index'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Index where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Index "metadata" Metadata where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Index'metadata (\ x__ y__ -> x__ {_Index'metadata = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Index "maybe'metadata" (Prelude.Maybe Metadata) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Index'metadata (\ x__ y__ -> x__ {_Index'metadata = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Index "documents" [Document] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Index'documents (\ x__ y__ -> x__ {_Index'documents = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Index "vec'documents" (Data.Vector.Vector Document) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Index'documents (\ x__ y__ -> x__ {_Index'documents = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Index "externalSymbols" [SymbolInformation] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Index'externalSymbols
           (\ x__ y__ -> x__ {_Index'externalSymbols = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Index "vec'externalSymbols" (Data.Vector.Vector SymbolInformation) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Index'externalSymbols
           (\ x__ y__ -> x__ {_Index'externalSymbols = y__}))
        Prelude.id
instance Data.ProtoLens.Message Index where
  messageName _ = Data.Text.pack "scip.Index"
  packedMessageDescriptor _
    = "\n\
      \\ENQIndex\DC2*\n\
      \\bmetadata\CAN\SOH \SOH(\v2\SO.scip.MetadataR\bmetadata\DC2,\n\
      \\tdocuments\CAN\STX \ETX(\v2\SO.scip.DocumentR\tdocuments\DC2B\n\
      \\DLEexternal_symbols\CAN\ETX \ETX(\v2\ETB.scip.SymbolInformationR\SIexternalSymbols"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        metadata__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "metadata"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Metadata)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'metadata")) ::
              Data.ProtoLens.FieldDescriptor Index
        documents__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "documents"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Document)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"documents")) ::
              Data.ProtoLens.FieldDescriptor Index
        externalSymbols__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "external_symbols"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SymbolInformation)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"externalSymbols")) ::
              Data.ProtoLens.FieldDescriptor Index
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, metadata__field_descriptor),
           (Data.ProtoLens.Tag 2, documents__field_descriptor),
           (Data.ProtoLens.Tag 3, externalSymbols__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Index'_unknownFields
        (\ x__ y__ -> x__ {_Index'_unknownFields = y__})
  defMessage
    = Index'_constructor
        {_Index'metadata = Prelude.Nothing,
         _Index'documents = Data.Vector.Generic.empty,
         _Index'externalSymbols = Data.Vector.Generic.empty,
         _Index'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Index
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Document
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld SymbolInformation
                -> Data.ProtoLens.Encoding.Bytes.Parser Index
        loop x mutable'documents mutable'externalSymbols
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'documents <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                               mutable'documents)
                      frozen'externalSymbols <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                  (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                     mutable'externalSymbols)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'documents") frozen'documents
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'externalSymbols")
                                 frozen'externalSymbols x)))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "metadata"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"metadata") y x)
                                  mutable'documents mutable'externalSymbols
                        18
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "documents"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'documents y)
                                loop x v mutable'externalSymbols
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "external_symbols"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'externalSymbols y)
                                loop x mutable'documents v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'documents mutable'externalSymbols
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'documents <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                     Data.ProtoLens.Encoding.Growing.new
              mutable'externalSymbols <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                           Data.ProtoLens.Encoding.Growing.new
              loop
                Data.ProtoLens.defMessage mutable'documents
                mutable'externalSymbols)
          "Index"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'metadata") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                   (\ _v
                      -> (Data.Monoid.<>)
                           (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                           ((Prelude..)
                              (\ bs
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Data.ProtoLens.encodeMessage _v))
                   (Lens.Family2.view
                      (Data.ProtoLens.Field.field @"vec'documents") _x))
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage _v))
                      (Lens.Family2.view
                         (Data.ProtoLens.Field.field @"vec'externalSymbols") _x))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData Index where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Index'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Index'metadata x__)
                (Control.DeepSeq.deepseq
                   (_Index'documents x__)
                   (Control.DeepSeq.deepseq (_Index'externalSymbols x__) ())))
newtype Language'UnrecognizedValue
  = Language'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data Language
  = UnspecifiedLanguage |
    CSharp |
    Swift |
    Dart |
    Kotlin |
    Scala |
    Java |
    Groovy |
    Clojure |
    CommonLisp |
    Scheme |
    Racket |
    Lua |
    Perl |
    Raku |
    Python |
    Ruby |
    Elixir |
    Erlang |
    PHP |
    Hack |
    Coffeescript |
    JavaScript |
    TypeScript |
    Flow |
    Vue |
    CSS |
    Less |
    Sass |
    SCSS |
    HTML |
    XML |
    XSL |
    Go |
    C |
    CPP |
    Objective_C |
    Objective_CPP |
    Zig |
    Ada |
    Rust |
    OCaml |
    FSharp |
    SML |
    Haskell |
    Agda |
    Idris |
    Coq |
    Lean |
    APL |
    Dyalog |
    J |
    Matlab |
    Wolfram |
    R |
    Julia |
    Fortran |
    Delphi |
    Assembly |
    COBOL |
    ABAP |
    SAS |
    Razor |
    VisualBasic |
    ShellScript |
    Fish |
    Awk |
    PowerShell |
    Bat |
    SQL |
    PLSQL |
    Prolog |
    Ini |
    TOML |
    YAML |
    JSON |
    Jsonnet |
    Nix |
    Skylark |
    Makefile |
    Dockerfile |
    BibTeX |
    TeX |
    LaTeX |
    Markdown |
    ReST |
    AsciiDoc |
    Diff |
    Git_Config |
    Handlebars |
    Git_Commit |
    Git_Rebase |
    JavaScriptReact |
    TypeScriptReact |
    Solidity |
    Apex |
    CUDA |
    GraphQL |
    Pascal |
    Protobuf |
    Tcl |
    Repro |
    Thrift |
    Verilog |
    VHDL |
    Svelte |
    Slang |
    Luau |
    Justfile |
    Nickel |
    Language'Unrecognized !Language'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum Language where
  maybeToEnum 0 = Prelude.Just UnspecifiedLanguage
  maybeToEnum 1 = Prelude.Just CSharp
  maybeToEnum 2 = Prelude.Just Swift
  maybeToEnum 3 = Prelude.Just Dart
  maybeToEnum 4 = Prelude.Just Kotlin
  maybeToEnum 5 = Prelude.Just Scala
  maybeToEnum 6 = Prelude.Just Java
  maybeToEnum 7 = Prelude.Just Groovy
  maybeToEnum 8 = Prelude.Just Clojure
  maybeToEnum 9 = Prelude.Just CommonLisp
  maybeToEnum 10 = Prelude.Just Scheme
  maybeToEnum 11 = Prelude.Just Racket
  maybeToEnum 12 = Prelude.Just Lua
  maybeToEnum 13 = Prelude.Just Perl
  maybeToEnum 14 = Prelude.Just Raku
  maybeToEnum 15 = Prelude.Just Python
  maybeToEnum 16 = Prelude.Just Ruby
  maybeToEnum 17 = Prelude.Just Elixir
  maybeToEnum 18 = Prelude.Just Erlang
  maybeToEnum 19 = Prelude.Just PHP
  maybeToEnum 20 = Prelude.Just Hack
  maybeToEnum 21 = Prelude.Just Coffeescript
  maybeToEnum 22 = Prelude.Just JavaScript
  maybeToEnum 23 = Prelude.Just TypeScript
  maybeToEnum 24 = Prelude.Just Flow
  maybeToEnum 25 = Prelude.Just Vue
  maybeToEnum 26 = Prelude.Just CSS
  maybeToEnum 27 = Prelude.Just Less
  maybeToEnum 28 = Prelude.Just Sass
  maybeToEnum 29 = Prelude.Just SCSS
  maybeToEnum 30 = Prelude.Just HTML
  maybeToEnum 31 = Prelude.Just XML
  maybeToEnum 32 = Prelude.Just XSL
  maybeToEnum 33 = Prelude.Just Go
  maybeToEnum 34 = Prelude.Just C
  maybeToEnum 35 = Prelude.Just CPP
  maybeToEnum 36 = Prelude.Just Objective_C
  maybeToEnum 37 = Prelude.Just Objective_CPP
  maybeToEnum 38 = Prelude.Just Zig
  maybeToEnum 39 = Prelude.Just Ada
  maybeToEnum 40 = Prelude.Just Rust
  maybeToEnum 41 = Prelude.Just OCaml
  maybeToEnum 42 = Prelude.Just FSharp
  maybeToEnum 43 = Prelude.Just SML
  maybeToEnum 44 = Prelude.Just Haskell
  maybeToEnum 45 = Prelude.Just Agda
  maybeToEnum 46 = Prelude.Just Idris
  maybeToEnum 47 = Prelude.Just Coq
  maybeToEnum 48 = Prelude.Just Lean
  maybeToEnum 49 = Prelude.Just APL
  maybeToEnum 50 = Prelude.Just Dyalog
  maybeToEnum 51 = Prelude.Just J
  maybeToEnum 52 = Prelude.Just Matlab
  maybeToEnum 53 = Prelude.Just Wolfram
  maybeToEnum 54 = Prelude.Just R
  maybeToEnum 55 = Prelude.Just Julia
  maybeToEnum 56 = Prelude.Just Fortran
  maybeToEnum 57 = Prelude.Just Delphi
  maybeToEnum 58 = Prelude.Just Assembly
  maybeToEnum 59 = Prelude.Just COBOL
  maybeToEnum 60 = Prelude.Just ABAP
  maybeToEnum 61 = Prelude.Just SAS
  maybeToEnum 62 = Prelude.Just Razor
  maybeToEnum 63 = Prelude.Just VisualBasic
  maybeToEnum 64 = Prelude.Just ShellScript
  maybeToEnum 65 = Prelude.Just Fish
  maybeToEnum 66 = Prelude.Just Awk
  maybeToEnum 67 = Prelude.Just PowerShell
  maybeToEnum 68 = Prelude.Just Bat
  maybeToEnum 69 = Prelude.Just SQL
  maybeToEnum 70 = Prelude.Just PLSQL
  maybeToEnum 71 = Prelude.Just Prolog
  maybeToEnum 72 = Prelude.Just Ini
  maybeToEnum 73 = Prelude.Just TOML
  maybeToEnum 74 = Prelude.Just YAML
  maybeToEnum 75 = Prelude.Just JSON
  maybeToEnum 76 = Prelude.Just Jsonnet
  maybeToEnum 77 = Prelude.Just Nix
  maybeToEnum 78 = Prelude.Just Skylark
  maybeToEnum 79 = Prelude.Just Makefile
  maybeToEnum 80 = Prelude.Just Dockerfile
  maybeToEnum 81 = Prelude.Just BibTeX
  maybeToEnum 82 = Prelude.Just TeX
  maybeToEnum 83 = Prelude.Just LaTeX
  maybeToEnum 84 = Prelude.Just Markdown
  maybeToEnum 85 = Prelude.Just ReST
  maybeToEnum 86 = Prelude.Just AsciiDoc
  maybeToEnum 88 = Prelude.Just Diff
  maybeToEnum 89 = Prelude.Just Git_Config
  maybeToEnum 90 = Prelude.Just Handlebars
  maybeToEnum 91 = Prelude.Just Git_Commit
  maybeToEnum 92 = Prelude.Just Git_Rebase
  maybeToEnum 93 = Prelude.Just JavaScriptReact
  maybeToEnum 94 = Prelude.Just TypeScriptReact
  maybeToEnum 95 = Prelude.Just Solidity
  maybeToEnum 96 = Prelude.Just Apex
  maybeToEnum 97 = Prelude.Just CUDA
  maybeToEnum 98 = Prelude.Just GraphQL
  maybeToEnum 99 = Prelude.Just Pascal
  maybeToEnum 100 = Prelude.Just Protobuf
  maybeToEnum 101 = Prelude.Just Tcl
  maybeToEnum 102 = Prelude.Just Repro
  maybeToEnum 103 = Prelude.Just Thrift
  maybeToEnum 104 = Prelude.Just Verilog
  maybeToEnum 105 = Prelude.Just VHDL
  maybeToEnum 106 = Prelude.Just Svelte
  maybeToEnum 107 = Prelude.Just Slang
  maybeToEnum 108 = Prelude.Just Luau
  maybeToEnum 109 = Prelude.Just Justfile
  maybeToEnum 110 = Prelude.Just Nickel
  maybeToEnum k
    = Prelude.Just
        (Language'Unrecognized
           (Language'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum UnspecifiedLanguage = "UnspecifiedLanguage"
  showEnum ABAP = "ABAP"
  showEnum Apex = "Apex"
  showEnum APL = "APL"
  showEnum Ada = "Ada"
  showEnum Agda = "Agda"
  showEnum AsciiDoc = "AsciiDoc"
  showEnum Assembly = "Assembly"
  showEnum Awk = "Awk"
  showEnum Bat = "Bat"
  showEnum BibTeX = "BibTeX"
  showEnum C = "C"
  showEnum COBOL = "COBOL"
  showEnum CPP = "CPP"
  showEnum CSS = "CSS"
  showEnum CSharp = "CSharp"
  showEnum Clojure = "Clojure"
  showEnum Coffeescript = "Coffeescript"
  showEnum CommonLisp = "CommonLisp"
  showEnum Coq = "Coq"
  showEnum CUDA = "CUDA"
  showEnum Dart = "Dart"
  showEnum Delphi = "Delphi"
  showEnum Diff = "Diff"
  showEnum Dockerfile = "Dockerfile"
  showEnum Dyalog = "Dyalog"
  showEnum Elixir = "Elixir"
  showEnum Erlang = "Erlang"
  showEnum FSharp = "FSharp"
  showEnum Fish = "Fish"
  showEnum Flow = "Flow"
  showEnum Fortran = "Fortran"
  showEnum Git_Commit = "Git_Commit"
  showEnum Git_Config = "Git_Config"
  showEnum Git_Rebase = "Git_Rebase"
  showEnum Go = "Go"
  showEnum GraphQL = "GraphQL"
  showEnum Groovy = "Groovy"
  showEnum HTML = "HTML"
  showEnum Hack = "Hack"
  showEnum Handlebars = "Handlebars"
  showEnum Haskell = "Haskell"
  showEnum Idris = "Idris"
  showEnum Ini = "Ini"
  showEnum J = "J"
  showEnum JSON = "JSON"
  showEnum Java = "Java"
  showEnum JavaScript = "JavaScript"
  showEnum JavaScriptReact = "JavaScriptReact"
  showEnum Jsonnet = "Jsonnet"
  showEnum Julia = "Julia"
  showEnum Justfile = "Justfile"
  showEnum Kotlin = "Kotlin"
  showEnum LaTeX = "LaTeX"
  showEnum Lean = "Lean"
  showEnum Less = "Less"
  showEnum Lua = "Lua"
  showEnum Luau = "Luau"
  showEnum Makefile = "Makefile"
  showEnum Markdown = "Markdown"
  showEnum Matlab = "Matlab"
  showEnum Nickel = "Nickel"
  showEnum Nix = "Nix"
  showEnum OCaml = "OCaml"
  showEnum Objective_C = "Objective_C"
  showEnum Objective_CPP = "Objective_CPP"
  showEnum Pascal = "Pascal"
  showEnum PHP = "PHP"
  showEnum PLSQL = "PLSQL"
  showEnum Perl = "Perl"
  showEnum PowerShell = "PowerShell"
  showEnum Prolog = "Prolog"
  showEnum Protobuf = "Protobuf"
  showEnum Python = "Python"
  showEnum R = "R"
  showEnum Racket = "Racket"
  showEnum Raku = "Raku"
  showEnum Razor = "Razor"
  showEnum Repro = "Repro"
  showEnum ReST = "ReST"
  showEnum Ruby = "Ruby"
  showEnum Rust = "Rust"
  showEnum SAS = "SAS"
  showEnum SCSS = "SCSS"
  showEnum SML = "SML"
  showEnum SQL = "SQL"
  showEnum Sass = "Sass"
  showEnum Scala = "Scala"
  showEnum Scheme = "Scheme"
  showEnum ShellScript = "ShellScript"
  showEnum Skylark = "Skylark"
  showEnum Slang = "Slang"
  showEnum Solidity = "Solidity"
  showEnum Svelte = "Svelte"
  showEnum Swift = "Swift"
  showEnum Tcl = "Tcl"
  showEnum TOML = "TOML"
  showEnum TeX = "TeX"
  showEnum Thrift = "Thrift"
  showEnum TypeScript = "TypeScript"
  showEnum TypeScriptReact = "TypeScriptReact"
  showEnum Verilog = "Verilog"
  showEnum VHDL = "VHDL"
  showEnum VisualBasic = "VisualBasic"
  showEnum Vue = "Vue"
  showEnum Wolfram = "Wolfram"
  showEnum XML = "XML"
  showEnum XSL = "XSL"
  showEnum YAML = "YAML"
  showEnum Zig = "Zig"
  showEnum (Language'Unrecognized (Language'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "UnspecifiedLanguage"
    = Prelude.Just UnspecifiedLanguage
    | (Prelude.==) k "ABAP" = Prelude.Just ABAP
    | (Prelude.==) k "Apex" = Prelude.Just Apex
    | (Prelude.==) k "APL" = Prelude.Just APL
    | (Prelude.==) k "Ada" = Prelude.Just Ada
    | (Prelude.==) k "Agda" = Prelude.Just Agda
    | (Prelude.==) k "AsciiDoc" = Prelude.Just AsciiDoc
    | (Prelude.==) k "Assembly" = Prelude.Just Assembly
    | (Prelude.==) k "Awk" = Prelude.Just Awk
    | (Prelude.==) k "Bat" = Prelude.Just Bat
    | (Prelude.==) k "BibTeX" = Prelude.Just BibTeX
    | (Prelude.==) k "C" = Prelude.Just C
    | (Prelude.==) k "COBOL" = Prelude.Just COBOL
    | (Prelude.==) k "CPP" = Prelude.Just CPP
    | (Prelude.==) k "CSS" = Prelude.Just CSS
    | (Prelude.==) k "CSharp" = Prelude.Just CSharp
    | (Prelude.==) k "Clojure" = Prelude.Just Clojure
    | (Prelude.==) k "Coffeescript" = Prelude.Just Coffeescript
    | (Prelude.==) k "CommonLisp" = Prelude.Just CommonLisp
    | (Prelude.==) k "Coq" = Prelude.Just Coq
    | (Prelude.==) k "CUDA" = Prelude.Just CUDA
    | (Prelude.==) k "Dart" = Prelude.Just Dart
    | (Prelude.==) k "Delphi" = Prelude.Just Delphi
    | (Prelude.==) k "Diff" = Prelude.Just Diff
    | (Prelude.==) k "Dockerfile" = Prelude.Just Dockerfile
    | (Prelude.==) k "Dyalog" = Prelude.Just Dyalog
    | (Prelude.==) k "Elixir" = Prelude.Just Elixir
    | (Prelude.==) k "Erlang" = Prelude.Just Erlang
    | (Prelude.==) k "FSharp" = Prelude.Just FSharp
    | (Prelude.==) k "Fish" = Prelude.Just Fish
    | (Prelude.==) k "Flow" = Prelude.Just Flow
    | (Prelude.==) k "Fortran" = Prelude.Just Fortran
    | (Prelude.==) k "Git_Commit" = Prelude.Just Git_Commit
    | (Prelude.==) k "Git_Config" = Prelude.Just Git_Config
    | (Prelude.==) k "Git_Rebase" = Prelude.Just Git_Rebase
    | (Prelude.==) k "Go" = Prelude.Just Go
    | (Prelude.==) k "GraphQL" = Prelude.Just GraphQL
    | (Prelude.==) k "Groovy" = Prelude.Just Groovy
    | (Prelude.==) k "HTML" = Prelude.Just HTML
    | (Prelude.==) k "Hack" = Prelude.Just Hack
    | (Prelude.==) k "Handlebars" = Prelude.Just Handlebars
    | (Prelude.==) k "Haskell" = Prelude.Just Haskell
    | (Prelude.==) k "Idris" = Prelude.Just Idris
    | (Prelude.==) k "Ini" = Prelude.Just Ini
    | (Prelude.==) k "J" = Prelude.Just J
    | (Prelude.==) k "JSON" = Prelude.Just JSON
    | (Prelude.==) k "Java" = Prelude.Just Java
    | (Prelude.==) k "JavaScript" = Prelude.Just JavaScript
    | (Prelude.==) k "JavaScriptReact" = Prelude.Just JavaScriptReact
    | (Prelude.==) k "Jsonnet" = Prelude.Just Jsonnet
    | (Prelude.==) k "Julia" = Prelude.Just Julia
    | (Prelude.==) k "Justfile" = Prelude.Just Justfile
    | (Prelude.==) k "Kotlin" = Prelude.Just Kotlin
    | (Prelude.==) k "LaTeX" = Prelude.Just LaTeX
    | (Prelude.==) k "Lean" = Prelude.Just Lean
    | (Prelude.==) k "Less" = Prelude.Just Less
    | (Prelude.==) k "Lua" = Prelude.Just Lua
    | (Prelude.==) k "Luau" = Prelude.Just Luau
    | (Prelude.==) k "Makefile" = Prelude.Just Makefile
    | (Prelude.==) k "Markdown" = Prelude.Just Markdown
    | (Prelude.==) k "Matlab" = Prelude.Just Matlab
    | (Prelude.==) k "Nickel" = Prelude.Just Nickel
    | (Prelude.==) k "Nix" = Prelude.Just Nix
    | (Prelude.==) k "OCaml" = Prelude.Just OCaml
    | (Prelude.==) k "Objective_C" = Prelude.Just Objective_C
    | (Prelude.==) k "Objective_CPP" = Prelude.Just Objective_CPP
    | (Prelude.==) k "Pascal" = Prelude.Just Pascal
    | (Prelude.==) k "PHP" = Prelude.Just PHP
    | (Prelude.==) k "PLSQL" = Prelude.Just PLSQL
    | (Prelude.==) k "Perl" = Prelude.Just Perl
    | (Prelude.==) k "PowerShell" = Prelude.Just PowerShell
    | (Prelude.==) k "Prolog" = Prelude.Just Prolog
    | (Prelude.==) k "Protobuf" = Prelude.Just Protobuf
    | (Prelude.==) k "Python" = Prelude.Just Python
    | (Prelude.==) k "R" = Prelude.Just R
    | (Prelude.==) k "Racket" = Prelude.Just Racket
    | (Prelude.==) k "Raku" = Prelude.Just Raku
    | (Prelude.==) k "Razor" = Prelude.Just Razor
    | (Prelude.==) k "Repro" = Prelude.Just Repro
    | (Prelude.==) k "ReST" = Prelude.Just ReST
    | (Prelude.==) k "Ruby" = Prelude.Just Ruby
    | (Prelude.==) k "Rust" = Prelude.Just Rust
    | (Prelude.==) k "SAS" = Prelude.Just SAS
    | (Prelude.==) k "SCSS" = Prelude.Just SCSS
    | (Prelude.==) k "SML" = Prelude.Just SML
    | (Prelude.==) k "SQL" = Prelude.Just SQL
    | (Prelude.==) k "Sass" = Prelude.Just Sass
    | (Prelude.==) k "Scala" = Prelude.Just Scala
    | (Prelude.==) k "Scheme" = Prelude.Just Scheme
    | (Prelude.==) k "ShellScript" = Prelude.Just ShellScript
    | (Prelude.==) k "Skylark" = Prelude.Just Skylark
    | (Prelude.==) k "Slang" = Prelude.Just Slang
    | (Prelude.==) k "Solidity" = Prelude.Just Solidity
    | (Prelude.==) k "Svelte" = Prelude.Just Svelte
    | (Prelude.==) k "Swift" = Prelude.Just Swift
    | (Prelude.==) k "Tcl" = Prelude.Just Tcl
    | (Prelude.==) k "TOML" = Prelude.Just TOML
    | (Prelude.==) k "TeX" = Prelude.Just TeX
    | (Prelude.==) k "Thrift" = Prelude.Just Thrift
    | (Prelude.==) k "TypeScript" = Prelude.Just TypeScript
    | (Prelude.==) k "TypeScriptReact" = Prelude.Just TypeScriptReact
    | (Prelude.==) k "Verilog" = Prelude.Just Verilog
    | (Prelude.==) k "VHDL" = Prelude.Just VHDL
    | (Prelude.==) k "VisualBasic" = Prelude.Just VisualBasic
    | (Prelude.==) k "Vue" = Prelude.Just Vue
    | (Prelude.==) k "Wolfram" = Prelude.Just Wolfram
    | (Prelude.==) k "XML" = Prelude.Just XML
    | (Prelude.==) k "XSL" = Prelude.Just XSL
    | (Prelude.==) k "YAML" = Prelude.Just YAML
    | (Prelude.==) k "Zig" = Prelude.Just Zig
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded Language where
  minBound = UnspecifiedLanguage
  maxBound = Nickel
instance Prelude.Enum Language where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum Language: " (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum UnspecifiedLanguage = 0
  fromEnum CSharp = 1
  fromEnum Swift = 2
  fromEnum Dart = 3
  fromEnum Kotlin = 4
  fromEnum Scala = 5
  fromEnum Java = 6
  fromEnum Groovy = 7
  fromEnum Clojure = 8
  fromEnum CommonLisp = 9
  fromEnum Scheme = 10
  fromEnum Racket = 11
  fromEnum Lua = 12
  fromEnum Perl = 13
  fromEnum Raku = 14
  fromEnum Python = 15
  fromEnum Ruby = 16
  fromEnum Elixir = 17
  fromEnum Erlang = 18
  fromEnum PHP = 19
  fromEnum Hack = 20
  fromEnum Coffeescript = 21
  fromEnum JavaScript = 22
  fromEnum TypeScript = 23
  fromEnum Flow = 24
  fromEnum Vue = 25
  fromEnum CSS = 26
  fromEnum Less = 27
  fromEnum Sass = 28
  fromEnum SCSS = 29
  fromEnum HTML = 30
  fromEnum XML = 31
  fromEnum XSL = 32
  fromEnum Go = 33
  fromEnum C = 34
  fromEnum CPP = 35
  fromEnum Objective_C = 36
  fromEnum Objective_CPP = 37
  fromEnum Zig = 38
  fromEnum Ada = 39
  fromEnum Rust = 40
  fromEnum OCaml = 41
  fromEnum FSharp = 42
  fromEnum SML = 43
  fromEnum Haskell = 44
  fromEnum Agda = 45
  fromEnum Idris = 46
  fromEnum Coq = 47
  fromEnum Lean = 48
  fromEnum APL = 49
  fromEnum Dyalog = 50
  fromEnum J = 51
  fromEnum Matlab = 52
  fromEnum Wolfram = 53
  fromEnum R = 54
  fromEnum Julia = 55
  fromEnum Fortran = 56
  fromEnum Delphi = 57
  fromEnum Assembly = 58
  fromEnum COBOL = 59
  fromEnum ABAP = 60
  fromEnum SAS = 61
  fromEnum Razor = 62
  fromEnum VisualBasic = 63
  fromEnum ShellScript = 64
  fromEnum Fish = 65
  fromEnum Awk = 66
  fromEnum PowerShell = 67
  fromEnum Bat = 68
  fromEnum SQL = 69
  fromEnum PLSQL = 70
  fromEnum Prolog = 71
  fromEnum Ini = 72
  fromEnum TOML = 73
  fromEnum YAML = 74
  fromEnum JSON = 75
  fromEnum Jsonnet = 76
  fromEnum Nix = 77
  fromEnum Skylark = 78
  fromEnum Makefile = 79
  fromEnum Dockerfile = 80
  fromEnum BibTeX = 81
  fromEnum TeX = 82
  fromEnum LaTeX = 83
  fromEnum Markdown = 84
  fromEnum ReST = 85
  fromEnum AsciiDoc = 86
  fromEnum Diff = 88
  fromEnum Git_Config = 89
  fromEnum Handlebars = 90
  fromEnum Git_Commit = 91
  fromEnum Git_Rebase = 92
  fromEnum JavaScriptReact = 93
  fromEnum TypeScriptReact = 94
  fromEnum Solidity = 95
  fromEnum Apex = 96
  fromEnum CUDA = 97
  fromEnum GraphQL = 98
  fromEnum Pascal = 99
  fromEnum Protobuf = 100
  fromEnum Tcl = 101
  fromEnum Repro = 102
  fromEnum Thrift = 103
  fromEnum Verilog = 104
  fromEnum VHDL = 105
  fromEnum Svelte = 106
  fromEnum Slang = 107
  fromEnum Luau = 108
  fromEnum Justfile = 109
  fromEnum Nickel = 110
  fromEnum (Language'Unrecognized (Language'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ Nickel
    = Prelude.error
        "Language.succ: bad argument Nickel. This value would be out of bounds."
  succ UnspecifiedLanguage = CSharp
  succ CSharp = Swift
  succ Swift = Dart
  succ Dart = Kotlin
  succ Kotlin = Scala
  succ Scala = Java
  succ Java = Groovy
  succ Groovy = Clojure
  succ Clojure = CommonLisp
  succ CommonLisp = Scheme
  succ Scheme = Racket
  succ Racket = Lua
  succ Lua = Perl
  succ Perl = Raku
  succ Raku = Python
  succ Python = Ruby
  succ Ruby = Elixir
  succ Elixir = Erlang
  succ Erlang = PHP
  succ PHP = Hack
  succ Hack = Coffeescript
  succ Coffeescript = JavaScript
  succ JavaScript = TypeScript
  succ TypeScript = Flow
  succ Flow = Vue
  succ Vue = CSS
  succ CSS = Less
  succ Less = Sass
  succ Sass = SCSS
  succ SCSS = HTML
  succ HTML = XML
  succ XML = XSL
  succ XSL = Go
  succ Go = C
  succ C = CPP
  succ CPP = Objective_C
  succ Objective_C = Objective_CPP
  succ Objective_CPP = Zig
  succ Zig = Ada
  succ Ada = Rust
  succ Rust = OCaml
  succ OCaml = FSharp
  succ FSharp = SML
  succ SML = Haskell
  succ Haskell = Agda
  succ Agda = Idris
  succ Idris = Coq
  succ Coq = Lean
  succ Lean = APL
  succ APL = Dyalog
  succ Dyalog = J
  succ J = Matlab
  succ Matlab = Wolfram
  succ Wolfram = R
  succ R = Julia
  succ Julia = Fortran
  succ Fortran = Delphi
  succ Delphi = Assembly
  succ Assembly = COBOL
  succ COBOL = ABAP
  succ ABAP = SAS
  succ SAS = Razor
  succ Razor = VisualBasic
  succ VisualBasic = ShellScript
  succ ShellScript = Fish
  succ Fish = Awk
  succ Awk = PowerShell
  succ PowerShell = Bat
  succ Bat = SQL
  succ SQL = PLSQL
  succ PLSQL = Prolog
  succ Prolog = Ini
  succ Ini = TOML
  succ TOML = YAML
  succ YAML = JSON
  succ JSON = Jsonnet
  succ Jsonnet = Nix
  succ Nix = Skylark
  succ Skylark = Makefile
  succ Makefile = Dockerfile
  succ Dockerfile = BibTeX
  succ BibTeX = TeX
  succ TeX = LaTeX
  succ LaTeX = Markdown
  succ Markdown = ReST
  succ ReST = AsciiDoc
  succ AsciiDoc = Diff
  succ Diff = Git_Config
  succ Git_Config = Handlebars
  succ Handlebars = Git_Commit
  succ Git_Commit = Git_Rebase
  succ Git_Rebase = JavaScriptReact
  succ JavaScriptReact = TypeScriptReact
  succ TypeScriptReact = Solidity
  succ Solidity = Apex
  succ Apex = CUDA
  succ CUDA = GraphQL
  succ GraphQL = Pascal
  succ Pascal = Protobuf
  succ Protobuf = Tcl
  succ Tcl = Repro
  succ Repro = Thrift
  succ Thrift = Verilog
  succ Verilog = VHDL
  succ VHDL = Svelte
  succ Svelte = Slang
  succ Slang = Luau
  succ Luau = Justfile
  succ Justfile = Nickel
  succ (Language'Unrecognized _)
    = Prelude.error "Language.succ: bad argument: unrecognized value"
  pred UnspecifiedLanguage
    = Prelude.error
        "Language.pred: bad argument UnspecifiedLanguage. This value would be out of bounds."
  pred CSharp = UnspecifiedLanguage
  pred Swift = CSharp
  pred Dart = Swift
  pred Kotlin = Dart
  pred Scala = Kotlin
  pred Java = Scala
  pred Groovy = Java
  pred Clojure = Groovy
  pred CommonLisp = Clojure
  pred Scheme = CommonLisp
  pred Racket = Scheme
  pred Lua = Racket
  pred Perl = Lua
  pred Raku = Perl
  pred Python = Raku
  pred Ruby = Python
  pred Elixir = Ruby
  pred Erlang = Elixir
  pred PHP = Erlang
  pred Hack = PHP
  pred Coffeescript = Hack
  pred JavaScript = Coffeescript
  pred TypeScript = JavaScript
  pred Flow = TypeScript
  pred Vue = Flow
  pred CSS = Vue
  pred Less = CSS
  pred Sass = Less
  pred SCSS = Sass
  pred HTML = SCSS
  pred XML = HTML
  pred XSL = XML
  pred Go = XSL
  pred C = Go
  pred CPP = C
  pred Objective_C = CPP
  pred Objective_CPP = Objective_C
  pred Zig = Objective_CPP
  pred Ada = Zig
  pred Rust = Ada
  pred OCaml = Rust
  pred FSharp = OCaml
  pred SML = FSharp
  pred Haskell = SML
  pred Agda = Haskell
  pred Idris = Agda
  pred Coq = Idris
  pred Lean = Coq
  pred APL = Lean
  pred Dyalog = APL
  pred J = Dyalog
  pred Matlab = J
  pred Wolfram = Matlab
  pred R = Wolfram
  pred Julia = R
  pred Fortran = Julia
  pred Delphi = Fortran
  pred Assembly = Delphi
  pred COBOL = Assembly
  pred ABAP = COBOL
  pred SAS = ABAP
  pred Razor = SAS
  pred VisualBasic = Razor
  pred ShellScript = VisualBasic
  pred Fish = ShellScript
  pred Awk = Fish
  pred PowerShell = Awk
  pred Bat = PowerShell
  pred SQL = Bat
  pred PLSQL = SQL
  pred Prolog = PLSQL
  pred Ini = Prolog
  pred TOML = Ini
  pred YAML = TOML
  pred JSON = YAML
  pred Jsonnet = JSON
  pred Nix = Jsonnet
  pred Skylark = Nix
  pred Makefile = Skylark
  pred Dockerfile = Makefile
  pred BibTeX = Dockerfile
  pred TeX = BibTeX
  pred LaTeX = TeX
  pred Markdown = LaTeX
  pred ReST = Markdown
  pred AsciiDoc = ReST
  pred Diff = AsciiDoc
  pred Git_Config = Diff
  pred Handlebars = Git_Config
  pred Git_Commit = Handlebars
  pred Git_Rebase = Git_Commit
  pred JavaScriptReact = Git_Rebase
  pred TypeScriptReact = JavaScriptReact
  pred Solidity = TypeScriptReact
  pred Apex = Solidity
  pred CUDA = Apex
  pred GraphQL = CUDA
  pred Pascal = GraphQL
  pred Protobuf = Pascal
  pred Tcl = Protobuf
  pred Repro = Tcl
  pred Thrift = Repro
  pred Verilog = Thrift
  pred VHDL = Verilog
  pred Svelte = VHDL
  pred Slang = Svelte
  pred Luau = Slang
  pred Justfile = Luau
  pred Nickel = Justfile
  pred (Language'Unrecognized _)
    = Prelude.error "Language.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault Language where
  fieldDefault = UnspecifiedLanguage
instance Control.DeepSeq.NFData Language where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :

         * 'Proto.Scip_Fields.version' @:: Lens' Metadata ProtocolVersion@
         * 'Proto.Scip_Fields.toolInfo' @:: Lens' Metadata ToolInfo@
         * 'Proto.Scip_Fields.maybe'toolInfo' @:: Lens' Metadata (Prelude.Maybe ToolInfo)@
         * 'Proto.Scip_Fields.projectRoot' @:: Lens' Metadata Data.Text.Text@
         * 'Proto.Scip_Fields.textDocumentEncoding' @:: Lens' Metadata TextEncoding@ -}
data Metadata
  = Metadata'_constructor {_Metadata'version :: !ProtocolVersion,
                           _Metadata'toolInfo :: !(Prelude.Maybe ToolInfo),
                           _Metadata'projectRoot :: !Data.Text.Text,
                           _Metadata'textDocumentEncoding :: !TextEncoding,
                           _Metadata'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Metadata where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Metadata "version" ProtocolVersion where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Metadata'version (\ x__ y__ -> x__ {_Metadata'version = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Metadata "toolInfo" ToolInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Metadata'toolInfo (\ x__ y__ -> x__ {_Metadata'toolInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Metadata "maybe'toolInfo" (Prelude.Maybe ToolInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Metadata'toolInfo (\ x__ y__ -> x__ {_Metadata'toolInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Metadata "projectRoot" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Metadata'projectRoot
           (\ x__ y__ -> x__ {_Metadata'projectRoot = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Metadata "textDocumentEncoding" TextEncoding where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Metadata'textDocumentEncoding
           (\ x__ y__ -> x__ {_Metadata'textDocumentEncoding = y__}))
        Prelude.id
instance Data.ProtoLens.Message Metadata where
  messageName _ = Data.Text.pack "scip.Metadata"
  packedMessageDescriptor _
    = "\n\
      \\bMetadata\DC2/\n\
      \\aversion\CAN\SOH \SOH(\SO2\NAK.scip.ProtocolVersionR\aversion\DC2+\n\
      \\ttool_info\CAN\STX \SOH(\v2\SO.scip.ToolInfoR\btoolInfo\DC2!\n\
      \\fproject_root\CAN\ETX \SOH(\tR\vprojectRoot\DC2H\n\
      \\SYNtext_document_encoding\CAN\EOT \SOH(\SO2\DC2.scip.TextEncodingR\DC4textDocumentEncoding"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        version__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "version"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor ProtocolVersion)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"version")) ::
              Data.ProtoLens.FieldDescriptor Metadata
        toolInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "tool_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ToolInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'toolInfo")) ::
              Data.ProtoLens.FieldDescriptor Metadata
        projectRoot__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "project_root"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"projectRoot")) ::
              Data.ProtoLens.FieldDescriptor Metadata
        textDocumentEncoding__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "text_document_encoding"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor TextEncoding)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"textDocumentEncoding")) ::
              Data.ProtoLens.FieldDescriptor Metadata
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, version__field_descriptor),
           (Data.ProtoLens.Tag 2, toolInfo__field_descriptor),
           (Data.ProtoLens.Tag 3, projectRoot__field_descriptor),
           (Data.ProtoLens.Tag 4, textDocumentEncoding__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Metadata'_unknownFields
        (\ x__ y__ -> x__ {_Metadata'_unknownFields = y__})
  defMessage
    = Metadata'_constructor
        {_Metadata'version = Data.ProtoLens.fieldDefault,
         _Metadata'toolInfo = Prelude.Nothing,
         _Metadata'projectRoot = Data.ProtoLens.fieldDefault,
         _Metadata'textDocumentEncoding = Data.ProtoLens.fieldDefault,
         _Metadata'_unknownFields = []}
  parseMessage
    = let
        loop :: Metadata -> Data.ProtoLens.Encoding.Bytes.Parser Metadata
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "version"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"version") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "tool_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"toolInfo") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "project_root"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"projectRoot") y x)
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "text_document_encoding"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"textDocumentEncoding") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Metadata"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"version") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                         Prelude.fromEnum _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'toolInfo") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                ((Data.Monoid.<>)
                   (let
                      _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"projectRoot") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8 _v))
                   ((Data.Monoid.<>)
                      (let
                         _v
                           = Lens.Family2.view
                               (Data.ProtoLens.Field.field @"textDocumentEncoding") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 32)
                               ((Prelude..)
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                                  Prelude.fromEnum _v))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData Metadata where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Metadata'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Metadata'version x__)
                (Control.DeepSeq.deepseq
                   (_Metadata'toolInfo x__)
                   (Control.DeepSeq.deepseq
                      (_Metadata'projectRoot x__)
                      (Control.DeepSeq.deepseq
                         (_Metadata'textDocumentEncoding x__) ()))))
{- | Fields :

         * 'Proto.Scip_Fields.range' @:: Lens' Occurrence [Data.Int.Int32]@
         * 'Proto.Scip_Fields.vec'range' @:: Lens' Occurrence (Data.Vector.Unboxed.Vector Data.Int.Int32)@
         * 'Proto.Scip_Fields.symbol' @:: Lens' Occurrence Data.Text.Text@
         * 'Proto.Scip_Fields.symbolRoles' @:: Lens' Occurrence Data.Int.Int32@
         * 'Proto.Scip_Fields.overrideDocumentation' @:: Lens' Occurrence [Data.Text.Text]@
         * 'Proto.Scip_Fields.vec'overrideDocumentation' @:: Lens' Occurrence (Data.Vector.Vector Data.Text.Text)@
         * 'Proto.Scip_Fields.syntaxKind' @:: Lens' Occurrence SyntaxKind@
         * 'Proto.Scip_Fields.diagnostics' @:: Lens' Occurrence [Diagnostic]@
         * 'Proto.Scip_Fields.vec'diagnostics' @:: Lens' Occurrence (Data.Vector.Vector Diagnostic)@
         * 'Proto.Scip_Fields.enclosingRange' @:: Lens' Occurrence [Data.Int.Int32]@
         * 'Proto.Scip_Fields.vec'enclosingRange' @:: Lens' Occurrence (Data.Vector.Unboxed.Vector Data.Int.Int32)@ -}
data Occurrence
  = Occurrence'_constructor {_Occurrence'range :: !(Data.Vector.Unboxed.Vector Data.Int.Int32),
                             _Occurrence'symbol :: !Data.Text.Text,
                             _Occurrence'symbolRoles :: !Data.Int.Int32,
                             _Occurrence'overrideDocumentation :: !(Data.Vector.Vector Data.Text.Text),
                             _Occurrence'syntaxKind :: !SyntaxKind,
                             _Occurrence'diagnostics :: !(Data.Vector.Vector Diagnostic),
                             _Occurrence'enclosingRange :: !(Data.Vector.Unboxed.Vector Data.Int.Int32),
                             _Occurrence'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Occurrence where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Occurrence "range" [Data.Int.Int32] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Occurrence'range (\ x__ y__ -> x__ {_Occurrence'range = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Occurrence "vec'range" (Data.Vector.Unboxed.Vector Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Occurrence'range (\ x__ y__ -> x__ {_Occurrence'range = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Occurrence "symbol" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Occurrence'symbol (\ x__ y__ -> x__ {_Occurrence'symbol = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Occurrence "symbolRoles" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Occurrence'symbolRoles
           (\ x__ y__ -> x__ {_Occurrence'symbolRoles = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Occurrence "overrideDocumentation" [Data.Text.Text] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Occurrence'overrideDocumentation
           (\ x__ y__ -> x__ {_Occurrence'overrideDocumentation = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Occurrence "vec'overrideDocumentation" (Data.Vector.Vector Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Occurrence'overrideDocumentation
           (\ x__ y__ -> x__ {_Occurrence'overrideDocumentation = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Occurrence "syntaxKind" SyntaxKind where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Occurrence'syntaxKind
           (\ x__ y__ -> x__ {_Occurrence'syntaxKind = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Occurrence "diagnostics" [Diagnostic] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Occurrence'diagnostics
           (\ x__ y__ -> x__ {_Occurrence'diagnostics = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Occurrence "vec'diagnostics" (Data.Vector.Vector Diagnostic) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Occurrence'diagnostics
           (\ x__ y__ -> x__ {_Occurrence'diagnostics = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Occurrence "enclosingRange" [Data.Int.Int32] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Occurrence'enclosingRange
           (\ x__ y__ -> x__ {_Occurrence'enclosingRange = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Occurrence "vec'enclosingRange" (Data.Vector.Unboxed.Vector Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Occurrence'enclosingRange
           (\ x__ y__ -> x__ {_Occurrence'enclosingRange = y__}))
        Prelude.id
instance Data.ProtoLens.Message Occurrence where
  messageName _ = Data.Text.pack "scip.Occurrence"
  packedMessageDescriptor _
    = "\n\
      \\n\
      \Occurrence\DC2\DC4\n\
      \\ENQrange\CAN\SOH \ETX(\ENQR\ENQrange\DC2\SYN\n\
      \\ACKsymbol\CAN\STX \SOH(\tR\ACKsymbol\DC2!\n\
      \\fsymbol_roles\CAN\ETX \SOH(\ENQR\vsymbolRoles\DC25\n\
      \\SYNoverride_documentation\CAN\EOT \ETX(\tR\NAKoverrideDocumentation\DC21\n\
      \\vsyntax_kind\CAN\ENQ \SOH(\SO2\DLE.scip.SyntaxKindR\n\
      \syntaxKind\DC22\n\
      \\vdiagnostics\CAN\ACK \ETX(\v2\DLE.scip.DiagnosticR\vdiagnostics\DC2'\n\
      \\SIenclosing_range\CAN\a \ETX(\ENQR\SOenclosingRange"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        range__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "range"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed (Data.ProtoLens.Field.field @"range")) ::
              Data.ProtoLens.FieldDescriptor Occurrence
        symbol__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "symbol"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"symbol")) ::
              Data.ProtoLens.FieldDescriptor Occurrence
        symbolRoles__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "symbol_roles"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"symbolRoles")) ::
              Data.ProtoLens.FieldDescriptor Occurrence
        overrideDocumentation__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "override_documentation"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"overrideDocumentation")) ::
              Data.ProtoLens.FieldDescriptor Occurrence
        syntaxKind__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "syntax_kind"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor SyntaxKind)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"syntaxKind")) ::
              Data.ProtoLens.FieldDescriptor Occurrence
        diagnostics__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "diagnostics"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Diagnostic)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"diagnostics")) ::
              Data.ProtoLens.FieldDescriptor Occurrence
        enclosingRange__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "enclosing_range"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"enclosingRange")) ::
              Data.ProtoLens.FieldDescriptor Occurrence
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, range__field_descriptor),
           (Data.ProtoLens.Tag 2, symbol__field_descriptor),
           (Data.ProtoLens.Tag 3, symbolRoles__field_descriptor),
           (Data.ProtoLens.Tag 4, overrideDocumentation__field_descriptor),
           (Data.ProtoLens.Tag 5, syntaxKind__field_descriptor),
           (Data.ProtoLens.Tag 6, diagnostics__field_descriptor),
           (Data.ProtoLens.Tag 7, enclosingRange__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Occurrence'_unknownFields
        (\ x__ y__ -> x__ {_Occurrence'_unknownFields = y__})
  defMessage
    = Occurrence'_constructor
        {_Occurrence'range = Data.Vector.Generic.empty,
         _Occurrence'symbol = Data.ProtoLens.fieldDefault,
         _Occurrence'symbolRoles = Data.ProtoLens.fieldDefault,
         _Occurrence'overrideDocumentation = Data.Vector.Generic.empty,
         _Occurrence'syntaxKind = Data.ProtoLens.fieldDefault,
         _Occurrence'diagnostics = Data.Vector.Generic.empty,
         _Occurrence'enclosingRange = Data.Vector.Generic.empty,
         _Occurrence'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Occurrence
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Diagnostic
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Int.Int32
                -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Text.Text
                   -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Int.Int32
                      -> Data.ProtoLens.Encoding.Bytes.Parser Occurrence
        loop
          x
          mutable'diagnostics
          mutable'enclosingRange
          mutable'overrideDocumentation
          mutable'range
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'diagnostics <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                              (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                 mutable'diagnostics)
                      frozen'enclosingRange <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                 (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                    mutable'enclosingRange)
                      frozen'overrideDocumentation <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                           mutable'overrideDocumentation)
                      frozen'range <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'range)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'diagnostics") frozen'diagnostics
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'enclosingRange")
                                 frozen'enclosingRange
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"vec'overrideDocumentation")
                                    frozen'overrideDocumentation
                                    (Lens.Family2.set
                                       (Data.ProtoLens.Field.field @"vec'range") frozen'range x)))))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Prelude.fromIntegral
                                           Data.ProtoLens.Encoding.Bytes.getVarInt)
                                        "range"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'range y)
                                loop
                                  x mutable'diagnostics mutable'enclosingRange
                                  mutable'overrideDocumentation v
                        10
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Prelude.fromIntegral
                                                                       Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                                    "range"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'range)
                                loop
                                  x mutable'diagnostics mutable'enclosingRange
                                  mutable'overrideDocumentation y
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "symbol"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"symbol") y x)
                                  mutable'diagnostics mutable'enclosingRange
                                  mutable'overrideDocumentation mutable'range
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "symbol_roles"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"symbolRoles") y x)
                                  mutable'diagnostics mutable'enclosingRange
                                  mutable'overrideDocumentation mutable'range
                        34
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                        Data.ProtoLens.Encoding.Bytes.getBytes
                                                          (Prelude.fromIntegral len)
                                            Data.ProtoLens.Encoding.Bytes.runEither
                                              (case Data.Text.Encoding.decodeUtf8' value of
                                                 (Prelude.Left err)
                                                   -> Prelude.Left (Prelude.show err)
                                                 (Prelude.Right r) -> Prelude.Right r))
                                        "override_documentation"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'overrideDocumentation y)
                                loop x mutable'diagnostics mutable'enclosingRange v mutable'range
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "syntax_kind"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"syntaxKind") y x)
                                  mutable'diagnostics mutable'enclosingRange
                                  mutable'overrideDocumentation mutable'range
                        50
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "diagnostics"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'diagnostics y)
                                loop
                                  x v mutable'enclosingRange mutable'overrideDocumentation
                                  mutable'range
                        56
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Prelude.fromIntegral
                                           Data.ProtoLens.Encoding.Bytes.getVarInt)
                                        "enclosing_range"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'enclosingRange y)
                                loop
                                  x mutable'diagnostics v mutable'overrideDocumentation
                                  mutable'range
                        58
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Prelude.fromIntegral
                                                                       Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                                    "enclosing_range"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'enclosingRange)
                                loop
                                  x mutable'diagnostics y mutable'overrideDocumentation
                                  mutable'range
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'diagnostics mutable'enclosingRange
                                  mutable'overrideDocumentation mutable'range
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'diagnostics <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       Data.ProtoLens.Encoding.Growing.new
              mutable'enclosingRange <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          Data.ProtoLens.Encoding.Growing.new
              mutable'overrideDocumentation <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                 Data.ProtoLens.Encoding.Growing.new
              mutable'range <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              loop
                Data.ProtoLens.defMessage mutable'diagnostics
                mutable'enclosingRange mutable'overrideDocumentation mutable'range)
          "Occurrence"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                p = Lens.Family2.view (Data.ProtoLens.Field.field @"vec'range") _x
              in
                if Data.Vector.Generic.null p then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((\ bs
                          -> (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                  (Prelude.fromIntegral (Data.ByteString.length bs)))
                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         (Data.ProtoLens.Encoding.Bytes.runBuilder
                            (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                               ((Prelude..)
                                  Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                               p))))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"symbol") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8 _v))
                ((Data.Monoid.<>)
                   (let
                      _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"symbolRoles") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                   ((Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                         (\ _v
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                 ((Prelude..)
                                    (\ bs
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                                            (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Data.Text.Encoding.encodeUtf8 _v))
                         (Lens.Family2.view
                            (Data.ProtoLens.Field.field @"vec'overrideDocumentation") _x))
                      ((Data.Monoid.<>)
                         (let
                            _v
                              = Lens.Family2.view (Data.ProtoLens.Field.field @"syntaxKind") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                                  ((Prelude..)
                                     ((Prelude..)
                                        Data.ProtoLens.Encoding.Bytes.putVarInt
                                        Prelude.fromIntegral)
                                     Prelude.fromEnum _v))
                         ((Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                               (\ _v
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt 50)
                                       ((Prelude..)
                                          (\ bs
                                             -> (Data.Monoid.<>)
                                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                     (Prelude.fromIntegral
                                                        (Data.ByteString.length bs)))
                                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                          Data.ProtoLens.encodeMessage _v))
                               (Lens.Family2.view
                                  (Data.ProtoLens.Field.field @"vec'diagnostics") _x))
                            ((Data.Monoid.<>)
                               (let
                                  p = Lens.Family2.view
                                        (Data.ProtoLens.Field.field @"vec'enclosingRange") _x
                                in
                                  if Data.Vector.Generic.null p then
                                      Data.Monoid.mempty
                                  else
                                      (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt 58)
                                        ((\ bs
                                            -> (Data.Monoid.<>)
                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                    (Prelude.fromIntegral
                                                       (Data.ByteString.length bs)))
                                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                           (Data.ProtoLens.Encoding.Bytes.runBuilder
                                              (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                 ((Prelude..)
                                                    Data.ProtoLens.Encoding.Bytes.putVarInt
                                                    Prelude.fromIntegral)
                                                 p))))
                               (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                  (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))))
instance Control.DeepSeq.NFData Occurrence where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Occurrence'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Occurrence'range x__)
                (Control.DeepSeq.deepseq
                   (_Occurrence'symbol x__)
                   (Control.DeepSeq.deepseq
                      (_Occurrence'symbolRoles x__)
                      (Control.DeepSeq.deepseq
                         (_Occurrence'overrideDocumentation x__)
                         (Control.DeepSeq.deepseq
                            (_Occurrence'syntaxKind x__)
                            (Control.DeepSeq.deepseq
                               (_Occurrence'diagnostics x__)
                               (Control.DeepSeq.deepseq (_Occurrence'enclosingRange x__) ())))))))
{- | Fields :

         * 'Proto.Scip_Fields.manager' @:: Lens' Package Data.Text.Text@
         * 'Proto.Scip_Fields.name' @:: Lens' Package Data.Text.Text@
         * 'Proto.Scip_Fields.version' @:: Lens' Package Data.Text.Text@ -}
data Package
  = Package'_constructor {_Package'manager :: !Data.Text.Text,
                          _Package'name :: !Data.Text.Text,
                          _Package'version :: !Data.Text.Text,
                          _Package'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Package where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Package "manager" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Package'manager (\ x__ y__ -> x__ {_Package'manager = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Package "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Package'name (\ x__ y__ -> x__ {_Package'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Package "version" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Package'version (\ x__ y__ -> x__ {_Package'version = y__}))
        Prelude.id
instance Data.ProtoLens.Message Package where
  messageName _ = Data.Text.pack "scip.Package"
  packedMessageDescriptor _
    = "\n\
      \\aPackage\DC2\CAN\n\
      \\amanager\CAN\SOH \SOH(\tR\amanager\DC2\DC2\n\
      \\EOTname\CAN\STX \SOH(\tR\EOTname\DC2\CAN\n\
      \\aversion\CAN\ETX \SOH(\tR\aversion"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        manager__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "manager"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"manager")) ::
              Data.ProtoLens.FieldDescriptor Package
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor Package
        version__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "version"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"version")) ::
              Data.ProtoLens.FieldDescriptor Package
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, manager__field_descriptor),
           (Data.ProtoLens.Tag 2, name__field_descriptor),
           (Data.ProtoLens.Tag 3, version__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Package'_unknownFields
        (\ x__ y__ -> x__ {_Package'_unknownFields = y__})
  defMessage
    = Package'_constructor
        {_Package'manager = Data.ProtoLens.fieldDefault,
         _Package'name = Data.ProtoLens.fieldDefault,
         _Package'version = Data.ProtoLens.fieldDefault,
         _Package'_unknownFields = []}
  parseMessage
    = let
        loop :: Package -> Data.ProtoLens.Encoding.Bytes.Parser Package
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "manager"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"manager") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "version"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"version") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Package"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"manager") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8 _v))
                ((Data.Monoid.<>)
                   (let
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"version") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8 _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData Package where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Package'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Package'manager x__)
                (Control.DeepSeq.deepseq
                   (_Package'name x__)
                   (Control.DeepSeq.deepseq (_Package'version x__) ())))
newtype PositionEncoding'UnrecognizedValue
  = PositionEncoding'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data PositionEncoding
  = UnspecifiedPositionEncoding |
    UTF8CodeUnitOffsetFromLineStart |
    UTF16CodeUnitOffsetFromLineStart |
    UTF32CodeUnitOffsetFromLineStart |
    PositionEncoding'Unrecognized !PositionEncoding'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum PositionEncoding where
  maybeToEnum 0 = Prelude.Just UnspecifiedPositionEncoding
  maybeToEnum 1 = Prelude.Just UTF8CodeUnitOffsetFromLineStart
  maybeToEnum 2 = Prelude.Just UTF16CodeUnitOffsetFromLineStart
  maybeToEnum 3 = Prelude.Just UTF32CodeUnitOffsetFromLineStart
  maybeToEnum k
    = Prelude.Just
        (PositionEncoding'Unrecognized
           (PositionEncoding'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum UnspecifiedPositionEncoding
    = "UnspecifiedPositionEncoding"
  showEnum UTF8CodeUnitOffsetFromLineStart
    = "UTF8CodeUnitOffsetFromLineStart"
  showEnum UTF16CodeUnitOffsetFromLineStart
    = "UTF16CodeUnitOffsetFromLineStart"
  showEnum UTF32CodeUnitOffsetFromLineStart
    = "UTF32CodeUnitOffsetFromLineStart"
  showEnum
    (PositionEncoding'Unrecognized (PositionEncoding'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "UnspecifiedPositionEncoding"
    = Prelude.Just UnspecifiedPositionEncoding
    | (Prelude.==) k "UTF8CodeUnitOffsetFromLineStart"
    = Prelude.Just UTF8CodeUnitOffsetFromLineStart
    | (Prelude.==) k "UTF16CodeUnitOffsetFromLineStart"
    = Prelude.Just UTF16CodeUnitOffsetFromLineStart
    | (Prelude.==) k "UTF32CodeUnitOffsetFromLineStart"
    = Prelude.Just UTF32CodeUnitOffsetFromLineStart
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded PositionEncoding where
  minBound = UnspecifiedPositionEncoding
  maxBound = UTF32CodeUnitOffsetFromLineStart
instance Prelude.Enum PositionEncoding where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum PositionEncoding: "
              (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum UnspecifiedPositionEncoding = 0
  fromEnum UTF8CodeUnitOffsetFromLineStart = 1
  fromEnum UTF16CodeUnitOffsetFromLineStart = 2
  fromEnum UTF32CodeUnitOffsetFromLineStart = 3
  fromEnum
    (PositionEncoding'Unrecognized (PositionEncoding'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ UTF32CodeUnitOffsetFromLineStart
    = Prelude.error
        "PositionEncoding.succ: bad argument UTF32CodeUnitOffsetFromLineStart. This value would be out of bounds."
  succ UnspecifiedPositionEncoding = UTF8CodeUnitOffsetFromLineStart
  succ UTF8CodeUnitOffsetFromLineStart
    = UTF16CodeUnitOffsetFromLineStart
  succ UTF16CodeUnitOffsetFromLineStart
    = UTF32CodeUnitOffsetFromLineStart
  succ (PositionEncoding'Unrecognized _)
    = Prelude.error
        "PositionEncoding.succ: bad argument: unrecognized value"
  pred UnspecifiedPositionEncoding
    = Prelude.error
        "PositionEncoding.pred: bad argument UnspecifiedPositionEncoding. This value would be out of bounds."
  pred UTF8CodeUnitOffsetFromLineStart = UnspecifiedPositionEncoding
  pred UTF16CodeUnitOffsetFromLineStart
    = UTF8CodeUnitOffsetFromLineStart
  pred UTF32CodeUnitOffsetFromLineStart
    = UTF16CodeUnitOffsetFromLineStart
  pred (PositionEncoding'Unrecognized _)
    = Prelude.error
        "PositionEncoding.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault PositionEncoding where
  fieldDefault = UnspecifiedPositionEncoding
instance Control.DeepSeq.NFData PositionEncoding where
  rnf x__ = Prelude.seq x__ ()
newtype ProtocolVersion'UnrecognizedValue
  = ProtocolVersion'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data ProtocolVersion
  = UnspecifiedProtocolVersion |
    ProtocolVersion'Unrecognized !ProtocolVersion'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum ProtocolVersion where
  maybeToEnum 0 = Prelude.Just UnspecifiedProtocolVersion
  maybeToEnum k
    = Prelude.Just
        (ProtocolVersion'Unrecognized
           (ProtocolVersion'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum UnspecifiedProtocolVersion = "UnspecifiedProtocolVersion"
  showEnum
    (ProtocolVersion'Unrecognized (ProtocolVersion'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "UnspecifiedProtocolVersion"
    = Prelude.Just UnspecifiedProtocolVersion
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded ProtocolVersion where
  minBound = UnspecifiedProtocolVersion
  maxBound = UnspecifiedProtocolVersion
instance Prelude.Enum ProtocolVersion where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum ProtocolVersion: "
              (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum UnspecifiedProtocolVersion = 0
  fromEnum
    (ProtocolVersion'Unrecognized (ProtocolVersion'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ UnspecifiedProtocolVersion
    = Prelude.error
        "ProtocolVersion.succ: bad argument UnspecifiedProtocolVersion. This value would be out of bounds."
  succ (ProtocolVersion'Unrecognized _)
    = Prelude.error
        "ProtocolVersion.succ: bad argument: unrecognized value"
  pred UnspecifiedProtocolVersion
    = Prelude.error
        "ProtocolVersion.pred: bad argument UnspecifiedProtocolVersion. This value would be out of bounds."
  pred (ProtocolVersion'Unrecognized _)
    = Prelude.error
        "ProtocolVersion.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault ProtocolVersion where
  fieldDefault = UnspecifiedProtocolVersion
instance Control.DeepSeq.NFData ProtocolVersion where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :

         * 'Proto.Scip_Fields.symbol' @:: Lens' Relationship Data.Text.Text@
         * 'Proto.Scip_Fields.isReference' @:: Lens' Relationship Prelude.Bool@
         * 'Proto.Scip_Fields.isImplementation' @:: Lens' Relationship Prelude.Bool@
         * 'Proto.Scip_Fields.isTypeDefinition' @:: Lens' Relationship Prelude.Bool@
         * 'Proto.Scip_Fields.isDefinition' @:: Lens' Relationship Prelude.Bool@ -}
data Relationship
  = Relationship'_constructor {_Relationship'symbol :: !Data.Text.Text,
                               _Relationship'isReference :: !Prelude.Bool,
                               _Relationship'isImplementation :: !Prelude.Bool,
                               _Relationship'isTypeDefinition :: !Prelude.Bool,
                               _Relationship'isDefinition :: !Prelude.Bool,
                               _Relationship'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Relationship where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Relationship "symbol" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Relationship'symbol
           (\ x__ y__ -> x__ {_Relationship'symbol = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Relationship "isReference" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Relationship'isReference
           (\ x__ y__ -> x__ {_Relationship'isReference = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Relationship "isImplementation" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Relationship'isImplementation
           (\ x__ y__ -> x__ {_Relationship'isImplementation = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Relationship "isTypeDefinition" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Relationship'isTypeDefinition
           (\ x__ y__ -> x__ {_Relationship'isTypeDefinition = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Relationship "isDefinition" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Relationship'isDefinition
           (\ x__ y__ -> x__ {_Relationship'isDefinition = y__}))
        Prelude.id
instance Data.ProtoLens.Message Relationship where
  messageName _ = Data.Text.pack "scip.Relationship"
  packedMessageDescriptor _
    = "\n\
      \\fRelationship\DC2\SYN\n\
      \\ACKsymbol\CAN\SOH \SOH(\tR\ACKsymbol\DC2!\n\
      \\fis_reference\CAN\STX \SOH(\bR\visReference\DC2+\n\
      \\DC1is_implementation\CAN\ETX \SOH(\bR\DLEisImplementation\DC2,\n\
      \\DC2is_type_definition\CAN\EOT \SOH(\bR\DLEisTypeDefinition\DC2#\n\
      \\ris_definition\CAN\ENQ \SOH(\bR\fisDefinition"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        symbol__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "symbol"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"symbol")) ::
              Data.ProtoLens.FieldDescriptor Relationship
        isReference__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "is_reference"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"isReference")) ::
              Data.ProtoLens.FieldDescriptor Relationship
        isImplementation__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "is_implementation"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"isImplementation")) ::
              Data.ProtoLens.FieldDescriptor Relationship
        isTypeDefinition__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "is_type_definition"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"isTypeDefinition")) ::
              Data.ProtoLens.FieldDescriptor Relationship
        isDefinition__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "is_definition"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"isDefinition")) ::
              Data.ProtoLens.FieldDescriptor Relationship
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, symbol__field_descriptor),
           (Data.ProtoLens.Tag 2, isReference__field_descriptor),
           (Data.ProtoLens.Tag 3, isImplementation__field_descriptor),
           (Data.ProtoLens.Tag 4, isTypeDefinition__field_descriptor),
           (Data.ProtoLens.Tag 5, isDefinition__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Relationship'_unknownFields
        (\ x__ y__ -> x__ {_Relationship'_unknownFields = y__})
  defMessage
    = Relationship'_constructor
        {_Relationship'symbol = Data.ProtoLens.fieldDefault,
         _Relationship'isReference = Data.ProtoLens.fieldDefault,
         _Relationship'isImplementation = Data.ProtoLens.fieldDefault,
         _Relationship'isTypeDefinition = Data.ProtoLens.fieldDefault,
         _Relationship'isDefinition = Data.ProtoLens.fieldDefault,
         _Relationship'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Relationship -> Data.ProtoLens.Encoding.Bytes.Parser Relationship
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "symbol"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"symbol") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "is_reference"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"isReference") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "is_implementation"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"isImplementation") y x)
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "is_type_definition"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"isTypeDefinition") y x)
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "is_definition"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"isDefinition") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Relationship"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"symbol") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view (Data.ProtoLens.Field.field @"isReference") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt (\ b -> if b then 1 else 0)
                            _v))
                ((Data.Monoid.<>)
                   (let
                      _v
                        = Lens.Family2.view
                            (Data.ProtoLens.Field.field @"isImplementation") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt (\ b -> if b then 1 else 0)
                               _v))
                   ((Data.Monoid.<>)
                      (let
                         _v
                           = Lens.Family2.view
                               (Data.ProtoLens.Field.field @"isTypeDefinition") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 32)
                               ((Prelude..)
                                  Data.ProtoLens.Encoding.Bytes.putVarInt
                                  (\ b -> if b then 1 else 0) _v))
                      ((Data.Monoid.<>)
                         (let
                            _v
                              = Lens.Family2.view (Data.ProtoLens.Field.field @"isDefinition") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (\ b -> if b then 1 else 0) _v))
                         (Data.ProtoLens.Encoding.Wire.buildFieldSet
                            (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))
instance Control.DeepSeq.NFData Relationship where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Relationship'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Relationship'symbol x__)
                (Control.DeepSeq.deepseq
                   (_Relationship'isReference x__)
                   (Control.DeepSeq.deepseq
                      (_Relationship'isImplementation x__)
                      (Control.DeepSeq.deepseq
                         (_Relationship'isTypeDefinition x__)
                         (Control.DeepSeq.deepseq (_Relationship'isDefinition x__) ())))))
newtype Severity'UnrecognizedValue
  = Severity'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data Severity
  = UnspecifiedSeverity |
    Error |
    Warning |
    Information |
    Hint |
    Severity'Unrecognized !Severity'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum Severity where
  maybeToEnum 0 = Prelude.Just UnspecifiedSeverity
  maybeToEnum 1 = Prelude.Just Error
  maybeToEnum 2 = Prelude.Just Warning
  maybeToEnum 3 = Prelude.Just Information
  maybeToEnum 4 = Prelude.Just Hint
  maybeToEnum k
    = Prelude.Just
        (Severity'Unrecognized
           (Severity'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum UnspecifiedSeverity = "UnspecifiedSeverity"
  showEnum Error = "Error"
  showEnum Warning = "Warning"
  showEnum Information = "Information"
  showEnum Hint = "Hint"
  showEnum (Severity'Unrecognized (Severity'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "UnspecifiedSeverity"
    = Prelude.Just UnspecifiedSeverity
    | (Prelude.==) k "Error" = Prelude.Just Error
    | (Prelude.==) k "Warning" = Prelude.Just Warning
    | (Prelude.==) k "Information" = Prelude.Just Information
    | (Prelude.==) k "Hint" = Prelude.Just Hint
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded Severity where
  minBound = UnspecifiedSeverity
  maxBound = Hint
instance Prelude.Enum Severity where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum Severity: " (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum UnspecifiedSeverity = 0
  fromEnum Error = 1
  fromEnum Warning = 2
  fromEnum Information = 3
  fromEnum Hint = 4
  fromEnum (Severity'Unrecognized (Severity'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ Hint
    = Prelude.error
        "Severity.succ: bad argument Hint. This value would be out of bounds."
  succ UnspecifiedSeverity = Error
  succ Error = Warning
  succ Warning = Information
  succ Information = Hint
  succ (Severity'Unrecognized _)
    = Prelude.error "Severity.succ: bad argument: unrecognized value"
  pred UnspecifiedSeverity
    = Prelude.error
        "Severity.pred: bad argument UnspecifiedSeverity. This value would be out of bounds."
  pred Error = UnspecifiedSeverity
  pred Warning = Error
  pred Information = Warning
  pred Hint = Information
  pred (Severity'Unrecognized _)
    = Prelude.error "Severity.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault Severity where
  fieldDefault = UnspecifiedSeverity
instance Control.DeepSeq.NFData Severity where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :

         * 'Proto.Scip_Fields.scheme' @:: Lens' Symbol Data.Text.Text@
         * 'Proto.Scip_Fields.package' @:: Lens' Symbol Package@
         * 'Proto.Scip_Fields.maybe'package' @:: Lens' Symbol (Prelude.Maybe Package)@
         * 'Proto.Scip_Fields.descriptors' @:: Lens' Symbol [Descriptor]@
         * 'Proto.Scip_Fields.vec'descriptors' @:: Lens' Symbol (Data.Vector.Vector Descriptor)@ -}
data Symbol
  = Symbol'_constructor {_Symbol'scheme :: !Data.Text.Text,
                         _Symbol'package :: !(Prelude.Maybe Package),
                         _Symbol'descriptors :: !(Data.Vector.Vector Descriptor),
                         _Symbol'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Symbol where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Symbol "scheme" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Symbol'scheme (\ x__ y__ -> x__ {_Symbol'scheme = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Symbol "package" Package where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Symbol'package (\ x__ y__ -> x__ {_Symbol'package = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Symbol "maybe'package" (Prelude.Maybe Package) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Symbol'package (\ x__ y__ -> x__ {_Symbol'package = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Symbol "descriptors" [Descriptor] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Symbol'descriptors (\ x__ y__ -> x__ {_Symbol'descriptors = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Symbol "vec'descriptors" (Data.Vector.Vector Descriptor) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Symbol'descriptors (\ x__ y__ -> x__ {_Symbol'descriptors = y__}))
        Prelude.id
instance Data.ProtoLens.Message Symbol where
  messageName _ = Data.Text.pack "scip.Symbol"
  packedMessageDescriptor _
    = "\n\
      \\ACKSymbol\DC2\SYN\n\
      \\ACKscheme\CAN\SOH \SOH(\tR\ACKscheme\DC2'\n\
      \\apackage\CAN\STX \SOH(\v2\r.scip.PackageR\apackage\DC22\n\
      \\vdescriptors\CAN\ETX \ETX(\v2\DLE.scip.DescriptorR\vdescriptors"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        scheme__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "scheme"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"scheme")) ::
              Data.ProtoLens.FieldDescriptor Symbol
        package__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "package"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Package)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'package")) ::
              Data.ProtoLens.FieldDescriptor Symbol
        descriptors__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "descriptors"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Descriptor)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"descriptors")) ::
              Data.ProtoLens.FieldDescriptor Symbol
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, scheme__field_descriptor),
           (Data.ProtoLens.Tag 2, package__field_descriptor),
           (Data.ProtoLens.Tag 3, descriptors__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Symbol'_unknownFields
        (\ x__ y__ -> x__ {_Symbol'_unknownFields = y__})
  defMessage
    = Symbol'_constructor
        {_Symbol'scheme = Data.ProtoLens.fieldDefault,
         _Symbol'package = Prelude.Nothing,
         _Symbol'descriptors = Data.Vector.Generic.empty,
         _Symbol'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Symbol
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Descriptor
             -> Data.ProtoLens.Encoding.Bytes.Parser Symbol
        loop x mutable'descriptors
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'descriptors <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                              (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                 mutable'descriptors)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'descriptors") frozen'descriptors
                              x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "scheme"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"scheme") y x)
                                  mutable'descriptors
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "package"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"package") y x)
                                  mutable'descriptors
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "descriptors"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'descriptors y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'descriptors
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'descriptors <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'descriptors)
          "Symbol"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"scheme") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'package") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage _v))
                      (Lens.Family2.view
                         (Data.ProtoLens.Field.field @"vec'descriptors") _x))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData Symbol where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Symbol'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Symbol'scheme x__)
                (Control.DeepSeq.deepseq
                   (_Symbol'package x__)
                   (Control.DeepSeq.deepseq (_Symbol'descriptors x__) ())))
{- | Fields :

         * 'Proto.Scip_Fields.symbol' @:: Lens' SymbolInformation Data.Text.Text@
         * 'Proto.Scip_Fields.documentation' @:: Lens' SymbolInformation [Data.Text.Text]@
         * 'Proto.Scip_Fields.vec'documentation' @:: Lens' SymbolInformation (Data.Vector.Vector Data.Text.Text)@
         * 'Proto.Scip_Fields.relationships' @:: Lens' SymbolInformation [Relationship]@
         * 'Proto.Scip_Fields.vec'relationships' @:: Lens' SymbolInformation (Data.Vector.Vector Relationship)@
         * 'Proto.Scip_Fields.kind' @:: Lens' SymbolInformation SymbolInformation'Kind@
         * 'Proto.Scip_Fields.displayName' @:: Lens' SymbolInformation Data.Text.Text@
         * 'Proto.Scip_Fields.signatureDocumentation' @:: Lens' SymbolInformation Document@
         * 'Proto.Scip_Fields.maybe'signatureDocumentation' @:: Lens' SymbolInformation (Prelude.Maybe Document)@
         * 'Proto.Scip_Fields.enclosingSymbol' @:: Lens' SymbolInformation Data.Text.Text@ -}
data SymbolInformation
  = SymbolInformation'_constructor {_SymbolInformation'symbol :: !Data.Text.Text,
                                    _SymbolInformation'documentation :: !(Data.Vector.Vector Data.Text.Text),
                                    _SymbolInformation'relationships :: !(Data.Vector.Vector Relationship),
                                    _SymbolInformation'kind :: !SymbolInformation'Kind,
                                    _SymbolInformation'displayName :: !Data.Text.Text,
                                    _SymbolInformation'signatureDocumentation :: !(Prelude.Maybe Document),
                                    _SymbolInformation'enclosingSymbol :: !Data.Text.Text,
                                    _SymbolInformation'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SymbolInformation where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SymbolInformation "symbol" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SymbolInformation'symbol
           (\ x__ y__ -> x__ {_SymbolInformation'symbol = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SymbolInformation "documentation" [Data.Text.Text] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SymbolInformation'documentation
           (\ x__ y__ -> x__ {_SymbolInformation'documentation = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField SymbolInformation "vec'documentation" (Data.Vector.Vector Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SymbolInformation'documentation
           (\ x__ y__ -> x__ {_SymbolInformation'documentation = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SymbolInformation "relationships" [Relationship] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SymbolInformation'relationships
           (\ x__ y__ -> x__ {_SymbolInformation'relationships = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField SymbolInformation "vec'relationships" (Data.Vector.Vector Relationship) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SymbolInformation'relationships
           (\ x__ y__ -> x__ {_SymbolInformation'relationships = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SymbolInformation "kind" SymbolInformation'Kind where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SymbolInformation'kind
           (\ x__ y__ -> x__ {_SymbolInformation'kind = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SymbolInformation "displayName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SymbolInformation'displayName
           (\ x__ y__ -> x__ {_SymbolInformation'displayName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SymbolInformation "signatureDocumentation" Document where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SymbolInformation'signatureDocumentation
           (\ x__ y__
              -> x__ {_SymbolInformation'signatureDocumentation = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField SymbolInformation "maybe'signatureDocumentation" (Prelude.Maybe Document) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SymbolInformation'signatureDocumentation
           (\ x__ y__
              -> x__ {_SymbolInformation'signatureDocumentation = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SymbolInformation "enclosingSymbol" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SymbolInformation'enclosingSymbol
           (\ x__ y__ -> x__ {_SymbolInformation'enclosingSymbol = y__}))
        Prelude.id
instance Data.ProtoLens.Message SymbolInformation where
  messageName _ = Data.Text.pack "scip.SymbolInformation"
  packedMessageDescriptor _
    = "\n\
      \\DC1SymbolInformation\DC2\SYN\n\
      \\ACKsymbol\CAN\SOH \SOH(\tR\ACKsymbol\DC2$\n\
      \\rdocumentation\CAN\ETX \ETX(\tR\rdocumentation\DC28\n\
      \\rrelationships\CAN\EOT \ETX(\v2\DC2.scip.RelationshipR\rrelationships\DC20\n\
      \\EOTkind\CAN\ENQ \SOH(\SO2\FS.scip.SymbolInformation.KindR\EOTkind\DC2!\n\
      \\fdisplay_name\CAN\ACK \SOH(\tR\vdisplayName\DC2G\n\
      \\ETBsignature_documentation\CAN\a \SOH(\v2\SO.scip.DocumentR\SYNsignatureDocumentation\DC2)\n\
      \\DLEenclosing_symbol\CAN\b \SOH(\tR\SIenclosingSymbol\"\251\t\n\
      \\EOTKind\DC2\DC3\n\
      \\SIUnspecifiedKind\DLE\NUL\DC2\DC2\n\
      \\SOAbstractMethod\DLEB\DC2\f\n\
      \\bAccessor\DLEH\DC2\t\n\
      \\ENQArray\DLE\SOH\DC2\r\n\
      \\tAssertion\DLE\STX\DC2\DC2\n\
      \\SOAssociatedType\DLE\ETX\DC2\r\n\
      \\tAttribute\DLE\EOT\DC2\t\n\
      \\ENQAxiom\DLE\ENQ\DC2\v\n\
      \\aBoolean\DLE\ACK\DC2\t\n\
      \\ENQClass\DLE\a\DC2\v\n\
      \\aConcept\DLEV\DC2\f\n\
      \\bConstant\DLE\b\DC2\SI\n\
      \\vConstructor\DLE\t\DC2\f\n\
      \\bContract\DLE>\DC2\SO\n\
      \\n\
      \DataFamily\DLE\n\
      \\DC2\f\n\
      \\bDelegate\DLEI\DC2\b\n\
      \\EOTEnum\DLE\v\DC2\SO\n\
      \\n\
      \EnumMember\DLE\f\DC2\t\n\
      \\ENQError\DLE?\DC2\t\n\
      \\ENQEvent\DLE\r\DC2\r\n\
      \\tExtension\DLET\DC2\b\n\
      \\EOTFact\DLE\SO\DC2\t\n\
      \\ENQField\DLE\SI\DC2\b\n\
      \\EOTFile\DLE\DLE\DC2\f\n\
      \\bFunction\DLE\DC1\DC2\n\
      \\n\
      \\ACKGetter\DLE\DC2\DC2\v\n\
      \\aGrammar\DLE\DC3\DC2\f\n\
      \\bInstance\DLE\DC4\DC2\r\n\
      \\tInterface\DLE\NAK\DC2\a\n\
      \\ETXKey\DLE\SYN\DC2\b\n\
      \\EOTLang\DLE\ETB\DC2\t\n\
      \\ENQLemma\DLE\CAN\DC2\v\n\
      \\aLibrary\DLE@\DC2\t\n\
      \\ENQMacro\DLE\EM\DC2\n\
      \\n\
      \\ACKMethod\DLE\SUB\DC2\SI\n\
      \\vMethodAlias\DLEJ\DC2\DC2\n\
      \\SOMethodReceiver\DLE\ESC\DC2\ETB\n\
      \\DC3MethodSpecification\DLEC\DC2\v\n\
      \\aMessage\DLE\FS\DC2\t\n\
      \\ENQMixin\DLEU\DC2\f\n\
      \\bModifier\DLEA\DC2\n\
      \\n\
      \\ACKModule\DLE\GS\DC2\r\n\
      \\tNamespace\DLE\RS\DC2\b\n\
      \\EOTNull\DLE\US\DC2\n\
      \\n\
      \\ACKNumber\DLE \DC2\n\
      \\n\
      \\ACKObject\DLE!\DC2\f\n\
      \\bOperator\DLE\"\DC2\v\n\
      \\aPackage\DLE#\DC2\DC1\n\
      \\rPackageObject\DLE$\DC2\r\n\
      \\tParameter\DLE%\DC2\DC2\n\
      \\SOParameterLabel\DLE&\DC2\v\n\
      \\aPattern\DLE'\DC2\r\n\
      \\tPredicate\DLE(\DC2\f\n\
      \\bProperty\DLE)\DC2\f\n\
      \\bProtocol\DLE*\DC2\DC2\n\
      \\SOProtocolMethod\DLED\DC2\NAK\n\
      \\DC1PureVirtualMethod\DLEE\DC2\SI\n\
      \\vQuasiquoter\DLE+\DC2\DC1\n\
      \\rSelfParameter\DLE,\DC2\n\
      \\n\
      \\ACKSetter\DLE-\DC2\r\n\
      \\tSignature\DLE.\DC2\DC2\n\
      \\SOSingletonClass\DLEK\DC2\DC3\n\
      \\SISingletonMethod\DLEL\DC2\DC4\n\
      \\DLEStaticDataMember\DLEM\DC2\SI\n\
      \\vStaticEvent\DLEN\DC2\SI\n\
      \\vStaticField\DLEO\DC2\DLE\n\
      \\fStaticMethod\DLEP\DC2\DC2\n\
      \\SOStaticProperty\DLEQ\DC2\DC2\n\
      \\SOStaticVariable\DLER\DC2\n\
      \\n\
      \\ACKString\DLE0\DC2\n\
      \\n\
      \\ACKStruct\DLE1\DC2\r\n\
      \\tSubscript\DLE/\DC2\n\
      \\n\
      \\ACKTactic\DLE2\DC2\v\n\
      \\aTheorem\DLE3\DC2\DC1\n\
      \\rThisParameter\DLE4\DC2\t\n\
      \\ENQTrait\DLE5\DC2\SI\n\
      \\vTraitMethod\DLEF\DC2\b\n\
      \\EOTType\DLE6\DC2\r\n\
      \\tTypeAlias\DLE7\DC2\r\n\
      \\tTypeClass\DLE8\DC2\DC3\n\
      \\SITypeClassMethod\DLEG\DC2\SO\n\
      \\n\
      \TypeFamily\DLE9\DC2\DC1\n\
      \\rTypeParameter\DLE:\DC2\t\n\
      \\ENQUnion\DLE;\DC2\t\n\
      \\ENQValue\DLE<\DC2\f\n\
      \\bVariable\DLE="
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        symbol__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "symbol"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"symbol")) ::
              Data.ProtoLens.FieldDescriptor SymbolInformation
        documentation__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "documentation"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"documentation")) ::
              Data.ProtoLens.FieldDescriptor SymbolInformation
        relationships__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "relationships"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Relationship)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"relationships")) ::
              Data.ProtoLens.FieldDescriptor SymbolInformation
        kind__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "kind"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor SymbolInformation'Kind)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"kind")) ::
              Data.ProtoLens.FieldDescriptor SymbolInformation
        displayName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "display_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"displayName")) ::
              Data.ProtoLens.FieldDescriptor SymbolInformation
        signatureDocumentation__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "signature_documentation"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Document)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'signatureDocumentation")) ::
              Data.ProtoLens.FieldDescriptor SymbolInformation
        enclosingSymbol__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "enclosing_symbol"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"enclosingSymbol")) ::
              Data.ProtoLens.FieldDescriptor SymbolInformation
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, symbol__field_descriptor),
           (Data.ProtoLens.Tag 3, documentation__field_descriptor),
           (Data.ProtoLens.Tag 4, relationships__field_descriptor),
           (Data.ProtoLens.Tag 5, kind__field_descriptor),
           (Data.ProtoLens.Tag 6, displayName__field_descriptor),
           (Data.ProtoLens.Tag 7, signatureDocumentation__field_descriptor),
           (Data.ProtoLens.Tag 8, enclosingSymbol__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SymbolInformation'_unknownFields
        (\ x__ y__ -> x__ {_SymbolInformation'_unknownFields = y__})
  defMessage
    = SymbolInformation'_constructor
        {_SymbolInformation'symbol = Data.ProtoLens.fieldDefault,
         _SymbolInformation'documentation = Data.Vector.Generic.empty,
         _SymbolInformation'relationships = Data.Vector.Generic.empty,
         _SymbolInformation'kind = Data.ProtoLens.fieldDefault,
         _SymbolInformation'displayName = Data.ProtoLens.fieldDefault,
         _SymbolInformation'signatureDocumentation = Prelude.Nothing,
         _SymbolInformation'enclosingSymbol = Data.ProtoLens.fieldDefault,
         _SymbolInformation'_unknownFields = []}
  parseMessage
    = let
        loop ::
          SymbolInformation
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Text.Text
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Relationship
                -> Data.ProtoLens.Encoding.Bytes.Parser SymbolInformation
        loop x mutable'documentation mutable'relationships
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'documentation <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                   mutable'documentation)
                      frozen'relationships <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                   mutable'relationships)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'documentation")
                              frozen'documentation
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'relationships")
                                 frozen'relationships x)))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "symbol"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"symbol") y x)
                                  mutable'documentation mutable'relationships
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                        Data.ProtoLens.Encoding.Bytes.getBytes
                                                          (Prelude.fromIntegral len)
                                            Data.ProtoLens.Encoding.Bytes.runEither
                                              (case Data.Text.Encoding.decodeUtf8' value of
                                                 (Prelude.Left err)
                                                   -> Prelude.Left (Prelude.show err)
                                                 (Prelude.Right r) -> Prelude.Right r))
                                        "documentation"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'documentation y)
                                loop x v mutable'relationships
                        34
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "relationships"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'relationships y)
                                loop x mutable'documentation v
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "kind"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"kind") y x)
                                  mutable'documentation mutable'relationships
                        50
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "display_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"displayName") y x)
                                  mutable'documentation mutable'relationships
                        58
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "signature_documentation"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"signatureDocumentation") y x)
                                  mutable'documentation mutable'relationships
                        66
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "enclosing_symbol"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"enclosingSymbol") y x)
                                  mutable'documentation mutable'relationships
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'documentation mutable'relationships
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'documentation <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         Data.ProtoLens.Encoding.Growing.new
              mutable'relationships <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         Data.ProtoLens.Encoding.Growing.new
              loop
                Data.ProtoLens.defMessage mutable'documentation
                mutable'relationships)
          "SymbolInformation"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"symbol") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                   (\ _v
                      -> (Data.Monoid.<>)
                           (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                           ((Prelude..)
                              (\ bs
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Data.Text.Encoding.encodeUtf8 _v))
                   (Lens.Family2.view
                      (Data.ProtoLens.Field.field @"vec'documentation") _x))
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage _v))
                      (Lens.Family2.view
                         (Data.ProtoLens.Field.field @"vec'relationships") _x))
                   ((Data.Monoid.<>)
                      (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"kind") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                               ((Prelude..)
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                                  Prelude.fromEnum _v))
                      ((Data.Monoid.<>)
                         (let
                            _v
                              = Lens.Family2.view (Data.ProtoLens.Field.field @"displayName") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 50)
                                  ((Prelude..)
                                     (\ bs
                                        -> (Data.Monoid.<>)
                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                (Prelude.fromIntegral (Data.ByteString.length bs)))
                                             (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                     Data.Text.Encoding.encodeUtf8 _v))
                         ((Data.Monoid.<>)
                            (case
                                 Lens.Family2.view
                                   (Data.ProtoLens.Field.field @"maybe'signatureDocumentation") _x
                             of
                               Prelude.Nothing -> Data.Monoid.mempty
                               (Prelude.Just _v)
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt 58)
                                      ((Prelude..)
                                         (\ bs
                                            -> (Data.Monoid.<>)
                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                    (Prelude.fromIntegral
                                                       (Data.ByteString.length bs)))
                                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                         Data.ProtoLens.encodeMessage _v))
                            ((Data.Monoid.<>)
                               (let
                                  _v
                                    = Lens.Family2.view
                                        (Data.ProtoLens.Field.field @"enclosingSymbol") _x
                                in
                                  if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                      Data.Monoid.mempty
                                  else
                                      (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt 66)
                                        ((Prelude..)
                                           (\ bs
                                              -> (Data.Monoid.<>)
                                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                      (Prelude.fromIntegral
                                                         (Data.ByteString.length bs)))
                                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                           Data.Text.Encoding.encodeUtf8 _v))
                               (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                  (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))))
instance Control.DeepSeq.NFData SymbolInformation where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_SymbolInformation'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_SymbolInformation'symbol x__)
                (Control.DeepSeq.deepseq
                   (_SymbolInformation'documentation x__)
                   (Control.DeepSeq.deepseq
                      (_SymbolInformation'relationships x__)
                      (Control.DeepSeq.deepseq
                         (_SymbolInformation'kind x__)
                         (Control.DeepSeq.deepseq
                            (_SymbolInformation'displayName x__)
                            (Control.DeepSeq.deepseq
                               (_SymbolInformation'signatureDocumentation x__)
                               (Control.DeepSeq.deepseq
                                  (_SymbolInformation'enclosingSymbol x__) ())))))))
newtype SymbolInformation'Kind'UnrecognizedValue
  = SymbolInformation'Kind'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data SymbolInformation'Kind
  = SymbolInformation'UnspecifiedKind |
    SymbolInformation'Array |
    SymbolInformation'Assertion |
    SymbolInformation'AssociatedType |
    SymbolInformation'Attribute |
    SymbolInformation'Axiom |
    SymbolInformation'Boolean |
    SymbolInformation'Class |
    SymbolInformation'Constant |
    SymbolInformation'Constructor |
    SymbolInformation'DataFamily |
    SymbolInformation'Enum |
    SymbolInformation'EnumMember |
    SymbolInformation'Event |
    SymbolInformation'Fact |
    SymbolInformation'Field |
    SymbolInformation'File |
    SymbolInformation'Function |
    SymbolInformation'Getter |
    SymbolInformation'Grammar |
    SymbolInformation'Instance |
    SymbolInformation'Interface |
    SymbolInformation'Key |
    SymbolInformation'Lang |
    SymbolInformation'Lemma |
    SymbolInformation'Macro |
    SymbolInformation'Method |
    SymbolInformation'MethodReceiver |
    SymbolInformation'Message |
    SymbolInformation'Module |
    SymbolInformation'Namespace |
    SymbolInformation'Null |
    SymbolInformation'Number |
    SymbolInformation'Object |
    SymbolInformation'Operator |
    SymbolInformation'Package |
    SymbolInformation'PackageObject |
    SymbolInformation'Parameter |
    SymbolInformation'ParameterLabel |
    SymbolInformation'Pattern |
    SymbolInformation'Predicate |
    SymbolInformation'Property |
    SymbolInformation'Protocol |
    SymbolInformation'Quasiquoter |
    SymbolInformation'SelfParameter |
    SymbolInformation'Setter |
    SymbolInformation'Signature |
    SymbolInformation'Subscript |
    SymbolInformation'String |
    SymbolInformation'Struct |
    SymbolInformation'Tactic |
    SymbolInformation'Theorem |
    SymbolInformation'ThisParameter |
    SymbolInformation'Trait |
    SymbolInformation'Type |
    SymbolInformation'TypeAlias |
    SymbolInformation'TypeClass |
    SymbolInformation'TypeFamily |
    SymbolInformation'TypeParameter |
    SymbolInformation'Union |
    SymbolInformation'Value |
    SymbolInformation'Variable |
    SymbolInformation'Contract |
    SymbolInformation'Error |
    SymbolInformation'Library |
    SymbolInformation'Modifier |
    SymbolInformation'AbstractMethod |
    SymbolInformation'MethodSpecification |
    SymbolInformation'ProtocolMethod |
    SymbolInformation'PureVirtualMethod |
    SymbolInformation'TraitMethod |
    SymbolInformation'TypeClassMethod |
    SymbolInformation'Accessor |
    SymbolInformation'Delegate |
    SymbolInformation'MethodAlias |
    SymbolInformation'SingletonClass |
    SymbolInformation'SingletonMethod |
    SymbolInformation'StaticDataMember |
    SymbolInformation'StaticEvent |
    SymbolInformation'StaticField |
    SymbolInformation'StaticMethod |
    SymbolInformation'StaticProperty |
    SymbolInformation'StaticVariable |
    SymbolInformation'Extension |
    SymbolInformation'Mixin |
    SymbolInformation'Concept |
    SymbolInformation'Kind'Unrecognized !SymbolInformation'Kind'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum SymbolInformation'Kind where
  maybeToEnum 0 = Prelude.Just SymbolInformation'UnspecifiedKind
  maybeToEnum 1 = Prelude.Just SymbolInformation'Array
  maybeToEnum 2 = Prelude.Just SymbolInformation'Assertion
  maybeToEnum 3 = Prelude.Just SymbolInformation'AssociatedType
  maybeToEnum 4 = Prelude.Just SymbolInformation'Attribute
  maybeToEnum 5 = Prelude.Just SymbolInformation'Axiom
  maybeToEnum 6 = Prelude.Just SymbolInformation'Boolean
  maybeToEnum 7 = Prelude.Just SymbolInformation'Class
  maybeToEnum 8 = Prelude.Just SymbolInformation'Constant
  maybeToEnum 9 = Prelude.Just SymbolInformation'Constructor
  maybeToEnum 10 = Prelude.Just SymbolInformation'DataFamily
  maybeToEnum 11 = Prelude.Just SymbolInformation'Enum
  maybeToEnum 12 = Prelude.Just SymbolInformation'EnumMember
  maybeToEnum 13 = Prelude.Just SymbolInformation'Event
  maybeToEnum 14 = Prelude.Just SymbolInformation'Fact
  maybeToEnum 15 = Prelude.Just SymbolInformation'Field
  maybeToEnum 16 = Prelude.Just SymbolInformation'File
  maybeToEnum 17 = Prelude.Just SymbolInformation'Function
  maybeToEnum 18 = Prelude.Just SymbolInformation'Getter
  maybeToEnum 19 = Prelude.Just SymbolInformation'Grammar
  maybeToEnum 20 = Prelude.Just SymbolInformation'Instance
  maybeToEnum 21 = Prelude.Just SymbolInformation'Interface
  maybeToEnum 22 = Prelude.Just SymbolInformation'Key
  maybeToEnum 23 = Prelude.Just SymbolInformation'Lang
  maybeToEnum 24 = Prelude.Just SymbolInformation'Lemma
  maybeToEnum 25 = Prelude.Just SymbolInformation'Macro
  maybeToEnum 26 = Prelude.Just SymbolInformation'Method
  maybeToEnum 27 = Prelude.Just SymbolInformation'MethodReceiver
  maybeToEnum 28 = Prelude.Just SymbolInformation'Message
  maybeToEnum 29 = Prelude.Just SymbolInformation'Module
  maybeToEnum 30 = Prelude.Just SymbolInformation'Namespace
  maybeToEnum 31 = Prelude.Just SymbolInformation'Null
  maybeToEnum 32 = Prelude.Just SymbolInformation'Number
  maybeToEnum 33 = Prelude.Just SymbolInformation'Object
  maybeToEnum 34 = Prelude.Just SymbolInformation'Operator
  maybeToEnum 35 = Prelude.Just SymbolInformation'Package
  maybeToEnum 36 = Prelude.Just SymbolInformation'PackageObject
  maybeToEnum 37 = Prelude.Just SymbolInformation'Parameter
  maybeToEnum 38 = Prelude.Just SymbolInformation'ParameterLabel
  maybeToEnum 39 = Prelude.Just SymbolInformation'Pattern
  maybeToEnum 40 = Prelude.Just SymbolInformation'Predicate
  maybeToEnum 41 = Prelude.Just SymbolInformation'Property
  maybeToEnum 42 = Prelude.Just SymbolInformation'Protocol
  maybeToEnum 43 = Prelude.Just SymbolInformation'Quasiquoter
  maybeToEnum 44 = Prelude.Just SymbolInformation'SelfParameter
  maybeToEnum 45 = Prelude.Just SymbolInformation'Setter
  maybeToEnum 46 = Prelude.Just SymbolInformation'Signature
  maybeToEnum 47 = Prelude.Just SymbolInformation'Subscript
  maybeToEnum 48 = Prelude.Just SymbolInformation'String
  maybeToEnum 49 = Prelude.Just SymbolInformation'Struct
  maybeToEnum 50 = Prelude.Just SymbolInformation'Tactic
  maybeToEnum 51 = Prelude.Just SymbolInformation'Theorem
  maybeToEnum 52 = Prelude.Just SymbolInformation'ThisParameter
  maybeToEnum 53 = Prelude.Just SymbolInformation'Trait
  maybeToEnum 54 = Prelude.Just SymbolInformation'Type
  maybeToEnum 55 = Prelude.Just SymbolInformation'TypeAlias
  maybeToEnum 56 = Prelude.Just SymbolInformation'TypeClass
  maybeToEnum 57 = Prelude.Just SymbolInformation'TypeFamily
  maybeToEnum 58 = Prelude.Just SymbolInformation'TypeParameter
  maybeToEnum 59 = Prelude.Just SymbolInformation'Union
  maybeToEnum 60 = Prelude.Just SymbolInformation'Value
  maybeToEnum 61 = Prelude.Just SymbolInformation'Variable
  maybeToEnum 62 = Prelude.Just SymbolInformation'Contract
  maybeToEnum 63 = Prelude.Just SymbolInformation'Error
  maybeToEnum 64 = Prelude.Just SymbolInformation'Library
  maybeToEnum 65 = Prelude.Just SymbolInformation'Modifier
  maybeToEnum 66 = Prelude.Just SymbolInformation'AbstractMethod
  maybeToEnum 67 = Prelude.Just SymbolInformation'MethodSpecification
  maybeToEnum 68 = Prelude.Just SymbolInformation'ProtocolMethod
  maybeToEnum 69 = Prelude.Just SymbolInformation'PureVirtualMethod
  maybeToEnum 70 = Prelude.Just SymbolInformation'TraitMethod
  maybeToEnum 71 = Prelude.Just SymbolInformation'TypeClassMethod
  maybeToEnum 72 = Prelude.Just SymbolInformation'Accessor
  maybeToEnum 73 = Prelude.Just SymbolInformation'Delegate
  maybeToEnum 74 = Prelude.Just SymbolInformation'MethodAlias
  maybeToEnum 75 = Prelude.Just SymbolInformation'SingletonClass
  maybeToEnum 76 = Prelude.Just SymbolInformation'SingletonMethod
  maybeToEnum 77 = Prelude.Just SymbolInformation'StaticDataMember
  maybeToEnum 78 = Prelude.Just SymbolInformation'StaticEvent
  maybeToEnum 79 = Prelude.Just SymbolInformation'StaticField
  maybeToEnum 80 = Prelude.Just SymbolInformation'StaticMethod
  maybeToEnum 81 = Prelude.Just SymbolInformation'StaticProperty
  maybeToEnum 82 = Prelude.Just SymbolInformation'StaticVariable
  maybeToEnum 84 = Prelude.Just SymbolInformation'Extension
  maybeToEnum 85 = Prelude.Just SymbolInformation'Mixin
  maybeToEnum 86 = Prelude.Just SymbolInformation'Concept
  maybeToEnum k
    = Prelude.Just
        (SymbolInformation'Kind'Unrecognized
           (SymbolInformation'Kind'UnrecognizedValue
              (Prelude.fromIntegral k)))
  showEnum SymbolInformation'UnspecifiedKind = "UnspecifiedKind"
  showEnum SymbolInformation'AbstractMethod = "AbstractMethod"
  showEnum SymbolInformation'Accessor = "Accessor"
  showEnum SymbolInformation'Array = "Array"
  showEnum SymbolInformation'Assertion = "Assertion"
  showEnum SymbolInformation'AssociatedType = "AssociatedType"
  showEnum SymbolInformation'Attribute = "Attribute"
  showEnum SymbolInformation'Axiom = "Axiom"
  showEnum SymbolInformation'Boolean = "Boolean"
  showEnum SymbolInformation'Class = "Class"
  showEnum SymbolInformation'Concept = "Concept"
  showEnum SymbolInformation'Constant = "Constant"
  showEnum SymbolInformation'Constructor = "Constructor"
  showEnum SymbolInformation'Contract = "Contract"
  showEnum SymbolInformation'DataFamily = "DataFamily"
  showEnum SymbolInformation'Delegate = "Delegate"
  showEnum SymbolInformation'Enum = "Enum"
  showEnum SymbolInformation'EnumMember = "EnumMember"
  showEnum SymbolInformation'Error = "Error"
  showEnum SymbolInformation'Event = "Event"
  showEnum SymbolInformation'Extension = "Extension"
  showEnum SymbolInformation'Fact = "Fact"
  showEnum SymbolInformation'Field = "Field"
  showEnum SymbolInformation'File = "File"
  showEnum SymbolInformation'Function = "Function"
  showEnum SymbolInformation'Getter = "Getter"
  showEnum SymbolInformation'Grammar = "Grammar"
  showEnum SymbolInformation'Instance = "Instance"
  showEnum SymbolInformation'Interface = "Interface"
  showEnum SymbolInformation'Key = "Key"
  showEnum SymbolInformation'Lang = "Lang"
  showEnum SymbolInformation'Lemma = "Lemma"
  showEnum SymbolInformation'Library = "Library"
  showEnum SymbolInformation'Macro = "Macro"
  showEnum SymbolInformation'Method = "Method"
  showEnum SymbolInformation'MethodAlias = "MethodAlias"
  showEnum SymbolInformation'MethodReceiver = "MethodReceiver"
  showEnum SymbolInformation'MethodSpecification
    = "MethodSpecification"
  showEnum SymbolInformation'Message = "Message"
  showEnum SymbolInformation'Mixin = "Mixin"
  showEnum SymbolInformation'Modifier = "Modifier"
  showEnum SymbolInformation'Module = "Module"
  showEnum SymbolInformation'Namespace = "Namespace"
  showEnum SymbolInformation'Null = "Null"
  showEnum SymbolInformation'Number = "Number"
  showEnum SymbolInformation'Object = "Object"
  showEnum SymbolInformation'Operator = "Operator"
  showEnum SymbolInformation'Package = "Package"
  showEnum SymbolInformation'PackageObject = "PackageObject"
  showEnum SymbolInformation'Parameter = "Parameter"
  showEnum SymbolInformation'ParameterLabel = "ParameterLabel"
  showEnum SymbolInformation'Pattern = "Pattern"
  showEnum SymbolInformation'Predicate = "Predicate"
  showEnum SymbolInformation'Property = "Property"
  showEnum SymbolInformation'Protocol = "Protocol"
  showEnum SymbolInformation'ProtocolMethod = "ProtocolMethod"
  showEnum SymbolInformation'PureVirtualMethod = "PureVirtualMethod"
  showEnum SymbolInformation'Quasiquoter = "Quasiquoter"
  showEnum SymbolInformation'SelfParameter = "SelfParameter"
  showEnum SymbolInformation'Setter = "Setter"
  showEnum SymbolInformation'Signature = "Signature"
  showEnum SymbolInformation'SingletonClass = "SingletonClass"
  showEnum SymbolInformation'SingletonMethod = "SingletonMethod"
  showEnum SymbolInformation'StaticDataMember = "StaticDataMember"
  showEnum SymbolInformation'StaticEvent = "StaticEvent"
  showEnum SymbolInformation'StaticField = "StaticField"
  showEnum SymbolInformation'StaticMethod = "StaticMethod"
  showEnum SymbolInformation'StaticProperty = "StaticProperty"
  showEnum SymbolInformation'StaticVariable = "StaticVariable"
  showEnum SymbolInformation'String = "String"
  showEnum SymbolInformation'Struct = "Struct"
  showEnum SymbolInformation'Subscript = "Subscript"
  showEnum SymbolInformation'Tactic = "Tactic"
  showEnum SymbolInformation'Theorem = "Theorem"
  showEnum SymbolInformation'ThisParameter = "ThisParameter"
  showEnum SymbolInformation'Trait = "Trait"
  showEnum SymbolInformation'TraitMethod = "TraitMethod"
  showEnum SymbolInformation'Type = "Type"
  showEnum SymbolInformation'TypeAlias = "TypeAlias"
  showEnum SymbolInformation'TypeClass = "TypeClass"
  showEnum SymbolInformation'TypeClassMethod = "TypeClassMethod"
  showEnum SymbolInformation'TypeFamily = "TypeFamily"
  showEnum SymbolInformation'TypeParameter = "TypeParameter"
  showEnum SymbolInformation'Union = "Union"
  showEnum SymbolInformation'Value = "Value"
  showEnum SymbolInformation'Variable = "Variable"
  showEnum
    (SymbolInformation'Kind'Unrecognized (SymbolInformation'Kind'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "UnspecifiedKind"
    = Prelude.Just SymbolInformation'UnspecifiedKind
    | (Prelude.==) k "AbstractMethod"
    = Prelude.Just SymbolInformation'AbstractMethod
    | (Prelude.==) k "Accessor"
    = Prelude.Just SymbolInformation'Accessor
    | (Prelude.==) k "Array" = Prelude.Just SymbolInformation'Array
    | (Prelude.==) k "Assertion"
    = Prelude.Just SymbolInformation'Assertion
    | (Prelude.==) k "AssociatedType"
    = Prelude.Just SymbolInformation'AssociatedType
    | (Prelude.==) k "Attribute"
    = Prelude.Just SymbolInformation'Attribute
    | (Prelude.==) k "Axiom" = Prelude.Just SymbolInformation'Axiom
    | (Prelude.==) k "Boolean" = Prelude.Just SymbolInformation'Boolean
    | (Prelude.==) k "Class" = Prelude.Just SymbolInformation'Class
    | (Prelude.==) k "Concept" = Prelude.Just SymbolInformation'Concept
    | (Prelude.==) k "Constant"
    = Prelude.Just SymbolInformation'Constant
    | (Prelude.==) k "Constructor"
    = Prelude.Just SymbolInformation'Constructor
    | (Prelude.==) k "Contract"
    = Prelude.Just SymbolInformation'Contract
    | (Prelude.==) k "DataFamily"
    = Prelude.Just SymbolInformation'DataFamily
    | (Prelude.==) k "Delegate"
    = Prelude.Just SymbolInformation'Delegate
    | (Prelude.==) k "Enum" = Prelude.Just SymbolInformation'Enum
    | (Prelude.==) k "EnumMember"
    = Prelude.Just SymbolInformation'EnumMember
    | (Prelude.==) k "Error" = Prelude.Just SymbolInformation'Error
    | (Prelude.==) k "Event" = Prelude.Just SymbolInformation'Event
    | (Prelude.==) k "Extension"
    = Prelude.Just SymbolInformation'Extension
    | (Prelude.==) k "Fact" = Prelude.Just SymbolInformation'Fact
    | (Prelude.==) k "Field" = Prelude.Just SymbolInformation'Field
    | (Prelude.==) k "File" = Prelude.Just SymbolInformation'File
    | (Prelude.==) k "Function"
    = Prelude.Just SymbolInformation'Function
    | (Prelude.==) k "Getter" = Prelude.Just SymbolInformation'Getter
    | (Prelude.==) k "Grammar" = Prelude.Just SymbolInformation'Grammar
    | (Prelude.==) k "Instance"
    = Prelude.Just SymbolInformation'Instance
    | (Prelude.==) k "Interface"
    = Prelude.Just SymbolInformation'Interface
    | (Prelude.==) k "Key" = Prelude.Just SymbolInformation'Key
    | (Prelude.==) k "Lang" = Prelude.Just SymbolInformation'Lang
    | (Prelude.==) k "Lemma" = Prelude.Just SymbolInformation'Lemma
    | (Prelude.==) k "Library" = Prelude.Just SymbolInformation'Library
    | (Prelude.==) k "Macro" = Prelude.Just SymbolInformation'Macro
    | (Prelude.==) k "Method" = Prelude.Just SymbolInformation'Method
    | (Prelude.==) k "MethodAlias"
    = Prelude.Just SymbolInformation'MethodAlias
    | (Prelude.==) k "MethodReceiver"
    = Prelude.Just SymbolInformation'MethodReceiver
    | (Prelude.==) k "MethodSpecification"
    = Prelude.Just SymbolInformation'MethodSpecification
    | (Prelude.==) k "Message" = Prelude.Just SymbolInformation'Message
    | (Prelude.==) k "Mixin" = Prelude.Just SymbolInformation'Mixin
    | (Prelude.==) k "Modifier"
    = Prelude.Just SymbolInformation'Modifier
    | (Prelude.==) k "Module" = Prelude.Just SymbolInformation'Module
    | (Prelude.==) k "Namespace"
    = Prelude.Just SymbolInformation'Namespace
    | (Prelude.==) k "Null" = Prelude.Just SymbolInformation'Null
    | (Prelude.==) k "Number" = Prelude.Just SymbolInformation'Number
    | (Prelude.==) k "Object" = Prelude.Just SymbolInformation'Object
    | (Prelude.==) k "Operator"
    = Prelude.Just SymbolInformation'Operator
    | (Prelude.==) k "Package" = Prelude.Just SymbolInformation'Package
    | (Prelude.==) k "PackageObject"
    = Prelude.Just SymbolInformation'PackageObject
    | (Prelude.==) k "Parameter"
    = Prelude.Just SymbolInformation'Parameter
    | (Prelude.==) k "ParameterLabel"
    = Prelude.Just SymbolInformation'ParameterLabel
    | (Prelude.==) k "Pattern" = Prelude.Just SymbolInformation'Pattern
    | (Prelude.==) k "Predicate"
    = Prelude.Just SymbolInformation'Predicate
    | (Prelude.==) k "Property"
    = Prelude.Just SymbolInformation'Property
    | (Prelude.==) k "Protocol"
    = Prelude.Just SymbolInformation'Protocol
    | (Prelude.==) k "ProtocolMethod"
    = Prelude.Just SymbolInformation'ProtocolMethod
    | (Prelude.==) k "PureVirtualMethod"
    = Prelude.Just SymbolInformation'PureVirtualMethod
    | (Prelude.==) k "Quasiquoter"
    = Prelude.Just SymbolInformation'Quasiquoter
    | (Prelude.==) k "SelfParameter"
    = Prelude.Just SymbolInformation'SelfParameter
    | (Prelude.==) k "Setter" = Prelude.Just SymbolInformation'Setter
    | (Prelude.==) k "Signature"
    = Prelude.Just SymbolInformation'Signature
    | (Prelude.==) k "SingletonClass"
    = Prelude.Just SymbolInformation'SingletonClass
    | (Prelude.==) k "SingletonMethod"
    = Prelude.Just SymbolInformation'SingletonMethod
    | (Prelude.==) k "StaticDataMember"
    = Prelude.Just SymbolInformation'StaticDataMember
    | (Prelude.==) k "StaticEvent"
    = Prelude.Just SymbolInformation'StaticEvent
    | (Prelude.==) k "StaticField"
    = Prelude.Just SymbolInformation'StaticField
    | (Prelude.==) k "StaticMethod"
    = Prelude.Just SymbolInformation'StaticMethod
    | (Prelude.==) k "StaticProperty"
    = Prelude.Just SymbolInformation'StaticProperty
    | (Prelude.==) k "StaticVariable"
    = Prelude.Just SymbolInformation'StaticVariable
    | (Prelude.==) k "String" = Prelude.Just SymbolInformation'String
    | (Prelude.==) k "Struct" = Prelude.Just SymbolInformation'Struct
    | (Prelude.==) k "Subscript"
    = Prelude.Just SymbolInformation'Subscript
    | (Prelude.==) k "Tactic" = Prelude.Just SymbolInformation'Tactic
    | (Prelude.==) k "Theorem" = Prelude.Just SymbolInformation'Theorem
    | (Prelude.==) k "ThisParameter"
    = Prelude.Just SymbolInformation'ThisParameter
    | (Prelude.==) k "Trait" = Prelude.Just SymbolInformation'Trait
    | (Prelude.==) k "TraitMethod"
    = Prelude.Just SymbolInformation'TraitMethod
    | (Prelude.==) k "Type" = Prelude.Just SymbolInformation'Type
    | (Prelude.==) k "TypeAlias"
    = Prelude.Just SymbolInformation'TypeAlias
    | (Prelude.==) k "TypeClass"
    = Prelude.Just SymbolInformation'TypeClass
    | (Prelude.==) k "TypeClassMethod"
    = Prelude.Just SymbolInformation'TypeClassMethod
    | (Prelude.==) k "TypeFamily"
    = Prelude.Just SymbolInformation'TypeFamily
    | (Prelude.==) k "TypeParameter"
    = Prelude.Just SymbolInformation'TypeParameter
    | (Prelude.==) k "Union" = Prelude.Just SymbolInformation'Union
    | (Prelude.==) k "Value" = Prelude.Just SymbolInformation'Value
    | (Prelude.==) k "Variable"
    = Prelude.Just SymbolInformation'Variable
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded SymbolInformation'Kind where
  minBound = SymbolInformation'UnspecifiedKind
  maxBound = SymbolInformation'Concept
instance Prelude.Enum SymbolInformation'Kind where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum Kind: " (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum SymbolInformation'UnspecifiedKind = 0
  fromEnum SymbolInformation'Array = 1
  fromEnum SymbolInformation'Assertion = 2
  fromEnum SymbolInformation'AssociatedType = 3
  fromEnum SymbolInformation'Attribute = 4
  fromEnum SymbolInformation'Axiom = 5
  fromEnum SymbolInformation'Boolean = 6
  fromEnum SymbolInformation'Class = 7
  fromEnum SymbolInformation'Constant = 8
  fromEnum SymbolInformation'Constructor = 9
  fromEnum SymbolInformation'DataFamily = 10
  fromEnum SymbolInformation'Enum = 11
  fromEnum SymbolInformation'EnumMember = 12
  fromEnum SymbolInformation'Event = 13
  fromEnum SymbolInformation'Fact = 14
  fromEnum SymbolInformation'Field = 15
  fromEnum SymbolInformation'File = 16
  fromEnum SymbolInformation'Function = 17
  fromEnum SymbolInformation'Getter = 18
  fromEnum SymbolInformation'Grammar = 19
  fromEnum SymbolInformation'Instance = 20
  fromEnum SymbolInformation'Interface = 21
  fromEnum SymbolInformation'Key = 22
  fromEnum SymbolInformation'Lang = 23
  fromEnum SymbolInformation'Lemma = 24
  fromEnum SymbolInformation'Macro = 25
  fromEnum SymbolInformation'Method = 26
  fromEnum SymbolInformation'MethodReceiver = 27
  fromEnum SymbolInformation'Message = 28
  fromEnum SymbolInformation'Module = 29
  fromEnum SymbolInformation'Namespace = 30
  fromEnum SymbolInformation'Null = 31
  fromEnum SymbolInformation'Number = 32
  fromEnum SymbolInformation'Object = 33
  fromEnum SymbolInformation'Operator = 34
  fromEnum SymbolInformation'Package = 35
  fromEnum SymbolInformation'PackageObject = 36
  fromEnum SymbolInformation'Parameter = 37
  fromEnum SymbolInformation'ParameterLabel = 38
  fromEnum SymbolInformation'Pattern = 39
  fromEnum SymbolInformation'Predicate = 40
  fromEnum SymbolInformation'Property = 41
  fromEnum SymbolInformation'Protocol = 42
  fromEnum SymbolInformation'Quasiquoter = 43
  fromEnum SymbolInformation'SelfParameter = 44
  fromEnum SymbolInformation'Setter = 45
  fromEnum SymbolInformation'Signature = 46
  fromEnum SymbolInformation'Subscript = 47
  fromEnum SymbolInformation'String = 48
  fromEnum SymbolInformation'Struct = 49
  fromEnum SymbolInformation'Tactic = 50
  fromEnum SymbolInformation'Theorem = 51
  fromEnum SymbolInformation'ThisParameter = 52
  fromEnum SymbolInformation'Trait = 53
  fromEnum SymbolInformation'Type = 54
  fromEnum SymbolInformation'TypeAlias = 55
  fromEnum SymbolInformation'TypeClass = 56
  fromEnum SymbolInformation'TypeFamily = 57
  fromEnum SymbolInformation'TypeParameter = 58
  fromEnum SymbolInformation'Union = 59
  fromEnum SymbolInformation'Value = 60
  fromEnum SymbolInformation'Variable = 61
  fromEnum SymbolInformation'Contract = 62
  fromEnum SymbolInformation'Error = 63
  fromEnum SymbolInformation'Library = 64
  fromEnum SymbolInformation'Modifier = 65
  fromEnum SymbolInformation'AbstractMethod = 66
  fromEnum SymbolInformation'MethodSpecification = 67
  fromEnum SymbolInformation'ProtocolMethod = 68
  fromEnum SymbolInformation'PureVirtualMethod = 69
  fromEnum SymbolInformation'TraitMethod = 70
  fromEnum SymbolInformation'TypeClassMethod = 71
  fromEnum SymbolInformation'Accessor = 72
  fromEnum SymbolInformation'Delegate = 73
  fromEnum SymbolInformation'MethodAlias = 74
  fromEnum SymbolInformation'SingletonClass = 75
  fromEnum SymbolInformation'SingletonMethod = 76
  fromEnum SymbolInformation'StaticDataMember = 77
  fromEnum SymbolInformation'StaticEvent = 78
  fromEnum SymbolInformation'StaticField = 79
  fromEnum SymbolInformation'StaticMethod = 80
  fromEnum SymbolInformation'StaticProperty = 81
  fromEnum SymbolInformation'StaticVariable = 82
  fromEnum SymbolInformation'Extension = 84
  fromEnum SymbolInformation'Mixin = 85
  fromEnum SymbolInformation'Concept = 86
  fromEnum
    (SymbolInformation'Kind'Unrecognized (SymbolInformation'Kind'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ SymbolInformation'Concept
    = Prelude.error
        "SymbolInformation'Kind.succ: bad argument SymbolInformation'Concept. This value would be out of bounds."
  succ SymbolInformation'UnspecifiedKind = SymbolInformation'Array
  succ SymbolInformation'Array = SymbolInformation'Assertion
  succ SymbolInformation'Assertion = SymbolInformation'AssociatedType
  succ SymbolInformation'AssociatedType = SymbolInformation'Attribute
  succ SymbolInformation'Attribute = SymbolInformation'Axiom
  succ SymbolInformation'Axiom = SymbolInformation'Boolean
  succ SymbolInformation'Boolean = SymbolInformation'Class
  succ SymbolInformation'Class = SymbolInformation'Constant
  succ SymbolInformation'Constant = SymbolInformation'Constructor
  succ SymbolInformation'Constructor = SymbolInformation'DataFamily
  succ SymbolInformation'DataFamily = SymbolInformation'Enum
  succ SymbolInformation'Enum = SymbolInformation'EnumMember
  succ SymbolInformation'EnumMember = SymbolInformation'Event
  succ SymbolInformation'Event = SymbolInformation'Fact
  succ SymbolInformation'Fact = SymbolInformation'Field
  succ SymbolInformation'Field = SymbolInformation'File
  succ SymbolInformation'File = SymbolInformation'Function
  succ SymbolInformation'Function = SymbolInformation'Getter
  succ SymbolInformation'Getter = SymbolInformation'Grammar
  succ SymbolInformation'Grammar = SymbolInformation'Instance
  succ SymbolInformation'Instance = SymbolInformation'Interface
  succ SymbolInformation'Interface = SymbolInformation'Key
  succ SymbolInformation'Key = SymbolInformation'Lang
  succ SymbolInformation'Lang = SymbolInformation'Lemma
  succ SymbolInformation'Lemma = SymbolInformation'Macro
  succ SymbolInformation'Macro = SymbolInformation'Method
  succ SymbolInformation'Method = SymbolInformation'MethodReceiver
  succ SymbolInformation'MethodReceiver = SymbolInformation'Message
  succ SymbolInformation'Message = SymbolInformation'Module
  succ SymbolInformation'Module = SymbolInformation'Namespace
  succ SymbolInformation'Namespace = SymbolInformation'Null
  succ SymbolInformation'Null = SymbolInformation'Number
  succ SymbolInformation'Number = SymbolInformation'Object
  succ SymbolInformation'Object = SymbolInformation'Operator
  succ SymbolInformation'Operator = SymbolInformation'Package
  succ SymbolInformation'Package = SymbolInformation'PackageObject
  succ SymbolInformation'PackageObject = SymbolInformation'Parameter
  succ SymbolInformation'Parameter = SymbolInformation'ParameterLabel
  succ SymbolInformation'ParameterLabel = SymbolInformation'Pattern
  succ SymbolInformation'Pattern = SymbolInformation'Predicate
  succ SymbolInformation'Predicate = SymbolInformation'Property
  succ SymbolInformation'Property = SymbolInformation'Protocol
  succ SymbolInformation'Protocol = SymbolInformation'Quasiquoter
  succ SymbolInformation'Quasiquoter
    = SymbolInformation'SelfParameter
  succ SymbolInformation'SelfParameter = SymbolInformation'Setter
  succ SymbolInformation'Setter = SymbolInformation'Signature
  succ SymbolInformation'Signature = SymbolInformation'Subscript
  succ SymbolInformation'Subscript = SymbolInformation'String
  succ SymbolInformation'String = SymbolInformation'Struct
  succ SymbolInformation'Struct = SymbolInformation'Tactic
  succ SymbolInformation'Tactic = SymbolInformation'Theorem
  succ SymbolInformation'Theorem = SymbolInformation'ThisParameter
  succ SymbolInformation'ThisParameter = SymbolInformation'Trait
  succ SymbolInformation'Trait = SymbolInformation'Type
  succ SymbolInformation'Type = SymbolInformation'TypeAlias
  succ SymbolInformation'TypeAlias = SymbolInformation'TypeClass
  succ SymbolInformation'TypeClass = SymbolInformation'TypeFamily
  succ SymbolInformation'TypeFamily = SymbolInformation'TypeParameter
  succ SymbolInformation'TypeParameter = SymbolInformation'Union
  succ SymbolInformation'Union = SymbolInformation'Value
  succ SymbolInformation'Value = SymbolInformation'Variable
  succ SymbolInformation'Variable = SymbolInformation'Contract
  succ SymbolInformation'Contract = SymbolInformation'Error
  succ SymbolInformation'Error = SymbolInformation'Library
  succ SymbolInformation'Library = SymbolInformation'Modifier
  succ SymbolInformation'Modifier = SymbolInformation'AbstractMethod
  succ SymbolInformation'AbstractMethod
    = SymbolInformation'MethodSpecification
  succ SymbolInformation'MethodSpecification
    = SymbolInformation'ProtocolMethod
  succ SymbolInformation'ProtocolMethod
    = SymbolInformation'PureVirtualMethod
  succ SymbolInformation'PureVirtualMethod
    = SymbolInformation'TraitMethod
  succ SymbolInformation'TraitMethod
    = SymbolInformation'TypeClassMethod
  succ SymbolInformation'TypeClassMethod = SymbolInformation'Accessor
  succ SymbolInformation'Accessor = SymbolInformation'Delegate
  succ SymbolInformation'Delegate = SymbolInformation'MethodAlias
  succ SymbolInformation'MethodAlias
    = SymbolInformation'SingletonClass
  succ SymbolInformation'SingletonClass
    = SymbolInformation'SingletonMethod
  succ SymbolInformation'SingletonMethod
    = SymbolInformation'StaticDataMember
  succ SymbolInformation'StaticDataMember
    = SymbolInformation'StaticEvent
  succ SymbolInformation'StaticEvent = SymbolInformation'StaticField
  succ SymbolInformation'StaticField = SymbolInformation'StaticMethod
  succ SymbolInformation'StaticMethod
    = SymbolInformation'StaticProperty
  succ SymbolInformation'StaticProperty
    = SymbolInformation'StaticVariable
  succ SymbolInformation'StaticVariable = SymbolInformation'Extension
  succ SymbolInformation'Extension = SymbolInformation'Mixin
  succ SymbolInformation'Mixin = SymbolInformation'Concept
  succ (SymbolInformation'Kind'Unrecognized _)
    = Prelude.error
        "SymbolInformation'Kind.succ: bad argument: unrecognized value"
  pred SymbolInformation'UnspecifiedKind
    = Prelude.error
        "SymbolInformation'Kind.pred: bad argument SymbolInformation'UnspecifiedKind. This value would be out of bounds."
  pred SymbolInformation'Array = SymbolInformation'UnspecifiedKind
  pred SymbolInformation'Assertion = SymbolInformation'Array
  pred SymbolInformation'AssociatedType = SymbolInformation'Assertion
  pred SymbolInformation'Attribute = SymbolInformation'AssociatedType
  pred SymbolInformation'Axiom = SymbolInformation'Attribute
  pred SymbolInformation'Boolean = SymbolInformation'Axiom
  pred SymbolInformation'Class = SymbolInformation'Boolean
  pred SymbolInformation'Constant = SymbolInformation'Class
  pred SymbolInformation'Constructor = SymbolInformation'Constant
  pred SymbolInformation'DataFamily = SymbolInformation'Constructor
  pred SymbolInformation'Enum = SymbolInformation'DataFamily
  pred SymbolInformation'EnumMember = SymbolInformation'Enum
  pred SymbolInformation'Event = SymbolInformation'EnumMember
  pred SymbolInformation'Fact = SymbolInformation'Event
  pred SymbolInformation'Field = SymbolInformation'Fact
  pred SymbolInformation'File = SymbolInformation'Field
  pred SymbolInformation'Function = SymbolInformation'File
  pred SymbolInformation'Getter = SymbolInformation'Function
  pred SymbolInformation'Grammar = SymbolInformation'Getter
  pred SymbolInformation'Instance = SymbolInformation'Grammar
  pred SymbolInformation'Interface = SymbolInformation'Instance
  pred SymbolInformation'Key = SymbolInformation'Interface
  pred SymbolInformation'Lang = SymbolInformation'Key
  pred SymbolInformation'Lemma = SymbolInformation'Lang
  pred SymbolInformation'Macro = SymbolInformation'Lemma
  pred SymbolInformation'Method = SymbolInformation'Macro
  pred SymbolInformation'MethodReceiver = SymbolInformation'Method
  pred SymbolInformation'Message = SymbolInformation'MethodReceiver
  pred SymbolInformation'Module = SymbolInformation'Message
  pred SymbolInformation'Namespace = SymbolInformation'Module
  pred SymbolInformation'Null = SymbolInformation'Namespace
  pred SymbolInformation'Number = SymbolInformation'Null
  pred SymbolInformation'Object = SymbolInformation'Number
  pred SymbolInformation'Operator = SymbolInformation'Object
  pred SymbolInformation'Package = SymbolInformation'Operator
  pred SymbolInformation'PackageObject = SymbolInformation'Package
  pred SymbolInformation'Parameter = SymbolInformation'PackageObject
  pred SymbolInformation'ParameterLabel = SymbolInformation'Parameter
  pred SymbolInformation'Pattern = SymbolInformation'ParameterLabel
  pred SymbolInformation'Predicate = SymbolInformation'Pattern
  pred SymbolInformation'Property = SymbolInformation'Predicate
  pred SymbolInformation'Protocol = SymbolInformation'Property
  pred SymbolInformation'Quasiquoter = SymbolInformation'Protocol
  pred SymbolInformation'SelfParameter
    = SymbolInformation'Quasiquoter
  pred SymbolInformation'Setter = SymbolInformation'SelfParameter
  pred SymbolInformation'Signature = SymbolInformation'Setter
  pred SymbolInformation'Subscript = SymbolInformation'Signature
  pred SymbolInformation'String = SymbolInformation'Subscript
  pred SymbolInformation'Struct = SymbolInformation'String
  pred SymbolInformation'Tactic = SymbolInformation'Struct
  pred SymbolInformation'Theorem = SymbolInformation'Tactic
  pred SymbolInformation'ThisParameter = SymbolInformation'Theorem
  pred SymbolInformation'Trait = SymbolInformation'ThisParameter
  pred SymbolInformation'Type = SymbolInformation'Trait
  pred SymbolInformation'TypeAlias = SymbolInformation'Type
  pred SymbolInformation'TypeClass = SymbolInformation'TypeAlias
  pred SymbolInformation'TypeFamily = SymbolInformation'TypeClass
  pred SymbolInformation'TypeParameter = SymbolInformation'TypeFamily
  pred SymbolInformation'Union = SymbolInformation'TypeParameter
  pred SymbolInformation'Value = SymbolInformation'Union
  pred SymbolInformation'Variable = SymbolInformation'Value
  pred SymbolInformation'Contract = SymbolInformation'Variable
  pred SymbolInformation'Error = SymbolInformation'Contract
  pred SymbolInformation'Library = SymbolInformation'Error
  pred SymbolInformation'Modifier = SymbolInformation'Library
  pred SymbolInformation'AbstractMethod = SymbolInformation'Modifier
  pred SymbolInformation'MethodSpecification
    = SymbolInformation'AbstractMethod
  pred SymbolInformation'ProtocolMethod
    = SymbolInformation'MethodSpecification
  pred SymbolInformation'PureVirtualMethod
    = SymbolInformation'ProtocolMethod
  pred SymbolInformation'TraitMethod
    = SymbolInformation'PureVirtualMethod
  pred SymbolInformation'TypeClassMethod
    = SymbolInformation'TraitMethod
  pred SymbolInformation'Accessor = SymbolInformation'TypeClassMethod
  pred SymbolInformation'Delegate = SymbolInformation'Accessor
  pred SymbolInformation'MethodAlias = SymbolInformation'Delegate
  pred SymbolInformation'SingletonClass
    = SymbolInformation'MethodAlias
  pred SymbolInformation'SingletonMethod
    = SymbolInformation'SingletonClass
  pred SymbolInformation'StaticDataMember
    = SymbolInformation'SingletonMethod
  pred SymbolInformation'StaticEvent
    = SymbolInformation'StaticDataMember
  pred SymbolInformation'StaticField = SymbolInformation'StaticEvent
  pred SymbolInformation'StaticMethod = SymbolInformation'StaticField
  pred SymbolInformation'StaticProperty
    = SymbolInformation'StaticMethod
  pred SymbolInformation'StaticVariable
    = SymbolInformation'StaticProperty
  pred SymbolInformation'Extension = SymbolInformation'StaticVariable
  pred SymbolInformation'Mixin = SymbolInformation'Extension
  pred SymbolInformation'Concept = SymbolInformation'Mixin
  pred (SymbolInformation'Kind'Unrecognized _)
    = Prelude.error
        "SymbolInformation'Kind.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault SymbolInformation'Kind where
  fieldDefault = SymbolInformation'UnspecifiedKind
instance Control.DeepSeq.NFData SymbolInformation'Kind where
  rnf x__ = Prelude.seq x__ ()
newtype SymbolRole'UnrecognizedValue
  = SymbolRole'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data SymbolRole
  = UnspecifiedSymbolRole |
    Definition |
    Import |
    WriteAccess |
    ReadAccess |
    Generated |
    Test |
    ForwardDefinition |
    SymbolRole'Unrecognized !SymbolRole'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum SymbolRole where
  maybeToEnum 0 = Prelude.Just UnspecifiedSymbolRole
  maybeToEnum 1 = Prelude.Just Definition
  maybeToEnum 2 = Prelude.Just Import
  maybeToEnum 4 = Prelude.Just WriteAccess
  maybeToEnum 8 = Prelude.Just ReadAccess
  maybeToEnum 16 = Prelude.Just Generated
  maybeToEnum 32 = Prelude.Just Test
  maybeToEnum 64 = Prelude.Just ForwardDefinition
  maybeToEnum k
    = Prelude.Just
        (SymbolRole'Unrecognized
           (SymbolRole'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum UnspecifiedSymbolRole = "UnspecifiedSymbolRole"
  showEnum Definition = "Definition"
  showEnum Import = "Import"
  showEnum WriteAccess = "WriteAccess"
  showEnum ReadAccess = "ReadAccess"
  showEnum Generated = "Generated"
  showEnum Test = "Test"
  showEnum ForwardDefinition = "ForwardDefinition"
  showEnum (SymbolRole'Unrecognized (SymbolRole'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "UnspecifiedSymbolRole"
    = Prelude.Just UnspecifiedSymbolRole
    | (Prelude.==) k "Definition" = Prelude.Just Definition
    | (Prelude.==) k "Import" = Prelude.Just Import
    | (Prelude.==) k "WriteAccess" = Prelude.Just WriteAccess
    | (Prelude.==) k "ReadAccess" = Prelude.Just ReadAccess
    | (Prelude.==) k "Generated" = Prelude.Just Generated
    | (Prelude.==) k "Test" = Prelude.Just Test
    | (Prelude.==) k "ForwardDefinition"
    = Prelude.Just ForwardDefinition
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded SymbolRole where
  minBound = UnspecifiedSymbolRole
  maxBound = ForwardDefinition
instance Prelude.Enum SymbolRole where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum SymbolRole: " (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum UnspecifiedSymbolRole = 0
  fromEnum Definition = 1
  fromEnum Import = 2
  fromEnum WriteAccess = 4
  fromEnum ReadAccess = 8
  fromEnum Generated = 16
  fromEnum Test = 32
  fromEnum ForwardDefinition = 64
  fromEnum (SymbolRole'Unrecognized (SymbolRole'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ ForwardDefinition
    = Prelude.error
        "SymbolRole.succ: bad argument ForwardDefinition. This value would be out of bounds."
  succ UnspecifiedSymbolRole = Definition
  succ Definition = Import
  succ Import = WriteAccess
  succ WriteAccess = ReadAccess
  succ ReadAccess = Generated
  succ Generated = Test
  succ Test = ForwardDefinition
  succ (SymbolRole'Unrecognized _)
    = Prelude.error "SymbolRole.succ: bad argument: unrecognized value"
  pred UnspecifiedSymbolRole
    = Prelude.error
        "SymbolRole.pred: bad argument UnspecifiedSymbolRole. This value would be out of bounds."
  pred Definition = UnspecifiedSymbolRole
  pred Import = Definition
  pred WriteAccess = Import
  pred ReadAccess = WriteAccess
  pred Generated = ReadAccess
  pred Test = Generated
  pred ForwardDefinition = Test
  pred (SymbolRole'Unrecognized _)
    = Prelude.error "SymbolRole.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault SymbolRole where
  fieldDefault = UnspecifiedSymbolRole
instance Control.DeepSeq.NFData SymbolRole where
  rnf x__ = Prelude.seq x__ ()
newtype SyntaxKind'UnrecognizedValue
  = SyntaxKind'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data SyntaxKind
  = UnspecifiedSyntaxKind |
    Comment |
    PunctuationDelimiter |
    PunctuationBracket |
    Keyword |
    IdentifierOperator |
    Identifier |
    IdentifierBuiltin |
    IdentifierNull |
    IdentifierConstant |
    IdentifierMutableGlobal |
    IdentifierParameter |
    IdentifierLocal |
    IdentifierShadowed |
    IdentifierNamespace |
    IdentifierFunction |
    IdentifierFunctionDefinition |
    IdentifierMacro |
    IdentifierMacroDefinition |
    IdentifierType |
    IdentifierBuiltinType |
    IdentifierAttribute |
    RegexEscape |
    RegexRepeated |
    RegexWildcard |
    RegexDelimiter |
    RegexJoin |
    StringLiteral |
    StringLiteralEscape |
    StringLiteralSpecial |
    StringLiteralKey |
    CharacterLiteral |
    NumericLiteral |
    BooleanLiteral |
    Tag |
    TagAttribute |
    TagDelimiter |
    SyntaxKind'Unrecognized !SyntaxKind'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum SyntaxKind where
  maybeToEnum 0 = Prelude.Just UnspecifiedSyntaxKind
  maybeToEnum 1 = Prelude.Just Comment
  maybeToEnum 2 = Prelude.Just PunctuationDelimiter
  maybeToEnum 3 = Prelude.Just PunctuationBracket
  maybeToEnum 4 = Prelude.Just Keyword
  maybeToEnum 5 = Prelude.Just IdentifierOperator
  maybeToEnum 6 = Prelude.Just Identifier
  maybeToEnum 7 = Prelude.Just IdentifierBuiltin
  maybeToEnum 8 = Prelude.Just IdentifierNull
  maybeToEnum 9 = Prelude.Just IdentifierConstant
  maybeToEnum 10 = Prelude.Just IdentifierMutableGlobal
  maybeToEnum 11 = Prelude.Just IdentifierParameter
  maybeToEnum 12 = Prelude.Just IdentifierLocal
  maybeToEnum 13 = Prelude.Just IdentifierShadowed
  maybeToEnum 14 = Prelude.Just IdentifierNamespace
  maybeToEnum 15 = Prelude.Just IdentifierFunction
  maybeToEnum 16 = Prelude.Just IdentifierFunctionDefinition
  maybeToEnum 17 = Prelude.Just IdentifierMacro
  maybeToEnum 18 = Prelude.Just IdentifierMacroDefinition
  maybeToEnum 19 = Prelude.Just IdentifierType
  maybeToEnum 20 = Prelude.Just IdentifierBuiltinType
  maybeToEnum 21 = Prelude.Just IdentifierAttribute
  maybeToEnum 22 = Prelude.Just RegexEscape
  maybeToEnum 23 = Prelude.Just RegexRepeated
  maybeToEnum 24 = Prelude.Just RegexWildcard
  maybeToEnum 25 = Prelude.Just RegexDelimiter
  maybeToEnum 26 = Prelude.Just RegexJoin
  maybeToEnum 27 = Prelude.Just StringLiteral
  maybeToEnum 28 = Prelude.Just StringLiteralEscape
  maybeToEnum 29 = Prelude.Just StringLiteralSpecial
  maybeToEnum 30 = Prelude.Just StringLiteralKey
  maybeToEnum 31 = Prelude.Just CharacterLiteral
  maybeToEnum 32 = Prelude.Just NumericLiteral
  maybeToEnum 33 = Prelude.Just BooleanLiteral
  maybeToEnum 34 = Prelude.Just Tag
  maybeToEnum 35 = Prelude.Just TagAttribute
  maybeToEnum 36 = Prelude.Just TagDelimiter
  maybeToEnum k
    = Prelude.Just
        (SyntaxKind'Unrecognized
           (SyntaxKind'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum UnspecifiedSyntaxKind = "UnspecifiedSyntaxKind"
  showEnum Comment = "Comment"
  showEnum PunctuationDelimiter = "PunctuationDelimiter"
  showEnum PunctuationBracket = "PunctuationBracket"
  showEnum Keyword = "Keyword"
  showEnum IdentifierOperator = "IdentifierOperator"
  showEnum Identifier = "Identifier"
  showEnum IdentifierBuiltin = "IdentifierBuiltin"
  showEnum IdentifierNull = "IdentifierNull"
  showEnum IdentifierConstant = "IdentifierConstant"
  showEnum IdentifierMutableGlobal = "IdentifierMutableGlobal"
  showEnum IdentifierParameter = "IdentifierParameter"
  showEnum IdentifierLocal = "IdentifierLocal"
  showEnum IdentifierShadowed = "IdentifierShadowed"
  showEnum IdentifierNamespace = "IdentifierNamespace"
  showEnum IdentifierFunction = "IdentifierFunction"
  showEnum IdentifierFunctionDefinition
    = "IdentifierFunctionDefinition"
  showEnum IdentifierMacro = "IdentifierMacro"
  showEnum IdentifierMacroDefinition = "IdentifierMacroDefinition"
  showEnum IdentifierType = "IdentifierType"
  showEnum IdentifierBuiltinType = "IdentifierBuiltinType"
  showEnum IdentifierAttribute = "IdentifierAttribute"
  showEnum RegexEscape = "RegexEscape"
  showEnum RegexRepeated = "RegexRepeated"
  showEnum RegexWildcard = "RegexWildcard"
  showEnum RegexDelimiter = "RegexDelimiter"
  showEnum RegexJoin = "RegexJoin"
  showEnum StringLiteral = "StringLiteral"
  showEnum StringLiteralEscape = "StringLiteralEscape"
  showEnum StringLiteralSpecial = "StringLiteralSpecial"
  showEnum StringLiteralKey = "StringLiteralKey"
  showEnum CharacterLiteral = "CharacterLiteral"
  showEnum NumericLiteral = "NumericLiteral"
  showEnum BooleanLiteral = "BooleanLiteral"
  showEnum Tag = "Tag"
  showEnum TagAttribute = "TagAttribute"
  showEnum TagDelimiter = "TagDelimiter"
  showEnum (SyntaxKind'Unrecognized (SyntaxKind'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "UnspecifiedSyntaxKind"
    = Prelude.Just UnspecifiedSyntaxKind
    | (Prelude.==) k "Comment" = Prelude.Just Comment
    | (Prelude.==) k "PunctuationDelimiter"
    = Prelude.Just PunctuationDelimiter
    | (Prelude.==) k "PunctuationBracket"
    = Prelude.Just PunctuationBracket
    | (Prelude.==) k "Keyword" = Prelude.Just Keyword
    | (Prelude.==) k "IdentifierKeyword"
    = Prelude.Just IdentifierKeyword
    | (Prelude.==) k "IdentifierOperator"
    = Prelude.Just IdentifierOperator
    | (Prelude.==) k "Identifier" = Prelude.Just Identifier
    | (Prelude.==) k "IdentifierBuiltin"
    = Prelude.Just IdentifierBuiltin
    | (Prelude.==) k "IdentifierNull" = Prelude.Just IdentifierNull
    | (Prelude.==) k "IdentifierConstant"
    = Prelude.Just IdentifierConstant
    | (Prelude.==) k "IdentifierMutableGlobal"
    = Prelude.Just IdentifierMutableGlobal
    | (Prelude.==) k "IdentifierParameter"
    = Prelude.Just IdentifierParameter
    | (Prelude.==) k "IdentifierLocal" = Prelude.Just IdentifierLocal
    | (Prelude.==) k "IdentifierShadowed"
    = Prelude.Just IdentifierShadowed
    | (Prelude.==) k "IdentifierNamespace"
    = Prelude.Just IdentifierNamespace
    | (Prelude.==) k "IdentifierModule" = Prelude.Just IdentifierModule
    | (Prelude.==) k "IdentifierFunction"
    = Prelude.Just IdentifierFunction
    | (Prelude.==) k "IdentifierFunctionDefinition"
    = Prelude.Just IdentifierFunctionDefinition
    | (Prelude.==) k "IdentifierMacro" = Prelude.Just IdentifierMacro
    | (Prelude.==) k "IdentifierMacroDefinition"
    = Prelude.Just IdentifierMacroDefinition
    | (Prelude.==) k "IdentifierType" = Prelude.Just IdentifierType
    | (Prelude.==) k "IdentifierBuiltinType"
    = Prelude.Just IdentifierBuiltinType
    | (Prelude.==) k "IdentifierAttribute"
    = Prelude.Just IdentifierAttribute
    | (Prelude.==) k "RegexEscape" = Prelude.Just RegexEscape
    | (Prelude.==) k "RegexRepeated" = Prelude.Just RegexRepeated
    | (Prelude.==) k "RegexWildcard" = Prelude.Just RegexWildcard
    | (Prelude.==) k "RegexDelimiter" = Prelude.Just RegexDelimiter
    | (Prelude.==) k "RegexJoin" = Prelude.Just RegexJoin
    | (Prelude.==) k "StringLiteral" = Prelude.Just StringLiteral
    | (Prelude.==) k "StringLiteralEscape"
    = Prelude.Just StringLiteralEscape
    | (Prelude.==) k "StringLiteralSpecial"
    = Prelude.Just StringLiteralSpecial
    | (Prelude.==) k "StringLiteralKey" = Prelude.Just StringLiteralKey
    | (Prelude.==) k "CharacterLiteral" = Prelude.Just CharacterLiteral
    | (Prelude.==) k "NumericLiteral" = Prelude.Just NumericLiteral
    | (Prelude.==) k "BooleanLiteral" = Prelude.Just BooleanLiteral
    | (Prelude.==) k "Tag" = Prelude.Just Tag
    | (Prelude.==) k "TagAttribute" = Prelude.Just TagAttribute
    | (Prelude.==) k "TagDelimiter" = Prelude.Just TagDelimiter
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded SyntaxKind where
  minBound = UnspecifiedSyntaxKind
  maxBound = TagDelimiter
instance Prelude.Enum SyntaxKind where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum SyntaxKind: " (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum UnspecifiedSyntaxKind = 0
  fromEnum Comment = 1
  fromEnum PunctuationDelimiter = 2
  fromEnum PunctuationBracket = 3
  fromEnum Keyword = 4
  fromEnum IdentifierOperator = 5
  fromEnum Identifier = 6
  fromEnum IdentifierBuiltin = 7
  fromEnum IdentifierNull = 8
  fromEnum IdentifierConstant = 9
  fromEnum IdentifierMutableGlobal = 10
  fromEnum IdentifierParameter = 11
  fromEnum IdentifierLocal = 12
  fromEnum IdentifierShadowed = 13
  fromEnum IdentifierNamespace = 14
  fromEnum IdentifierFunction = 15
  fromEnum IdentifierFunctionDefinition = 16
  fromEnum IdentifierMacro = 17
  fromEnum IdentifierMacroDefinition = 18
  fromEnum IdentifierType = 19
  fromEnum IdentifierBuiltinType = 20
  fromEnum IdentifierAttribute = 21
  fromEnum RegexEscape = 22
  fromEnum RegexRepeated = 23
  fromEnum RegexWildcard = 24
  fromEnum RegexDelimiter = 25
  fromEnum RegexJoin = 26
  fromEnum StringLiteral = 27
  fromEnum StringLiteralEscape = 28
  fromEnum StringLiteralSpecial = 29
  fromEnum StringLiteralKey = 30
  fromEnum CharacterLiteral = 31
  fromEnum NumericLiteral = 32
  fromEnum BooleanLiteral = 33
  fromEnum Tag = 34
  fromEnum TagAttribute = 35
  fromEnum TagDelimiter = 36
  fromEnum (SyntaxKind'Unrecognized (SyntaxKind'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ TagDelimiter
    = Prelude.error
        "SyntaxKind.succ: bad argument TagDelimiter. This value would be out of bounds."
  succ UnspecifiedSyntaxKind = Comment
  succ Comment = PunctuationDelimiter
  succ PunctuationDelimiter = PunctuationBracket
  succ PunctuationBracket = Keyword
  succ Keyword = IdentifierOperator
  succ IdentifierOperator = Identifier
  succ Identifier = IdentifierBuiltin
  succ IdentifierBuiltin = IdentifierNull
  succ IdentifierNull = IdentifierConstant
  succ IdentifierConstant = IdentifierMutableGlobal
  succ IdentifierMutableGlobal = IdentifierParameter
  succ IdentifierParameter = IdentifierLocal
  succ IdentifierLocal = IdentifierShadowed
  succ IdentifierShadowed = IdentifierNamespace
  succ IdentifierNamespace = IdentifierFunction
  succ IdentifierFunction = IdentifierFunctionDefinition
  succ IdentifierFunctionDefinition = IdentifierMacro
  succ IdentifierMacro = IdentifierMacroDefinition
  succ IdentifierMacroDefinition = IdentifierType
  succ IdentifierType = IdentifierBuiltinType
  succ IdentifierBuiltinType = IdentifierAttribute
  succ IdentifierAttribute = RegexEscape
  succ RegexEscape = RegexRepeated
  succ RegexRepeated = RegexWildcard
  succ RegexWildcard = RegexDelimiter
  succ RegexDelimiter = RegexJoin
  succ RegexJoin = StringLiteral
  succ StringLiteral = StringLiteralEscape
  succ StringLiteralEscape = StringLiteralSpecial
  succ StringLiteralSpecial = StringLiteralKey
  succ StringLiteralKey = CharacterLiteral
  succ CharacterLiteral = NumericLiteral
  succ NumericLiteral = BooleanLiteral
  succ BooleanLiteral = Tag
  succ Tag = TagAttribute
  succ TagAttribute = TagDelimiter
  succ (SyntaxKind'Unrecognized _)
    = Prelude.error "SyntaxKind.succ: bad argument: unrecognized value"
  pred UnspecifiedSyntaxKind
    = Prelude.error
        "SyntaxKind.pred: bad argument UnspecifiedSyntaxKind. This value would be out of bounds."
  pred Comment = UnspecifiedSyntaxKind
  pred PunctuationDelimiter = Comment
  pred PunctuationBracket = PunctuationDelimiter
  pred Keyword = PunctuationBracket
  pred IdentifierOperator = Keyword
  pred Identifier = IdentifierOperator
  pred IdentifierBuiltin = Identifier
  pred IdentifierNull = IdentifierBuiltin
  pred IdentifierConstant = IdentifierNull
  pred IdentifierMutableGlobal = IdentifierConstant
  pred IdentifierParameter = IdentifierMutableGlobal
  pred IdentifierLocal = IdentifierParameter
  pred IdentifierShadowed = IdentifierLocal
  pred IdentifierNamespace = IdentifierShadowed
  pred IdentifierFunction = IdentifierNamespace
  pred IdentifierFunctionDefinition = IdentifierFunction
  pred IdentifierMacro = IdentifierFunctionDefinition
  pred IdentifierMacroDefinition = IdentifierMacro
  pred IdentifierType = IdentifierMacroDefinition
  pred IdentifierBuiltinType = IdentifierType
  pred IdentifierAttribute = IdentifierBuiltinType
  pred RegexEscape = IdentifierAttribute
  pred RegexRepeated = RegexEscape
  pred RegexWildcard = RegexRepeated
  pred RegexDelimiter = RegexWildcard
  pred RegexJoin = RegexDelimiter
  pred StringLiteral = RegexJoin
  pred StringLiteralEscape = StringLiteral
  pred StringLiteralSpecial = StringLiteralEscape
  pred StringLiteralKey = StringLiteralSpecial
  pred CharacterLiteral = StringLiteralKey
  pred NumericLiteral = CharacterLiteral
  pred BooleanLiteral = NumericLiteral
  pred Tag = BooleanLiteral
  pred TagAttribute = Tag
  pred TagDelimiter = TagAttribute
  pred (SyntaxKind'Unrecognized _)
    = Prelude.error "SyntaxKind.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault SyntaxKind where
  fieldDefault = UnspecifiedSyntaxKind
instance Control.DeepSeq.NFData SyntaxKind where
  rnf x__ = Prelude.seq x__ ()
pattern IdentifierKeyword :: SyntaxKind
pattern IdentifierKeyword = Keyword
pattern IdentifierModule :: SyntaxKind
pattern IdentifierModule = IdentifierNamespace
newtype TextEncoding'UnrecognizedValue
  = TextEncoding'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data TextEncoding
  = UnspecifiedTextEncoding |
    UTF8 |
    UTF16 |
    TextEncoding'Unrecognized !TextEncoding'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum TextEncoding where
  maybeToEnum 0 = Prelude.Just UnspecifiedTextEncoding
  maybeToEnum 1 = Prelude.Just UTF8
  maybeToEnum 2 = Prelude.Just UTF16
  maybeToEnum k
    = Prelude.Just
        (TextEncoding'Unrecognized
           (TextEncoding'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum UnspecifiedTextEncoding = "UnspecifiedTextEncoding"
  showEnum UTF8 = "UTF8"
  showEnum UTF16 = "UTF16"
  showEnum
    (TextEncoding'Unrecognized (TextEncoding'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "UnspecifiedTextEncoding"
    = Prelude.Just UnspecifiedTextEncoding
    | (Prelude.==) k "UTF8" = Prelude.Just UTF8
    | (Prelude.==) k "UTF16" = Prelude.Just UTF16
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded TextEncoding where
  minBound = UnspecifiedTextEncoding
  maxBound = UTF16
instance Prelude.Enum TextEncoding where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum TextEncoding: "
              (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum UnspecifiedTextEncoding = 0
  fromEnum UTF8 = 1
  fromEnum UTF16 = 2
  fromEnum
    (TextEncoding'Unrecognized (TextEncoding'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ UTF16
    = Prelude.error
        "TextEncoding.succ: bad argument UTF16. This value would be out of bounds."
  succ UnspecifiedTextEncoding = UTF8
  succ UTF8 = UTF16
  succ (TextEncoding'Unrecognized _)
    = Prelude.error
        "TextEncoding.succ: bad argument: unrecognized value"
  pred UnspecifiedTextEncoding
    = Prelude.error
        "TextEncoding.pred: bad argument UnspecifiedTextEncoding. This value would be out of bounds."
  pred UTF8 = UnspecifiedTextEncoding
  pred UTF16 = UTF8
  pred (TextEncoding'Unrecognized _)
    = Prelude.error
        "TextEncoding.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault TextEncoding where
  fieldDefault = UnspecifiedTextEncoding
instance Control.DeepSeq.NFData TextEncoding where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :

         * 'Proto.Scip_Fields.name' @:: Lens' ToolInfo Data.Text.Text@
         * 'Proto.Scip_Fields.version' @:: Lens' ToolInfo Data.Text.Text@
         * 'Proto.Scip_Fields.arguments' @:: Lens' ToolInfo [Data.Text.Text]@
         * 'Proto.Scip_Fields.vec'arguments' @:: Lens' ToolInfo (Data.Vector.Vector Data.Text.Text)@ -}
data ToolInfo
  = ToolInfo'_constructor {_ToolInfo'name :: !Data.Text.Text,
                           _ToolInfo'version :: !Data.Text.Text,
                           _ToolInfo'arguments :: !(Data.Vector.Vector Data.Text.Text),
                           _ToolInfo'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ToolInfo where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ToolInfo "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ToolInfo'name (\ x__ y__ -> x__ {_ToolInfo'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ToolInfo "version" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ToolInfo'version (\ x__ y__ -> x__ {_ToolInfo'version = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ToolInfo "arguments" [Data.Text.Text] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ToolInfo'arguments (\ x__ y__ -> x__ {_ToolInfo'arguments = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ToolInfo "vec'arguments" (Data.Vector.Vector Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ToolInfo'arguments (\ x__ y__ -> x__ {_ToolInfo'arguments = y__}))
        Prelude.id
instance Data.ProtoLens.Message ToolInfo where
  messageName _ = Data.Text.pack "scip.ToolInfo"
  packedMessageDescriptor _
    = "\n\
      \\bToolInfo\DC2\DC2\n\
      \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2\CAN\n\
      \\aversion\CAN\STX \SOH(\tR\aversion\DC2\FS\n\
      \\targuments\CAN\ETX \ETX(\tR\targuments"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor ToolInfo
        version__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "version"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"version")) ::
              Data.ProtoLens.FieldDescriptor ToolInfo
        arguments__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "arguments"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"arguments")) ::
              Data.ProtoLens.FieldDescriptor ToolInfo
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, name__field_descriptor),
           (Data.ProtoLens.Tag 2, version__field_descriptor),
           (Data.ProtoLens.Tag 3, arguments__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ToolInfo'_unknownFields
        (\ x__ y__ -> x__ {_ToolInfo'_unknownFields = y__})
  defMessage
    = ToolInfo'_constructor
        {_ToolInfo'name = Data.ProtoLens.fieldDefault,
         _ToolInfo'version = Data.ProtoLens.fieldDefault,
         _ToolInfo'arguments = Data.Vector.Generic.empty,
         _ToolInfo'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ToolInfo
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Text.Text
             -> Data.ProtoLens.Encoding.Bytes.Parser ToolInfo
        loop x mutable'arguments
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'arguments <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                               mutable'arguments)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'arguments") frozen'arguments x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                                  mutable'arguments
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "version"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"version") y x)
                                  mutable'arguments
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                        Data.ProtoLens.Encoding.Bytes.getBytes
                                                          (Prelude.fromIntegral len)
                                            Data.ProtoLens.Encoding.Bytes.runEither
                                              (case Data.Text.Encoding.decodeUtf8' value of
                                                 (Prelude.Left err)
                                                   -> Prelude.Left (Prelude.show err)
                                                 (Prelude.Right r) -> Prelude.Right r))
                                        "arguments"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'arguments y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'arguments
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'arguments <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                     Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'arguments)
          "ToolInfo"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"version") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8 _v))
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.Text.Encoding.encodeUtf8 _v))
                      (Lens.Family2.view
                         (Data.ProtoLens.Field.field @"vec'arguments") _x))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData ToolInfo where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ToolInfo'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ToolInfo'name x__)
                (Control.DeepSeq.deepseq
                   (_ToolInfo'version x__)
                   (Control.DeepSeq.deepseq (_ToolInfo'arguments x__) ())))
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\n\
    \scip.proto\DC2\EOTscip\"\165\SOH\n\
    \\ENQIndex\DC2*\n\
    \\bmetadata\CAN\SOH \SOH(\v2\SO.scip.MetadataR\bmetadata\DC2,\n\
    \\tdocuments\CAN\STX \ETX(\v2\SO.scip.DocumentR\tdocuments\DC2B\n\
    \\DLEexternal_symbols\CAN\ETX \ETX(\v2\ETB.scip.SymbolInformationR\SIexternalSymbols\"\213\SOH\n\
    \\bMetadata\DC2/\n\
    \\aversion\CAN\SOH \SOH(\SO2\NAK.scip.ProtocolVersionR\aversion\DC2+\n\
    \\ttool_info\CAN\STX \SOH(\v2\SO.scip.ToolInfoR\btoolInfo\DC2!\n\
    \\fproject_root\CAN\ETX \SOH(\tR\vprojectRoot\DC2H\n\
    \\SYNtext_document_encoding\CAN\EOT \SOH(\SO2\DC2.scip.TextEncodingR\DC4textDocumentEncoding\"V\n\
    \\bToolInfo\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2\CAN\n\
    \\aversion\CAN\STX \SOH(\tR\aversion\DC2\FS\n\
    \\targuments\CAN\ETX \ETX(\tR\targuments\"\139\STX\n\
    \\bDocument\DC2\SUB\n\
    \\blanguage\CAN\EOT \SOH(\tR\blanguage\DC2#\n\
    \\rrelative_path\CAN\SOH \SOH(\tR\frelativePath\DC22\n\
    \\voccurrences\CAN\STX \ETX(\v2\DLE.scip.OccurrenceR\voccurrences\DC21\n\
    \\asymbols\CAN\ETX \ETX(\v2\ETB.scip.SymbolInformationR\asymbols\DC2\DC2\n\
    \\EOTtext\CAN\ENQ \SOH(\tR\EOTtext\DC2C\n\
    \\DC1position_encoding\CAN\ACK \SOH(\SO2\SYN.scip.PositionEncodingR\DLEpositionEncoding\"}\n\
    \\ACKSymbol\DC2\SYN\n\
    \\ACKscheme\CAN\SOH \SOH(\tR\ACKscheme\DC2'\n\
    \\apackage\CAN\STX \SOH(\v2\r.scip.PackageR\apackage\DC22\n\
    \\vdescriptors\CAN\ETX \ETX(\v2\DLE.scip.DescriptorR\vdescriptors\"Q\n\
    \\aPackage\DC2\CAN\n\
    \\amanager\CAN\SOH \SOH(\tR\amanager\DC2\DC2\n\
    \\EOTname\CAN\STX \SOH(\tR\EOTname\DC2\CAN\n\
    \\aversion\CAN\ETX \SOH(\tR\aversion\"\159\STX\n\
    \\n\
    \Descriptor\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2$\n\
    \\rdisambiguator\CAN\STX \SOH(\tR\rdisambiguator\DC2/\n\
    \\ACKsuffix\CAN\ETX \SOH(\SO2\ETB.scip.Descriptor.SuffixR\ACKsuffix\"\165\SOH\n\
    \\ACKSuffix\DC2\NAK\n\
    \\DC1UnspecifiedSuffix\DLE\NUL\DC2\r\n\
    \\tNamespace\DLE\SOH\DC2\SI\n\
    \\aPackage\DLE\SOH\SUB\STX\b\SOH\DC2\b\n\
    \\EOTType\DLE\STX\DC2\b\n\
    \\EOTTerm\DLE\ETX\DC2\n\
    \\n\
    \\ACKMethod\DLE\EOT\DC2\DC1\n\
    \\rTypeParameter\DLE\ENQ\DC2\r\n\
    \\tParameter\DLE\ACK\DC2\b\n\
    \\EOTMeta\DLE\a\DC2\t\n\
    \\ENQLocal\DLE\b\DC2\t\n\
    \\ENQMacro\DLE\t\SUB\STX\DLE\SOH\"\210\f\n\
    \\DC1SymbolInformation\DC2\SYN\n\
    \\ACKsymbol\CAN\SOH \SOH(\tR\ACKsymbol\DC2$\n\
    \\rdocumentation\CAN\ETX \ETX(\tR\rdocumentation\DC28\n\
    \\rrelationships\CAN\EOT \ETX(\v2\DC2.scip.RelationshipR\rrelationships\DC20\n\
    \\EOTkind\CAN\ENQ \SOH(\SO2\FS.scip.SymbolInformation.KindR\EOTkind\DC2!\n\
    \\fdisplay_name\CAN\ACK \SOH(\tR\vdisplayName\DC2G\n\
    \\ETBsignature_documentation\CAN\a \SOH(\v2\SO.scip.DocumentR\SYNsignatureDocumentation\DC2)\n\
    \\DLEenclosing_symbol\CAN\b \SOH(\tR\SIenclosingSymbol\"\251\t\n\
    \\EOTKind\DC2\DC3\n\
    \\SIUnspecifiedKind\DLE\NUL\DC2\DC2\n\
    \\SOAbstractMethod\DLEB\DC2\f\n\
    \\bAccessor\DLEH\DC2\t\n\
    \\ENQArray\DLE\SOH\DC2\r\n\
    \\tAssertion\DLE\STX\DC2\DC2\n\
    \\SOAssociatedType\DLE\ETX\DC2\r\n\
    \\tAttribute\DLE\EOT\DC2\t\n\
    \\ENQAxiom\DLE\ENQ\DC2\v\n\
    \\aBoolean\DLE\ACK\DC2\t\n\
    \\ENQClass\DLE\a\DC2\v\n\
    \\aConcept\DLEV\DC2\f\n\
    \\bConstant\DLE\b\DC2\SI\n\
    \\vConstructor\DLE\t\DC2\f\n\
    \\bContract\DLE>\DC2\SO\n\
    \\n\
    \DataFamily\DLE\n\
    \\DC2\f\n\
    \\bDelegate\DLEI\DC2\b\n\
    \\EOTEnum\DLE\v\DC2\SO\n\
    \\n\
    \EnumMember\DLE\f\DC2\t\n\
    \\ENQError\DLE?\DC2\t\n\
    \\ENQEvent\DLE\r\DC2\r\n\
    \\tExtension\DLET\DC2\b\n\
    \\EOTFact\DLE\SO\DC2\t\n\
    \\ENQField\DLE\SI\DC2\b\n\
    \\EOTFile\DLE\DLE\DC2\f\n\
    \\bFunction\DLE\DC1\DC2\n\
    \\n\
    \\ACKGetter\DLE\DC2\DC2\v\n\
    \\aGrammar\DLE\DC3\DC2\f\n\
    \\bInstance\DLE\DC4\DC2\r\n\
    \\tInterface\DLE\NAK\DC2\a\n\
    \\ETXKey\DLE\SYN\DC2\b\n\
    \\EOTLang\DLE\ETB\DC2\t\n\
    \\ENQLemma\DLE\CAN\DC2\v\n\
    \\aLibrary\DLE@\DC2\t\n\
    \\ENQMacro\DLE\EM\DC2\n\
    \\n\
    \\ACKMethod\DLE\SUB\DC2\SI\n\
    \\vMethodAlias\DLEJ\DC2\DC2\n\
    \\SOMethodReceiver\DLE\ESC\DC2\ETB\n\
    \\DC3MethodSpecification\DLEC\DC2\v\n\
    \\aMessage\DLE\FS\DC2\t\n\
    \\ENQMixin\DLEU\DC2\f\n\
    \\bModifier\DLEA\DC2\n\
    \\n\
    \\ACKModule\DLE\GS\DC2\r\n\
    \\tNamespace\DLE\RS\DC2\b\n\
    \\EOTNull\DLE\US\DC2\n\
    \\n\
    \\ACKNumber\DLE \DC2\n\
    \\n\
    \\ACKObject\DLE!\DC2\f\n\
    \\bOperator\DLE\"\DC2\v\n\
    \\aPackage\DLE#\DC2\DC1\n\
    \\rPackageObject\DLE$\DC2\r\n\
    \\tParameter\DLE%\DC2\DC2\n\
    \\SOParameterLabel\DLE&\DC2\v\n\
    \\aPattern\DLE'\DC2\r\n\
    \\tPredicate\DLE(\DC2\f\n\
    \\bProperty\DLE)\DC2\f\n\
    \\bProtocol\DLE*\DC2\DC2\n\
    \\SOProtocolMethod\DLED\DC2\NAK\n\
    \\DC1PureVirtualMethod\DLEE\DC2\SI\n\
    \\vQuasiquoter\DLE+\DC2\DC1\n\
    \\rSelfParameter\DLE,\DC2\n\
    \\n\
    \\ACKSetter\DLE-\DC2\r\n\
    \\tSignature\DLE.\DC2\DC2\n\
    \\SOSingletonClass\DLEK\DC2\DC3\n\
    \\SISingletonMethod\DLEL\DC2\DC4\n\
    \\DLEStaticDataMember\DLEM\DC2\SI\n\
    \\vStaticEvent\DLEN\DC2\SI\n\
    \\vStaticField\DLEO\DC2\DLE\n\
    \\fStaticMethod\DLEP\DC2\DC2\n\
    \\SOStaticProperty\DLEQ\DC2\DC2\n\
    \\SOStaticVariable\DLER\DC2\n\
    \\n\
    \\ACKString\DLE0\DC2\n\
    \\n\
    \\ACKStruct\DLE1\DC2\r\n\
    \\tSubscript\DLE/\DC2\n\
    \\n\
    \\ACKTactic\DLE2\DC2\v\n\
    \\aTheorem\DLE3\DC2\DC1\n\
    \\rThisParameter\DLE4\DC2\t\n\
    \\ENQTrait\DLE5\DC2\SI\n\
    \\vTraitMethod\DLEF\DC2\b\n\
    \\EOTType\DLE6\DC2\r\n\
    \\tTypeAlias\DLE7\DC2\r\n\
    \\tTypeClass\DLE8\DC2\DC3\n\
    \\SITypeClassMethod\DLEG\DC2\SO\n\
    \\n\
    \TypeFamily\DLE9\DC2\DC1\n\
    \\rTypeParameter\DLE:\DC2\t\n\
    \\ENQUnion\DLE;\DC2\t\n\
    \\ENQValue\DLE<\DC2\f\n\
    \\bVariable\DLE=\"\201\SOH\n\
    \\fRelationship\DC2\SYN\n\
    \\ACKsymbol\CAN\SOH \SOH(\tR\ACKsymbol\DC2!\n\
    \\fis_reference\CAN\STX \SOH(\bR\visReference\DC2+\n\
    \\DC1is_implementation\CAN\ETX \SOH(\bR\DLEisImplementation\DC2,\n\
    \\DC2is_type_definition\CAN\EOT \SOH(\bR\DLEisTypeDefinition\DC2#\n\
    \\ris_definition\CAN\ENQ \SOH(\bR\fisDefinition\"\164\STX\n\
    \\n\
    \Occurrence\DC2\DC4\n\
    \\ENQrange\CAN\SOH \ETX(\ENQR\ENQrange\DC2\SYN\n\
    \\ACKsymbol\CAN\STX \SOH(\tR\ACKsymbol\DC2!\n\
    \\fsymbol_roles\CAN\ETX \SOH(\ENQR\vsymbolRoles\DC25\n\
    \\SYNoverride_documentation\CAN\EOT \ETX(\tR\NAKoverrideDocumentation\DC21\n\
    \\vsyntax_kind\CAN\ENQ \SOH(\SO2\DLE.scip.SyntaxKindR\n\
    \syntaxKind\DC22\n\
    \\vdiagnostics\CAN\ACK \ETX(\v2\DLE.scip.DiagnosticR\vdiagnostics\DC2'\n\
    \\SIenclosing_range\CAN\a \ETX(\ENQR\SOenclosingRange\"\167\SOH\n\
    \\n\
    \Diagnostic\DC2*\n\
    \\bseverity\CAN\SOH \SOH(\SO2\SO.scip.SeverityR\bseverity\DC2\DC2\n\
    \\EOTcode\CAN\STX \SOH(\tR\EOTcode\DC2\CAN\n\
    \\amessage\CAN\ETX \SOH(\tR\amessage\DC2\SYN\n\
    \\ACKsource\CAN\EOT \SOH(\tR\ACKsource\DC2'\n\
    \\EOTtags\CAN\ENQ \ETX(\SO2\DC3.scip.DiagnosticTagR\EOTtags*1\n\
    \\SIProtocolVersion\DC2\RS\n\
    \\SUBUnspecifiedProtocolVersion\DLE\NUL*@\n\
    \\fTextEncoding\DC2\ESC\n\
    \\ETBUnspecifiedTextEncoding\DLE\NUL\DC2\b\n\
    \\EOTUTF8\DLE\SOH\DC2\t\n\
    \\ENQUTF16\DLE\STX*\164\SOH\n\
    \\DLEPositionEncoding\DC2\US\n\
    \\ESCUnspecifiedPositionEncoding\DLE\NUL\DC2#\n\
    \\USUTF8CodeUnitOffsetFromLineStart\DLE\SOH\DC2$\n\
    \ UTF16CodeUnitOffsetFromLineStart\DLE\STX\DC2$\n\
    \ UTF32CodeUnitOffsetFromLineStart\DLE\ETX*\148\SOH\n\
    \\n\
    \SymbolRole\DC2\EM\n\
    \\NAKUnspecifiedSymbolRole\DLE\NUL\DC2\SO\n\
    \\n\
    \Definition\DLE\SOH\DC2\n\
    \\n\
    \\ACKImport\DLE\STX\DC2\SI\n\
    \\vWriteAccess\DLE\EOT\DC2\SO\n\
    \\n\
    \ReadAccess\DLE\b\DC2\r\n\
    \\tGenerated\DLE\DLE\DC2\b\n\
    \\EOTTest\DLE \DC2\NAK\n\
    \\DC1ForwardDefinition\DLE@*\234\ACK\n\
    \\n\
    \SyntaxKind\DC2\EM\n\
    \\NAKUnspecifiedSyntaxKind\DLE\NUL\DC2\v\n\
    \\aComment\DLE\SOH\DC2\CAN\n\
    \\DC4PunctuationDelimiter\DLE\STX\DC2\SYN\n\
    \\DC2PunctuationBracket\DLE\ETX\DC2\v\n\
    \\aKeyword\DLE\EOT\DC2\EM\n\
    \\DC1IdentifierKeyword\DLE\EOT\SUB\STX\b\SOH\DC2\SYN\n\
    \\DC2IdentifierOperator\DLE\ENQ\DC2\SO\n\
    \\n\
    \Identifier\DLE\ACK\DC2\NAK\n\
    \\DC1IdentifierBuiltin\DLE\a\DC2\DC2\n\
    \\SOIdentifierNull\DLE\b\DC2\SYN\n\
    \\DC2IdentifierConstant\DLE\t\DC2\ESC\n\
    \\ETBIdentifierMutableGlobal\DLE\n\
    \\DC2\ETB\n\
    \\DC3IdentifierParameter\DLE\v\DC2\DC3\n\
    \\SIIdentifierLocal\DLE\f\DC2\SYN\n\
    \\DC2IdentifierShadowed\DLE\r\DC2\ETB\n\
    \\DC3IdentifierNamespace\DLE\SO\DC2\CAN\n\
    \\DLEIdentifierModule\DLE\SO\SUB\STX\b\SOH\DC2\SYN\n\
    \\DC2IdentifierFunction\DLE\SI\DC2 \n\
    \\FSIdentifierFunctionDefinition\DLE\DLE\DC2\DC3\n\
    \\SIIdentifierMacro\DLE\DC1\DC2\GS\n\
    \\EMIdentifierMacroDefinition\DLE\DC2\DC2\DC2\n\
    \\SOIdentifierType\DLE\DC3\DC2\EM\n\
    \\NAKIdentifierBuiltinType\DLE\DC4\DC2\ETB\n\
    \\DC3IdentifierAttribute\DLE\NAK\DC2\SI\n\
    \\vRegexEscape\DLE\SYN\DC2\DC1\n\
    \\rRegexRepeated\DLE\ETB\DC2\DC1\n\
    \\rRegexWildcard\DLE\CAN\DC2\DC2\n\
    \\SORegexDelimiter\DLE\EM\DC2\r\n\
    \\tRegexJoin\DLE\SUB\DC2\DC1\n\
    \\rStringLiteral\DLE\ESC\DC2\ETB\n\
    \\DC3StringLiteralEscape\DLE\FS\DC2\CAN\n\
    \\DC4StringLiteralSpecial\DLE\GS\DC2\DC4\n\
    \\DLEStringLiteralKey\DLE\RS\DC2\DC4\n\
    \\DLECharacterLiteral\DLE\US\DC2\DC2\n\
    \\SONumericLiteral\DLE \DC2\DC2\n\
    \\SOBooleanLiteral\DLE!\DC2\a\n\
    \\ETXTag\DLE\"\DC2\DLE\n\
    \\fTagAttribute\DLE#\DC2\DLE\n\
    \\fTagDelimiter\DLE$\SUB\STX\DLE\SOH*V\n\
    \\bSeverity\DC2\ETB\n\
    \\DC3UnspecifiedSeverity\DLE\NUL\DC2\t\n\
    \\ENQError\DLE\SOH\DC2\v\n\
    \\aWarning\DLE\STX\DC2\SI\n\
    \\vInformation\DLE\ETX\DC2\b\n\
    \\EOTHint\DLE\EOT*N\n\
    \\rDiagnosticTag\DC2\FS\n\
    \\CANUnspecifiedDiagnosticTag\DLE\NUL\DC2\SI\n\
    \\vUnnecessary\DLE\SOH\DC2\SO\n\
    \\n\
    \Deprecated\DLE\STX*\155\n\
    \\n\
    \\bLanguage\DC2\ETB\n\
    \\DC3UnspecifiedLanguage\DLE\NUL\DC2\b\n\
    \\EOTABAP\DLE<\DC2\b\n\
    \\EOTApex\DLE`\DC2\a\n\
    \\ETXAPL\DLE1\DC2\a\n\
    \\ETXAda\DLE'\DC2\b\n\
    \\EOTAgda\DLE-\DC2\f\n\
    \\bAsciiDoc\DLEV\DC2\f\n\
    \\bAssembly\DLE:\DC2\a\n\
    \\ETXAwk\DLEB\DC2\a\n\
    \\ETXBat\DLED\DC2\n\
    \\n\
    \\ACKBibTeX\DLEQ\DC2\ENQ\n\
    \\SOHC\DLE\"\DC2\t\n\
    \\ENQCOBOL\DLE;\DC2\a\n\
    \\ETXCPP\DLE#\DC2\a\n\
    \\ETXCSS\DLE\SUB\DC2\n\
    \\n\
    \\ACKCSharp\DLE\SOH\DC2\v\n\
    \\aClojure\DLE\b\DC2\DLE\n\
    \\fCoffeescript\DLE\NAK\DC2\SO\n\
    \\n\
    \CommonLisp\DLE\t\DC2\a\n\
    \\ETXCoq\DLE/\DC2\b\n\
    \\EOTCUDA\DLEa\DC2\b\n\
    \\EOTDart\DLE\ETX\DC2\n\
    \\n\
    \\ACKDelphi\DLE9\DC2\b\n\
    \\EOTDiff\DLEX\DC2\SO\n\
    \\n\
    \Dockerfile\DLEP\DC2\n\
    \\n\
    \\ACKDyalog\DLE2\DC2\n\
    \\n\
    \\ACKElixir\DLE\DC1\DC2\n\
    \\n\
    \\ACKErlang\DLE\DC2\DC2\n\
    \\n\
    \\ACKFSharp\DLE*\DC2\b\n\
    \\EOTFish\DLEA\DC2\b\n\
    \\EOTFlow\DLE\CAN\DC2\v\n\
    \\aFortran\DLE8\DC2\SO\n\
    \\n\
    \Git_Commit\DLE[\DC2\SO\n\
    \\n\
    \Git_Config\DLEY\DC2\SO\n\
    \\n\
    \Git_Rebase\DLE\\\DC2\ACK\n\
    \\STXGo\DLE!\DC2\v\n\
    \\aGraphQL\DLEb\DC2\n\
    \\n\
    \\ACKGroovy\DLE\a\DC2\b\n\
    \\EOTHTML\DLE\RS\DC2\b\n\
    \\EOTHack\DLE\DC4\DC2\SO\n\
    \\n\
    \Handlebars\DLEZ\DC2\v\n\
    \\aHaskell\DLE,\DC2\t\n\
    \\ENQIdris\DLE.\DC2\a\n\
    \\ETXIni\DLEH\DC2\ENQ\n\
    \\SOHJ\DLE3\DC2\b\n\
    \\EOTJSON\DLEK\DC2\b\n\
    \\EOTJava\DLE\ACK\DC2\SO\n\
    \\n\
    \JavaScript\DLE\SYN\DC2\DC3\n\
    \\SIJavaScriptReact\DLE]\DC2\v\n\
    \\aJsonnet\DLEL\DC2\t\n\
    \\ENQJulia\DLE7\DC2\f\n\
    \\bJustfile\DLEm\DC2\n\
    \\n\
    \\ACKKotlin\DLE\EOT\DC2\t\n\
    \\ENQLaTeX\DLES\DC2\b\n\
    \\EOTLean\DLE0\DC2\b\n\
    \\EOTLess\DLE\ESC\DC2\a\n\
    \\ETXLua\DLE\f\DC2\b\n\
    \\EOTLuau\DLEl\DC2\f\n\
    \\bMakefile\DLEO\DC2\f\n\
    \\bMarkdown\DLET\DC2\n\
    \\n\
    \\ACKMatlab\DLE4\DC2\n\
    \\n\
    \\ACKNickel\DLEn\DC2\a\n\
    \\ETXNix\DLEM\DC2\t\n\
    \\ENQOCaml\DLE)\DC2\SI\n\
    \\vObjective_C\DLE$\DC2\DC1\n\
    \\rObjective_CPP\DLE%\DC2\n\
    \\n\
    \\ACKPascal\DLEc\DC2\a\n\
    \\ETXPHP\DLE\DC3\DC2\t\n\
    \\ENQPLSQL\DLEF\DC2\b\n\
    \\EOTPerl\DLE\r\DC2\SO\n\
    \\n\
    \PowerShell\DLEC\DC2\n\
    \\n\
    \\ACKProlog\DLEG\DC2\f\n\
    \\bProtobuf\DLEd\DC2\n\
    \\n\
    \\ACKPython\DLE\SI\DC2\ENQ\n\
    \\SOHR\DLE6\DC2\n\
    \\n\
    \\ACKRacket\DLE\v\DC2\b\n\
    \\EOTRaku\DLE\SO\DC2\t\n\
    \\ENQRazor\DLE>\DC2\t\n\
    \\ENQRepro\DLEf\DC2\b\n\
    \\EOTReST\DLEU\DC2\b\n\
    \\EOTRuby\DLE\DLE\DC2\b\n\
    \\EOTRust\DLE(\DC2\a\n\
    \\ETXSAS\DLE=\DC2\b\n\
    \\EOTSCSS\DLE\GS\DC2\a\n\
    \\ETXSML\DLE+\DC2\a\n\
    \\ETXSQL\DLEE\DC2\b\n\
    \\EOTSass\DLE\FS\DC2\t\n\
    \\ENQScala\DLE\ENQ\DC2\n\
    \\n\
    \\ACKScheme\DLE\n\
    \\DC2\SI\n\
    \\vShellScript\DLE@\DC2\v\n\
    \\aSkylark\DLEN\DC2\t\n\
    \\ENQSlang\DLEk\DC2\f\n\
    \\bSolidity\DLE_\DC2\n\
    \\n\
    \\ACKSvelte\DLEj\DC2\t\n\
    \\ENQSwift\DLE\STX\DC2\a\n\
    \\ETXTcl\DLEe\DC2\b\n\
    \\EOTTOML\DLEI\DC2\a\n\
    \\ETXTeX\DLER\DC2\n\
    \\n\
    \\ACKThrift\DLEg\DC2\SO\n\
    \\n\
    \TypeScript\DLE\ETB\DC2\DC3\n\
    \\SITypeScriptReact\DLE^\DC2\v\n\
    \\aVerilog\DLEh\DC2\b\n\
    \\EOTVHDL\DLEi\DC2\SI\n\
    \\vVisualBasic\DLE?\DC2\a\n\
    \\ETXVue\DLE\EM\DC2\v\n\
    \\aWolfram\DLE5\DC2\a\n\
    \\ETXXML\DLE\US\DC2\a\n\
    \\ETXXSL\DLE \DC2\b\n\
    \\EOTYAML\DLEJ\DC2\a\n\
    \\ETXZig\DLE&B/Z-github.com/sourcegraph/scip/bindings/go/scip/J\139\185\STX\n\
    \\a\DC2\ENQ\n\
    \\NUL\244\ACK\SOH\n\
    \\130\EOT\n\
    \\SOH\f\DC2\ETX\n\
    \\NUL\DC22\247\ETX An index contains one or more pieces of information about a given piece of\n\
    \ source code or software artifact. Complementary information can be merged\n\
    \ together from multiple sources to provide a unified code intelligence\n\
    \ experience.\n\
    \\n\
    \ Programs producing a file of this format is an \"indexer\" and may operate\n\
    \ somewhere on the spectrum between precision, such as indexes produced by\n\
    \ compiler-backed indexers, and heurstics, such as indexes produced by local\n\
    \ syntax-directed analysis for scope rules.\n\
    \\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\f\NUL\r\n\
    \\b\n\
    \\SOH\b\DC2\ETX\SO\NULD\n\
    \\t\n\
    \\STX\b\v\DC2\ETX\SO\NULD\n\
    \\208\ETX\n\
    \\STX\EOT\NUL\DC2\EOT\SYN\NUL#\SOH\SUB\195\ETX Index represents a complete SCIP index for a workspace this is rooted at a\n\
    \ single directory. An Index message payload can have a large memory footprint\n\
    \ and it's therefore recommended to emit and consume an Index payload one field\n\
    \ value at a time. To permit streaming consumption of an Index payload, the\n\
    \ `metadata` field must appear at the start of the stream and must only appear\n\
    \ once in the stream. Other field values may appear in any order.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\SYN\b\r\n\
    \)\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\CAN\STX\CAN\SUB\FS Metadata about this index.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ACK\DC2\ETX\CAN\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\CAN\v\DC3\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\CAN\SYN\ETB\n\
    \3\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETX\SUB\STX\"\SUB& Documents that belong to this index.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\EOT\DC2\ETX\SUB\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ACK\DC2\ETX\SUB\v\DC3\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETX\SUB\DC4\GS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETX\SUB !\n\
    \\246\ETX\n\
    \\EOT\EOT\NUL\STX\STX\DC2\ETX \STX2\SUB\233\STX (optional) Symbols that are referenced from this index but are defined in\n\
    \ an external package (a separate `Index` message). Leave this field empty\n\
    \ if you assume the external package will get indexed separately. If the\n\
    \ external package won't get indexed for some reason then you can use this\n\
    \ field to provide hover documentation for those external symbols.\n\
    \\"} IMPORTANT: When adding a new field to `Index` here, add a matching\n\
    \ function in `IndexVisitor` and update `ParseStreaming`.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\EOT\DC2\ETX \STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ACK\DC2\ETX \v\FS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\SOH\DC2\ETX \GS-\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ETX\DC2\ETX 01\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT%\NUL2\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX%\b\DLE\n\
    \N\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX'\STX\RS\SUBA Which version of this protocol was used to generate this index?\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ACK\DC2\ETX'\STX\DC1\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX'\DC2\EM\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX'\FS\GS\n\
    \C\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETX)\STX\EM\SUB6 Information about the tool that produced this index.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ACK\DC2\ETX)\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETX)\v\DC4\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETX)\ETB\CAN\n\
    \\162\SOH\n\
    \\EOT\EOT\SOH\STX\STX\DC2\ETX-\STX\SUB\SUB\148\SOH URI-encoded absolute path to the root directory of this index. All\n\
    \ documents in this index must appear in a subdirectory of this root\n\
    \ directory.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ENQ\DC2\ETX-\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\SOH\DC2\ETX-\t\NAK\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ETX\DC2\ETX-\CAN\EM\n\
    \\224\SOH\n\
    \\EOT\EOT\SOH\STX\ETX\DC2\ETX1\STX*\SUB\210\SOH Text encoding of the source files on disk that are referenced from\n\
    \ `Document.relative_path`. This value is unrelated to the `Document.text`\n\
    \ field, which is a Protobuf string and hence must be UTF-8 encoded.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\ETX\ACK\DC2\ETX1\STX\SO\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\ETX\SOH\DC2\ETX1\SI%\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\ETX\ETX\DC2\ETX1()\n\
    \\n\
    \\n\
    \\STX\ENQ\NUL\DC2\EOT4\NUL6\SOH\n\
    \\n\
    \\n\
    \\ETX\ENQ\NUL\SOH\DC2\ETX4\ENQ\DC4\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\NUL\DC2\ETX5\STX!\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\NUL\SOH\DC2\ETX5\STX\FS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\NUL\STX\DC2\ETX5\US \n\
    \\n\
    \\n\
    \\STX\ENQ\SOH\DC2\EOT8\NUL<\SOH\n\
    \\n\
    \\n\
    \\ETX\ENQ\SOH\SOH\DC2\ETX8\ENQ\DC1\n\
    \\v\n\
    \\EOT\ENQ\SOH\STX\NUL\DC2\ETX9\STX\RS\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\NUL\SOH\DC2\ETX9\STX\EM\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\NUL\STX\DC2\ETX9\FS\GS\n\
    \\v\n\
    \\EOT\ENQ\SOH\STX\SOH\DC2\ETX:\STX\v\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\SOH\SOH\DC2\ETX:\STX\ACK\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\SOH\STX\DC2\ETX:\t\n\
    \\n\
    \\v\n\
    \\EOT\ENQ\SOH\STX\STX\DC2\ETX;\STX\f\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\STX\SOH\DC2\ETX;\STX\a\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\STX\STX\DC2\ETX;\n\
    \\v\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT>\NULE\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX>\b\DLE\n\
    \<\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX@\STX\DC2\SUB/ Name of the indexer that produced this index.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETX@\STX\b\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX@\t\r\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX@\DLE\DC1\n\
    \?\n\
    \\EOT\EOT\STX\STX\SOH\DC2\ETXB\STX\NAK\SUB2 Version of the indexer that produced this index.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ENQ\DC2\ETXB\STX\b\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\ETXB\t\DLE\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\ETXB\DC3\DC4\n\
    \L\n\
    \\EOT\EOT\STX\STX\STX\DC2\ETXD\STX \SUB? Command-line arguments that were used to invoke this indexer.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\EOT\DC2\ETXD\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\ENQ\DC2\ETXD\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\SOH\DC2\ETXD\DC2\ESC\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\ETX\DC2\ETXD\RS\US\n\
    \H\n\
    \\STX\EOT\ETX\DC2\EOTH\NULu\SOH\SUB< Document defines the metadata about a source file on disk.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETXH\b\DLE\n\
    \\165\STX\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETXM\STX\SYN\SUB\151\STX The string ID for the programming language this file is written in.\n\
    \ The `Language` enum contains the names of most common programming languages.\n\
    \ This field is typed as a string to permit any programming language, including\n\
    \ ones that are not specified by the `Language` enum.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ENQ\DC2\ETXM\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETXM\t\DC1\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETXM\DC4\NAK\n\
    \\181\ETX\n\
    \\EOT\EOT\ETX\STX\SOH\DC2\ETXW\STX\ESC\SUB\167\ETX (Required) Unique path to the text document.\n\
    \\n\
    \ 1. The path must be relative to the directory supplied in the associated\n\
    \    `Metadata.project_root`.\n\
    \ 2. The path must not begin with a leading '/'.\n\
    \ 3. The path must point to a regular file, not a symbolic link.\n\
    \ 4. The path must use '/' as the separator, including on Windows.\n\
    \ 5. The path must be canonical; it cannot include empty components ('//'),\n\
    \    or '.' or '..'.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ENQ\DC2\ETXW\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\SOH\DC2\ETXW\t\SYN\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ETX\DC2\ETXW\EM\SUB\n\
    \4\n\
    \\EOT\EOT\ETX\STX\STX\DC2\ETXY\STX&\SUB' Occurrences that appear in this file.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\EOT\DC2\ETXY\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\ACK\DC2\ETXY\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\SOH\DC2\ETXY\SYN!\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\ETX\DC2\ETXY$%\n\
    \\234\SOH\n\
    \\EOT\EOT\ETX\STX\ETX\DC2\ETX_\STX)\SUB\220\SOH Symbols that are \"defined\" within this document.\n\
    \\n\
    \ This should include symbols which technically do not have any definition,\n\
    \ but have a reference and are defined by some other symbol (see\n\
    \ Relationship.is_definition).\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ETX\EOT\DC2\ETX_\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ETX\ACK\DC2\ETX_\v\FS\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ETX\SOH\DC2\ETX_\GS$\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ETX\ETX\DC2\ETX_'(\n\
    \\247\ETX\n\
    \\EOT\EOT\ETX\STX\EOT\DC2\ETXh\STX\DC2\SUB\233\ETX (optional) Text contents of the this document. Indexers are not expected to\n\
    \ include the text by default. It's preferrable that clients read the text\n\
    \ contents from the file system by resolving the absolute path from joining\n\
    \ `Index.metadata.project_root` and `Document.relative_path`. This field was\n\
    \ introduced to support `SymbolInformation.signature_documentation`, but it\n\
    \ can be used for other purposes as well, for example testing or when working\n\
    \ with virtual/in-memory documents.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\EOT\ENQ\DC2\ETXh\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\EOT\SOH\DC2\ETXh\t\r\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\EOT\ETX\DC2\ETXh\DLE\DC1\n\
    \\231\ETX\n\
    \\EOT\EOT\ETX\STX\ENQ\DC2\ETXt\STX)\SUB\217\ETX Specifies the encoding used for source ranges in this Document.\n\
    \\n\
    \ Usually, this will match the type used to index the string type\n\
    \ in the indexer's implementation language in O(1) time.\n\
    \ - For an indexer implemented in JVM/.NET language or JavaScript/TypeScript,\n\
    \   use UTF16CodeUnitOffsetFromLineStart.\n\
    \ - For an indexer implemented in Python,\n\
    \   use UTF32CodeUnitOffsetFromLineStart.\n\
    \ - For an indexer implemented in Go, Rust or C++,\n\
    \   use UTF8ByteOffsetFromLineStart.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ENQ\ACK\DC2\ETXt\STX\DC2\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ENQ\SOH\DC2\ETXt\DC3$\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\ENQ\ETX\DC2\ETXt'(\n\
    \Q\n\
    \\STX\ENQ\STX\DC2\ENQx\NUL\144\SOH\SOH\SUBD Encoding used to interpret the 'character' value in source ranges.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\ENQ\STX\SOH\DC2\ETXx\ENQ\NAK\n\
    \\147\SOH\n\
    \\EOT\ENQ\STX\STX\NUL\DC2\ETX{\STX\"\SUB\133\SOH Default value. This value should not be used by new SCIP indexers\n\
    \ so that a consumer can process the SCIP index without ambiguity.\n\
    \\n\
    \\f\n\
    \\ENQ\ENQ\STX\STX\NUL\SOH\DC2\ETX{\STX\GS\n\
    \\f\n\
    \\ENQ\ENQ\STX\STX\NUL\STX\DC2\ETX{ !\n\
    \\247\SOH\n\
    \\EOT\ENQ\STX\STX\SOH\DC2\EOT\130\SOH\STX&\SUB\232\SOH The 'character' value is interpreted as an offset in terms\n\
    \ of UTF-8 code units (i.e. bytes).\n\
    \\n\
    \ Example: For the string \"\240\159\154\128 Woo\" in UTF-8, the bytes are\n\
    \ [240, 159, 154, 128, 32, 87, 111, 111], so the offset for 'W'\n\
    \ would be 5.\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\SOH\SOH\DC2\EOT\130\SOH\STX!\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\SOH\STX\DC2\EOT\130\SOH$%\n\
    \\130\STX\n\
    \\EOT\ENQ\STX\STX\STX\DC2\EOT\137\SOH\STX'\SUB\243\SOH The 'character' value is interpreted as an offset in terms\n\
    \ of UTF-16 code units (each is 2 bytes).\n\
    \\n\
    \ Example: For the string \"\240\159\154\128 Woo\", the UTF-16 code units are\n\
    \ ['\\ud83d', '\\ude80', ' ', 'W', 'o', 'o'], so the offset for 'W'\n\
    \ would be 3.\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\STX\SOH\DC2\EOT\137\SOH\STX\"\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\STX\STX\DC2\EOT\137\SOH%&\n\
    \\245\SOH\n\
    \\EOT\ENQ\STX\STX\ETX\DC2\EOT\143\SOH\STX'\SUB\230\SOH The 'character' value is interpreted as an offset in terms\n\
    \ of UTF-32 code units (each is 4 bytes).\n\
    \\n\
    \ Example: For the string \"\240\159\154\128 Woo\", the UTF-32 code units are\n\
    \ ['\240\159\154\128', ' ', 'W', 'o', 'o'], so the offset for 'W' would be 2.\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\ETX\SOH\DC2\EOT\143\SOH\STX\"\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\ETX\STX\DC2\EOT\143\SOH%&\n\
    \\204\DC2\n\
    \\STX\EOT\EOT\DC2\ACK\188\SOH\NUL\192\SOH\SOH\SUB\189\DC2 Symbol is similar to a URI, it identifies a class, method, or a local\n\
    \ variable. `SymbolInformation` contains rich metadata about symbols such as\n\
    \ the docstring.\n\
    \\n\
    \ Symbol has a standardized string representation, which can be used\n\
    \ interchangeably with `Symbol`. The syntax for Symbol is the following:\n\
    \ ```\n\
    \ # (<x>)+ stands for one or more repetitions of <x>\n\
    \ # (<x>)? stands for zero or one occurrence of <x>\n\
    \ <symbol>               ::= <scheme> ' ' <package> ' ' (<descriptor>)+ | 'local ' <local-id>\n\
    \ <package>              ::= <manager> ' ' <package-name> ' ' <version>\n\
    \ <scheme>               ::= any UTF-8, escape spaces with double space. Must not be empty nor start with 'local'\n\
    \ <manager>              ::= any UTF-8, escape spaces with double space. Use the placeholder '.' to indicate an empty value\n\
    \ <package-name>         ::= same as above\n\
    \ <version>              ::= same as above\n\
    \ <descriptor>           ::= <namespace> | <type> | <term> | <method> | <type-parameter> | <parameter> | <meta> | <macro>\n\
    \ <namespace>            ::= <name> '/'\n\
    \ <type>                 ::= <name> '#'\n\
    \ <term>                 ::= <name> '.'\n\
    \ <meta>                 ::= <name> ':'\n\
    \ <macro>                ::= <name> '!'\n\
    \ <method>               ::= <name> '(' (<method-disambiguator>)? ').'\n\
    \ <type-parameter>       ::= '[' <name> ']'\n\
    \ <parameter>            ::= '(' <name> ')'\n\
    \ <name>                 ::= <identifier>\n\
    \ <method-disambiguator> ::= <simple-identifier>\n\
    \ <identifier>           ::= <simple-identifier> | <escaped-identifier>\n\
    \ <simple-identifier>    ::= (<identifier-character>)+\n\
    \ <identifier-character> ::= '_' | '+' | '-' | '$' | ASCII letter or digit\n\
    \ <escaped-identifier>   ::= '`' (<escaped-character>)+ '`', must contain at least one non-<identifier-character>\n\
    \ <escaped-characters>   ::= any UTF-8, escape backticks with double backtick.\n\
    \ <local-id>             ::= <simple-identifier>\n\
    \ ```\n\
    \\n\
    \ The list of descriptors for a symbol should together form a fully\n\
    \ qualified name for the symbol. That is, it should serve as a unique\n\
    \ identifier across the package. Typically, it will include one descriptor\n\
    \ for every node in the AST (along the ancestry path) between the root of\n\
    \ the file and the node corresponding to the symbol.\n\
    \\n\
    \ Local symbols MUST only be used for entities which are local to a Document,\n\
    \ and cannot be accessed from outside the Document.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\EOT\SOH\DC2\EOT\188\SOH\b\SO\n\
    \\f\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\EOT\189\SOH\STX\DC4\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\NUL\ENQ\DC2\EOT\189\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\EOT\189\SOH\t\SI\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\EOT\189\SOH\DC2\DC3\n\
    \\f\n\
    \\EOT\EOT\EOT\STX\SOH\DC2\EOT\190\SOH\STX\SYN\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\SOH\ACK\DC2\EOT\190\SOH\STX\t\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\SOH\SOH\DC2\EOT\190\SOH\n\
    \\DC1\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\SOH\ETX\DC2\EOT\190\SOH\DC4\NAK\n\
    \\f\n\
    \\EOT\EOT\EOT\STX\STX\DC2\EOT\191\SOH\STX&\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\STX\EOT\DC2\EOT\191\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\STX\ACK\DC2\EOT\191\SOH\v\NAK\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\STX\SOH\DC2\EOT\191\SOH\SYN!\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\STX\ETX\DC2\EOT\191\SOH$%\n\
    \q\n\
    \\STX\EOT\ENQ\DC2\ACK\197\SOH\NUL\201\SOH\SOH\SUBc Unit of packaging and distribution.\n\
    \\n\
    \ NOTE: This corresponds to a module in Go and JVM languages.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\ENQ\SOH\DC2\EOT\197\SOH\b\SI\n\
    \\f\n\
    \\EOT\EOT\ENQ\STX\NUL\DC2\EOT\198\SOH\STX\NAK\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\NUL\ENQ\DC2\EOT\198\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\NUL\SOH\DC2\EOT\198\SOH\t\DLE\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\NUL\ETX\DC2\EOT\198\SOH\DC3\DC4\n\
    \\f\n\
    \\EOT\EOT\ENQ\STX\SOH\DC2\EOT\199\SOH\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\SOH\ENQ\DC2\EOT\199\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\SOH\SOH\DC2\EOT\199\SOH\t\r\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\SOH\ETX\DC2\EOT\199\SOH\DLE\DC1\n\
    \\f\n\
    \\EOT\EOT\ENQ\STX\STX\DC2\EOT\200\SOH\STX\NAK\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\STX\ENQ\DC2\EOT\200\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\STX\SOH\DC2\EOT\200\SOH\t\DLE\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\STX\ETX\DC2\EOT\200\SOH\DC3\DC4\n\
    \\f\n\
    \\STX\EOT\ACK\DC2\ACK\203\SOH\NUL\228\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\ACK\SOH\DC2\EOT\203\SOH\b\DC2\n\
    \\SO\n\
    \\EOT\EOT\ACK\EOT\NUL\DC2\ACK\204\SOH\STX\222\SOH\ETX\n\
    \\r\n\
    \\ENQ\EOT\ACK\EOT\NUL\SOH\DC2\EOT\204\SOH\a\r\n\
    \\r\n\
    \\ENQ\EOT\ACK\EOT\NUL\ETX\DC2\EOT\205\SOH\EOT\RS\n\
    \\SO\n\
    \\ACK\EOT\ACK\EOT\NUL\ETX\STX\DC2\EOT\205\SOH\EOT\RS\n\
    \\SO\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\NUL\DC2\EOT\206\SOH\EOT\SUB\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\NUL\SOH\DC2\EOT\206\SOH\EOT\NAK\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\NUL\STX\DC2\EOT\206\SOH\CAN\EM\n\
    \}\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\SOH\DC2\EOT\210\SOH\EOT\DC2\SUBm Unit of code abstraction and/or namespacing.\n\
    \\n\
    \ NOTE: This corresponds to a package in Go and JVM languages.\n\
    \\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\SOH\SOH\DC2\EOT\210\SOH\EOT\r\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\SOH\STX\DC2\EOT\210\SOH\DLE\DC1\n\
    \(\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\STX\DC2\EOT\212\SOH\EOT\"\SUB\CAN Use Namespace instead.\n\
    \\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\STX\SOH\DC2\EOT\212\SOH\EOT\v\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\STX\STX\DC2\EOT\212\SOH\SO\SI\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\STX\ETX\DC2\EOT\212\SOH\DLE!\n\
    \\DLE\n\
    \\b\EOT\ACK\EOT\NUL\STX\STX\ETX\SOH\DC2\EOT\212\SOH\DC1 \n\
    \\SO\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\ETX\DC2\EOT\213\SOH\EOT\r\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\ETX\SOH\DC2\EOT\213\SOH\EOT\b\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\ETX\STX\DC2\EOT\213\SOH\v\f\n\
    \\SO\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\EOT\DC2\EOT\214\SOH\EOT\r\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\EOT\SOH\DC2\EOT\214\SOH\EOT\b\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\EOT\STX\DC2\EOT\214\SOH\v\f\n\
    \\SO\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\ENQ\DC2\EOT\215\SOH\EOT\SI\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\ENQ\SOH\DC2\EOT\215\SOH\EOT\n\
    \\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\ENQ\STX\DC2\EOT\215\SOH\r\SO\n\
    \\SO\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\ACK\DC2\EOT\216\SOH\EOT\SYN\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\ACK\SOH\DC2\EOT\216\SOH\EOT\DC1\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\ACK\STX\DC2\EOT\216\SOH\DC4\NAK\n\
    \\SO\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\a\DC2\EOT\217\SOH\EOT\DC2\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\a\SOH\DC2\EOT\217\SOH\EOT\r\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\a\STX\DC2\EOT\217\SOH\DLE\DC1\n\
    \.\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\b\DC2\EOT\219\SOH\EOT\r\SUB\RS Can be used for any purpose.\n\
    \\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\b\SOH\DC2\EOT\219\SOH\EOT\b\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\b\STX\DC2\EOT\219\SOH\v\f\n\
    \\SO\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\t\DC2\EOT\220\SOH\EOT\SO\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\t\SOH\DC2\EOT\220\SOH\EOT\t\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\t\STX\DC2\EOT\220\SOH\f\r\n\
    \\SO\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\n\
    \\DC2\EOT\221\SOH\EOT\SO\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\n\
    \\SOH\DC2\EOT\221\SOH\EOT\t\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\n\
    \\STX\DC2\EOT\221\SOH\f\r\n\
    \\f\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\EOT\223\SOH\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\NUL\ENQ\DC2\EOT\223\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\EOT\223\SOH\t\r\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\EOT\223\SOH\DLE\DC1\n\
    \\f\n\
    \\EOT\EOT\ACK\STX\SOH\DC2\EOT\224\SOH\STX\ESC\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\SOH\ENQ\DC2\EOT\224\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\SOH\SOH\DC2\EOT\224\SOH\t\SYN\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\SOH\ETX\DC2\EOT\224\SOH\EM\SUB\n\
    \\130\SOH\n\
    \\EOT\EOT\ACK\STX\STX\DC2\EOT\225\SOH\STX\DC4\"t NOTE: If you add new fields here, make sure to update the prepareSlot()\n\
    \ function responsible for parsing symbols.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\STX\ACK\DC2\EOT\225\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\STX\SOH\DC2\EOT\225\SOH\t\SI\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\STX\ETX\DC2\EOT\225\SOH\DC2\DC3\n\
    \\131\SOH\n\
    \\STX\EOT\a\DC2\ACK\232\SOH\NUL\186\ETX\SOH\SUBu SymbolInformation defines metadata about a symbol, such as the symbol's\n\
    \ docstring or what package it's defined it.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\a\SOH\DC2\EOT\232\SOH\b\EM\n\
    \\160\SOH\n\
    \\EOT\EOT\a\STX\NUL\DC2\EOT\235\SOH\STX\DC4\SUB\145\SOH Identifier of this symbol, which can be referenced from `Occurence.symbol`.\n\
    \ The string must be formatted according to the grammar in `Symbol`.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\NUL\ENQ\DC2\EOT\235\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\a\STX\NUL\SOH\DC2\EOT\235\SOH\t\SI\n\
    \\r\n\
    \\ENQ\EOT\a\STX\NUL\ETX\DC2\EOT\235\SOH\DC2\DC3\n\
    \\180\ETX\n\
    \\EOT\EOT\a\STX\SOH\DC2\EOT\242\SOH\STX$\SUB\165\ETX (optional, but strongly recommended) The markdown-formatted documentation\n\
    \ for this symbol. Use `SymbolInformation.signature_documentation` to\n\
    \ document the method/class/type signature of this symbol.\n\
    \ Due to historical reasons, indexers may include signature documentation in\n\
    \ this field by rendering markdown code blocks. New indexers should only\n\
    \ include non-code documentation in this field, for example docstrings.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\EOT\DC2\EOT\242\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\ENQ\DC2\EOT\242\SOH\v\DC1\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\SOH\DC2\EOT\242\SOH\DC2\US\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\ETX\DC2\EOT\242\SOH\"#\n\
    \^\n\
    \\EOT\EOT\a\STX\STX\DC2\EOT\244\SOH\STX*\SUBP (optional) Relationships to other symbols (e.g., implements, type definition).\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\STX\EOT\DC2\EOT\244\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\STX\ACK\DC2\EOT\244\SOH\v\ETB\n\
    \\r\n\
    \\ENQ\EOT\a\STX\STX\SOH\DC2\EOT\244\SOH\CAN%\n\
    \\r\n\
    \\ENQ\EOT\a\STX\STX\ETX\DC2\EOT\244\SOH()\n\
    \\164\SOH\n\
    \\EOT\EOT\a\STX\ETX\DC2\EOT\248\SOH\STX\DLE\SUB\149\SOH The kind of this symbol. Use this field instead of\n\
    \ `SymbolDescriptor.Suffix` to determine whether something is, for example, a\n\
    \ class or a method.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ETX\ACK\DC2\EOT\248\SOH\STX\ACK\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ETX\SOH\DC2\EOT\248\SOH\a\v\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ETX\ETX\DC2\EOT\248\SOH\SO\SI\n\
    \\245\ENQ\n\
    \\EOT\EOT\a\EOT\NUL\DC2\ACK\135\STX\STX\150\ETX\ETX\SUB\228\ENQ (optional) Kind represents the fine-grained category of a symbol, suitable for presenting\n\
    \ information about the symbol's meaning in the language.\n\
    \\n\
    \ For example:\n\
    \ - A Java method would have the kind `Method` while a Go function would\n\
    \   have the kind `Function`, even if the symbols for these use the same\n\
    \   syntax for the descriptor `SymbolDescriptor.Suffix.Method`.\n\
    \ - A Go struct has the symbol kind `Struct` while a Java class has\n\
    \   the symbol kind `Class` even if they both have the same descriptor:\n\
    \   `SymbolDescriptor.Suffix.Type`.\n\
    \\n\
    \ Since Kind is more fine-grained than Suffix:\n\
    \ - If two symbols have the same Kind, they should share the same Suffix.\n\
    \ - If two symbols have different Suffixes, they should have different Kinds.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\EOT\NUL\SOH\DC2\EOT\135\STX\a\v\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX\NUL\DC2\EOT\136\STX\ACK\SUB\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\NUL\SOH\DC2\EOT\136\STX\ACK\NAK\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\NUL\STX\DC2\EOT\136\STX\CAN\EM\n\
    \R\n\
    \\ACK\EOT\a\EOT\NUL\STX\SOH\DC2\EOT\138\STX\ACK\SUB\SUBB A method which may or may not have a body. For Java, Kotlin etc.\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\SOH\SOH\DC2\EOT\138\STX\ACK\DC4\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\SOH\STX\DC2\EOT\138\STX\ETB\EM\n\
    \*\n\
    \\ACK\EOT\a\EOT\NUL\STX\STX\DC2\EOT\140\STX\ACK\DC4\SUB\SUB For Ruby's attr_accessor\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\STX\SOH\DC2\EOT\140\STX\ACK\SO\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\STX\STX\DC2\EOT\140\STX\DC1\DC3\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX\ETX\DC2\EOT\141\STX\ACK\DLE\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\ETX\SOH\DC2\EOT\141\STX\ACK\v\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\ETX\STX\DC2\EOT\141\STX\SO\SI\n\
    \\ESC\n\
    \\ACK\EOT\a\EOT\NUL\STX\EOT\DC2\EOT\143\STX\ACK\DC4\SUB\v For Alloy\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\EOT\SOH\DC2\EOT\143\STX\ACK\SI\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\EOT\STX\DC2\EOT\143\STX\DC2\DC3\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX\ENQ\DC2\EOT\144\STX\ACK\EM\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\ENQ\SOH\DC2\EOT\144\STX\ACK\DC4\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\ENQ\STX\DC2\EOT\144\STX\ETB\CAN\n\
    \\EM\n\
    \\ACK\EOT\a\EOT\NUL\STX\ACK\DC2\EOT\146\STX\ACK\DC4\SUB\t For C++\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\ACK\SOH\DC2\EOT\146\STX\ACK\SI\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\ACK\STX\DC2\EOT\146\STX\DC2\DC3\n\
    \\SUB\n\
    \\ACK\EOT\a\EOT\NUL\STX\a\DC2\EOT\148\STX\ACK\DLE\SUB\n\
    \ For Lean\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\a\SOH\DC2\EOT\148\STX\ACK\v\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\a\STX\DC2\EOT\148\STX\SO\SI\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX\b\DC2\EOT\149\STX\ACK\DC2\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\b\SOH\DC2\EOT\149\STX\ACK\r\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\b\STX\DC2\EOT\149\STX\DLE\DC1\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX\t\DC2\EOT\150\STX\ACK\DLE\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\t\SOH\DC2\EOT\150\STX\ACK\v\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\t\STX\DC2\EOT\150\STX\SO\SI\n\
    \\EM\n\
    \\ACK\EOT\a\EOT\NUL\STX\n\
    \\DC2\EOT\152\STX\ACK\DC3\SUB\t For C++\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\n\
    \\SOH\DC2\EOT\152\STX\ACK\r\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\n\
    \\STX\DC2\EOT\152\STX\DLE\DC2\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX\v\DC2\EOT\153\STX\ACK\DC3\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\v\SOH\DC2\EOT\153\STX\ACK\SO\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\v\STX\DC2\EOT\153\STX\DC1\DC2\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX\f\DC2\EOT\154\STX\ACK\SYN\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\f\SOH\DC2\EOT\154\STX\ACK\DC1\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\f\STX\DC2\EOT\154\STX\DC4\NAK\n\
    \\RS\n\
    \\ACK\EOT\a\EOT\NUL\STX\r\DC2\EOT\156\STX\ACK\DC4\SUB\SO For Solidity\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\r\SOH\DC2\EOT\156\STX\ACK\SO\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\r\STX\DC2\EOT\156\STX\DC1\DC3\n\
    \\GS\n\
    \\ACK\EOT\a\EOT\NUL\STX\SO\DC2\EOT\158\STX\ACK\SYN\SUB\r For Haskell\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\SO\SOH\DC2\EOT\158\STX\ACK\DLE\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\SO\STX\DC2\EOT\158\STX\DC3\NAK\n\
    \\US\n\
    \\ACK\EOT\a\EOT\NUL\STX\SI\DC2\EOT\160\STX\ACK\DC4\SUB\SI For C# and F#\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\SI\SOH\DC2\EOT\160\STX\ACK\SO\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\SI\STX\DC2\EOT\160\STX\DC1\DC3\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX\DLE\DC2\EOT\161\STX\ACK\DLE\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\DLE\SOH\DC2\EOT\161\STX\ACK\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\DLE\STX\DC2\EOT\161\STX\r\SI\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX\DC1\DC2\EOT\162\STX\ACK\SYN\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\DC1\SOH\DC2\EOT\162\STX\ACK\DLE\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\DC1\STX\DC2\EOT\162\STX\DC3\NAK\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX\DC2\DC2\EOT\163\STX\ACK\DC1\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\DC2\SOH\DC2\EOT\163\STX\ACK\v\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\DC2\STX\DC2\EOT\163\STX\SO\DLE\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX\DC3\DC2\EOT\164\STX\ACK\DC1\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\DC3\SOH\DC2\EOT\164\STX\ACK\v\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\DC3\STX\DC2\EOT\164\STX\SO\DLE\n\
    \\SUB\n\
    \\ACK\EOT\a\EOT\NUL\STX\DC4\DC2\EOT\166\STX\ACK\NAK\SUB\n\
    \ For Dart\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\DC4\SOH\DC2\EOT\166\STX\ACK\SI\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\DC4\STX\DC2\EOT\166\STX\DC2\DC4\n\
    \\ESC\n\
    \\ACK\EOT\a\EOT\NUL\STX\NAK\DC2\EOT\168\STX\ACK\DLE\SUB\v For Alloy\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\NAK\SOH\DC2\EOT\168\STX\ACK\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\NAK\STX\DC2\EOT\168\STX\r\SI\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX\SYN\DC2\EOT\169\STX\ACK\DC1\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\SYN\SOH\DC2\EOT\169\STX\ACK\v\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\SYN\STX\DC2\EOT\169\STX\SO\DLE\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX\ETB\DC2\EOT\170\STX\ACK\DLE\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\ETB\SOH\DC2\EOT\170\STX\ACK\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\ETB\STX\DC2\EOT\170\STX\r\SI\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX\CAN\DC2\EOT\171\STX\ACK\DC4\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\CAN\SOH\DC2\EOT\171\STX\ACK\SO\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\CAN\STX\DC2\EOT\171\STX\DC1\DC3\n\
    \;\n\
    \\ACK\EOT\a\EOT\NUL\STX\EM\DC2\EOT\173\STX\ACK\DC2\SUB+ For 'get' in Swift, 'attr_reader' in Ruby\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\EM\SOH\DC2\EOT\173\STX\ACK\f\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\EM\STX\DC2\EOT\173\STX\SI\DC1\n\
    \\SUB\n\
    \\ACK\EOT\a\EOT\NUL\STX\SUB\DC2\EOT\175\STX\ACK\DC3\SUB\n\
    \ For Raku\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\SUB\SOH\DC2\EOT\175\STX\ACK\r\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\SUB\STX\DC2\EOT\175\STX\DLE\DC2\n\
    \)\n\
    \\ACK\EOT\a\EOT\NUL\STX\ESC\DC2\EOT\177\STX\ACK\DC4\SUB\EM For Purescript and Lean\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\ESC\SOH\DC2\EOT\177\STX\ACK\SO\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\ESC\STX\DC2\EOT\177\STX\DC1\DC3\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX\FS\DC2\EOT\178\STX\ACK\NAK\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\FS\SOH\DC2\EOT\178\STX\ACK\SI\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\FS\STX\DC2\EOT\178\STX\DC2\DC4\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX\GS\DC2\EOT\179\STX\ACK\SI\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\GS\SOH\DC2\EOT\179\STX\ACK\t\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\GS\STX\DC2\EOT\179\STX\f\SO\n\
    \\FS\n\
    \\ACK\EOT\a\EOT\NUL\STX\RS\DC2\EOT\181\STX\ACK\DLE\SUB\f For Racket\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\RS\SOH\DC2\EOT\181\STX\ACK\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\RS\STX\DC2\EOT\181\STX\r\SI\n\
    \\SUB\n\
    \\ACK\EOT\a\EOT\NUL\STX\US\DC2\EOT\183\STX\ACK\DC1\SUB\n\
    \ For Lean\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\US\SOH\DC2\EOT\183\STX\ACK\v\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\US\STX\DC2\EOT\183\STX\SO\DLE\n\
    \\RS\n\
    \\ACK\EOT\a\EOT\NUL\STX \DC2\EOT\185\STX\ACK\DC3\SUB\SO For solidity\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX \SOH\DC2\EOT\185\STX\ACK\r\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX \STX\DC2\EOT\185\STX\DLE\DC2\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX!\DC2\EOT\186\STX\ACK\DC1\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX!\SOH\DC2\EOT\186\STX\ACK\v\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX!\STX\DC2\EOT\186\STX\SO\DLE\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX\"\DC2\EOT\187\STX\ACK\DC2\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\"\SOH\DC2\EOT\187\STX\ACK\f\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX\"\STX\DC2\EOT\187\STX\SI\DC1\n\
    \\SUB\n\
    \\ACK\EOT\a\EOT\NUL\STX#\DC2\EOT\189\STX\ACK\ETB\SUB\n\
    \ For Ruby\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX#\SOH\DC2\EOT\189\STX\ACK\DC1\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX#\STX\DC2\EOT\189\STX\DC4\SYN\n\
    \\148\SOH\n\
    \\ACK\EOT\a\EOT\NUL\STX$\DC2\EOT\192\STX\ACK\SUB\SUB\131\SOH Analogous to 'ThisParameter' and 'SelfParameter', but for languages\n\
    \ like Go where the receiver doesn't have a conventional name.\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX$\SOH\DC2\EOT\192\STX\ACK\DC4\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX$\STX\DC2\EOT\192\STX\ETB\EM\n\
    \8\n\
    \\ACK\EOT\a\EOT\NUL\STX%\DC2\EOT\194\STX\ACK\US\SUB( Analogous to 'AbstractMethod', for Go.\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX%\SOH\DC2\EOT\194\STX\ACK\EM\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX%\STX\DC2\EOT\194\STX\FS\RS\n\
    \\RS\n\
    \\ACK\EOT\a\EOT\NUL\STX&\DC2\EOT\196\STX\ACK\DC3\SUB\SO For Protobuf\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX&\SOH\DC2\EOT\196\STX\ACK\r\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX&\STX\DC2\EOT\196\STX\DLE\DC2\n\
    \\SUB\n\
    \\ACK\EOT\a\EOT\NUL\STX'\DC2\EOT\198\STX\ACK\DC1\SUB\n\
    \ For Dart\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX'\SOH\DC2\EOT\198\STX\ACK\v\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX'\STX\DC2\EOT\198\STX\SO\DLE\n\
    \\RS\n\
    \\ACK\EOT\a\EOT\NUL\STX(\DC2\EOT\200\STX\ACK\DC4\SUB\SO For Solidity\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX(\SOH\DC2\EOT\200\STX\ACK\SO\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX(\STX\DC2\EOT\200\STX\DC1\DC3\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX)\DC2\EOT\201\STX\ACK\DC2\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX)\SOH\DC2\EOT\201\STX\ACK\f\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX)\STX\DC2\EOT\201\STX\SI\DC1\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX*\DC2\EOT\202\STX\ACK\NAK\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX*\SOH\DC2\EOT\202\STX\ACK\SI\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX*\STX\DC2\EOT\202\STX\DC2\DC4\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX+\DC2\EOT\203\STX\ACK\DLE\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX+\SOH\DC2\EOT\203\STX\ACK\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX+\STX\DC2\EOT\203\STX\r\SI\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX,\DC2\EOT\204\STX\ACK\DC2\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX,\SOH\DC2\EOT\204\STX\ACK\f\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX,\STX\DC2\EOT\204\STX\SI\DC1\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX-\DC2\EOT\205\STX\ACK\DC2\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX-\SOH\DC2\EOT\205\STX\ACK\f\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX-\STX\DC2\EOT\205\STX\SI\DC1\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX.\DC2\EOT\206\STX\ACK\DC4\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX.\SOH\DC2\EOT\206\STX\ACK\SO\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX.\STX\DC2\EOT\206\STX\DC1\DC3\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX/\DC2\EOT\207\STX\ACK\DC3\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX/\SOH\DC2\EOT\207\STX\ACK\r\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX/\STX\DC2\EOT\207\STX\DLE\DC2\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX0\DC2\EOT\208\STX\ACK\EM\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX0\SOH\DC2\EOT\208\STX\ACK\DC3\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX0\STX\DC2\EOT\208\STX\SYN\CAN\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX1\DC2\EOT\209\STX\ACK\NAK\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX1\SOH\DC2\EOT\209\STX\ACK\SI\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX1\STX\DC2\EOT\209\STX\DC2\DC4\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX2\DC2\EOT\210\STX\ACK\SUB\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX2\SOH\DC2\EOT\210\STX\ACK\DC4\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX2\STX\DC2\EOT\210\STX\ETB\EM\n\
    \/\n\
    \\ACK\EOT\a\EOT\NUL\STX3\DC2\EOT\212\STX\ACK\DC3\SUB\US For Haskell's PatternSynonyms\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX3\SOH\DC2\EOT\212\STX\ACK\r\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX3\STX\DC2\EOT\212\STX\DLE\DC2\n\
    \\ESC\n\
    \\ACK\EOT\a\EOT\NUL\STX4\DC2\EOT\214\STX\ACK\NAK\SUB\v For Alloy\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX4\SOH\DC2\EOT\214\STX\ACK\SI\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX4\STX\DC2\EOT\214\STX\DC2\DC4\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STX5\DC2\EOT\215\STX\ACK\DC4\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX5\SOH\DC2\EOT\215\STX\ACK\SO\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX5\STX\DC2\EOT\215\STX\DC1\DC3\n\
    \Q\n\
    \\ACK\EOT\a\EOT\NUL\STX6\DC2\EOT\217\STX\ACK\DC4\SUBA Analogous to 'Trait' and 'TypeClass', for Swift and Objective-C\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX6\SOH\DC2\EOT\217\STX\ACK\SO\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX6\STX\DC2\EOT\217\STX\DC1\DC3\n\
    \K\n\
    \\ACK\EOT\a\EOT\NUL\STX7\DC2\EOT\219\STX\ACK\SUB\SUB; Analogous to 'AbstractMethod', for Swift and Objective-C.\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX7\SOH\DC2\EOT\219\STX\ACK\DC4\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX7\STX\DC2\EOT\219\STX\ETB\EM\n\
    \9\n\
    \\ACK\EOT\a\EOT\NUL\STX8\DC2\EOT\221\STX\ACK\GS\SUB) Analogous to 'AbstractMethod', for C++.\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX8\SOH\DC2\EOT\221\STX\ACK\ETB\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX8\STX\DC2\EOT\221\STX\SUB\FS\n\
    \\GS\n\
    \\ACK\EOT\a\EOT\NUL\STX9\DC2\EOT\223\STX\ACK\ETB\SUB\r For Haskell\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX9\SOH\DC2\EOT\223\STX\ACK\DC1\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX9\STX\DC2\EOT\223\STX\DC4\SYN\n\
    \4\n\
    \\ACK\EOT\a\EOT\NUL\STX:\DC2\EOT\225\STX\ACK\EM\SUB$ 'self' in Python, Rust, Swift etc.\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX:\SOH\DC2\EOT\225\STX\ACK\DC3\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX:\STX\DC2\EOT\225\STX\SYN\CAN\n\
    \;\n\
    \\ACK\EOT\a\EOT\NUL\STX;\DC2\EOT\227\STX\ACK\DC2\SUB+ For 'set' in Swift, 'attr_writer' in Ruby\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX;\SOH\DC2\EOT\227\STX\ACK\f\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX;\STX\DC2\EOT\227\STX\SI\DC1\n\
    \3\n\
    \\ACK\EOT\a\EOT\NUL\STX<\DC2\EOT\229\STX\ACK\NAK\SUB# For Alloy, analogous to 'Struct'.\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX<\SOH\DC2\EOT\229\STX\ACK\SI\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX<\STX\DC2\EOT\229\STX\DC2\DC4\n\
    \\SUB\n\
    \\ACK\EOT\a\EOT\NUL\STX=\DC2\EOT\231\STX\ACK\SUB\SUB\n\
    \ For Ruby\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX=\SOH\DC2\EOT\231\STX\ACK\DC4\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX=\STX\DC2\EOT\231\STX\ETB\EM\n\
    \8\n\
    \\ACK\EOT\a\EOT\NUL\STX>\DC2\EOT\233\STX\ACK\ESC\SUB( Analogous to 'StaticMethod', for Ruby.\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX>\SOH\DC2\EOT\233\STX\ACK\NAK\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX>\STX\DC2\EOT\233\STX\CAN\SUB\n\
    \5\n\
    \\ACK\EOT\a\EOT\NUL\STX?\DC2\EOT\235\STX\ACK\FS\SUB% Analogous to 'StaticField', for C++\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX?\SOH\DC2\EOT\235\STX\ACK\SYN\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX?\STX\DC2\EOT\235\STX\EM\ESC\n\
    \\CAN\n\
    \\ACK\EOT\a\EOT\NUL\STX@\DC2\EOT\237\STX\ACK\ETB\SUB\b For C#\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX@\SOH\DC2\EOT\237\STX\ACK\DC1\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STX@\STX\DC2\EOT\237\STX\DC4\SYN\n\
    \\CAN\n\
    \\ACK\EOT\a\EOT\NUL\STXA\DC2\EOT\239\STX\ACK\ETB\SUB\b For C#\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXA\SOH\DC2\EOT\239\STX\ACK\DC1\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXA\STX\DC2\EOT\239\STX\DC4\SYN\n\
    \(\n\
    \\ACK\EOT\a\EOT\NUL\STXB\DC2\EOT\241\STX\ACK\CAN\SUB\CAN For Java, C#, C++ etc.\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXB\SOH\DC2\EOT\241\STX\ACK\DC2\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXB\STX\DC2\EOT\241\STX\NAK\ETB\n\
    \)\n\
    \\ACK\EOT\a\EOT\NUL\STXC\DC2\EOT\243\STX\ACK\SUB\SUB\EM For C#, TypeScript etc.\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXC\SOH\DC2\EOT\243\STX\ACK\DC4\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXC\STX\DC2\EOT\243\STX\ETB\EM\n\
    \\FS\n\
    \\ACK\EOT\a\EOT\NUL\STXD\DC2\EOT\245\STX\ACK\SUB\SUB\f For C, C++\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXD\SOH\DC2\EOT\245\STX\ACK\DC4\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXD\STX\DC2\EOT\245\STX\ETB\EM\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STXE\DC2\EOT\246\STX\ACK\DC2\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXE\SOH\DC2\EOT\246\STX\ACK\f\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXE\STX\DC2\EOT\246\STX\SI\DC1\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STXF\DC2\EOT\247\STX\ACK\DC2\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXF\SOH\DC2\EOT\247\STX\ACK\f\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXF\STX\DC2\EOT\247\STX\SI\DC1\n\
    \\ESC\n\
    \\ACK\EOT\a\EOT\NUL\STXG\DC2\EOT\249\STX\ACK\NAK\SUB\v For Swift\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXG\SOH\DC2\EOT\249\STX\ACK\SI\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXG\STX\DC2\EOT\249\STX\DC2\DC4\n\
    \\SUB\n\
    \\ACK\EOT\a\EOT\NUL\STXH\DC2\EOT\251\STX\ACK\DC2\SUB\n\
    \ For Lean\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXH\SOH\DC2\EOT\251\STX\ACK\f\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXH\STX\DC2\EOT\251\STX\SI\DC1\n\
    \\SUB\n\
    \\ACK\EOT\a\EOT\NUL\STXI\DC2\EOT\253\STX\ACK\DC3\SUB\n\
    \ For Lean\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXI\SOH\DC2\EOT\253\STX\ACK\r\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXI\STX\DC2\EOT\253\STX\DLE\DC2\n\
    \U\n\
    \\ACK\EOT\a\EOT\NUL\STXJ\DC2\EOT\128\ETX\ACK\EM\SUBE Method receiver for languages\n\
    \ 'this' in JavaScript, C++, Java etc.\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXJ\SOH\DC2\EOT\128\ETX\ACK\DC3\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXJ\STX\DC2\EOT\128\ETX\SYN\CAN\n\
    \O\n\
    \\ACK\EOT\a\EOT\NUL\STXK\DC2\EOT\130\ETX\ACK\DC1\SUB? Analogous to 'Protocol' and 'TypeClass', for Rust, Scala etc.\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXK\SOH\DC2\EOT\130\ETX\ACK\v\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXK\STX\DC2\EOT\130\ETX\SO\DLE\n\
    \E\n\
    \\ACK\EOT\a\EOT\NUL\STXL\DC2\EOT\132\ETX\ACK\ETB\SUB5 Analogous to 'AbstractMethod', for Rust, Scala etc.\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXL\SOH\DC2\EOT\132\ETX\ACK\DC1\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXL\STX\DC2\EOT\132\ETX\DC4\SYN\n\
    \\137\SOH\n\
    \\ACK\EOT\a\EOT\NUL\STXM\DC2\EOT\135\ETX\ACK\DLE\SUBy Data type definition for languages like OCaml which use `type`\n\
    \ rather than separate keywords like `struct` and `enum`.\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXM\SOH\DC2\EOT\135\ETX\ACK\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXM\STX\DC2\EOT\135\ETX\r\SI\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STXN\DC2\EOT\136\ETX\ACK\NAK\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXN\SOH\DC2\EOT\136\ETX\ACK\SI\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXN\STX\DC2\EOT\136\ETX\DC2\DC4\n\
    \S\n\
    \\ACK\EOT\a\EOT\NUL\STXO\DC2\EOT\138\ETX\ACK\NAK\SUBC Analogous to 'Trait' and 'Protocol', for Haskell, Purescript etc.\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXO\SOH\DC2\EOT\138\ETX\ACK\SI\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXO\STX\DC2\EOT\138\ETX\DC2\DC4\n\
    \M\n\
    \\ACK\EOT\a\EOT\NUL\STXP\DC2\EOT\140\ETX\ACK\ESC\SUB= Analogous to 'AbstractMethod', for Haskell, Purescript etc.\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXP\SOH\DC2\EOT\140\ETX\ACK\NAK\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXP\STX\DC2\EOT\140\ETX\CAN\SUB\n\
    \\GS\n\
    \\ACK\EOT\a\EOT\NUL\STXQ\DC2\EOT\142\ETX\ACK\SYN\SUB\r For Haskell\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXQ\SOH\DC2\EOT\142\ETX\ACK\DLE\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXQ\STX\DC2\EOT\142\ETX\DC3\NAK\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STXR\DC2\EOT\143\ETX\ACK\EM\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXR\SOH\DC2\EOT\143\ETX\ACK\DC3\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXR\STX\DC2\EOT\143\ETX\SYN\CAN\n\
    \(\n\
    \\ACK\EOT\a\EOT\NUL\STXS\DC2\EOT\145\ETX\ACK\DC1\SUB\CAN For C, C++, Capn Proto\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXS\SOH\DC2\EOT\145\ETX\ACK\v\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXS\STX\DC2\EOT\145\ETX\SO\DLE\n\
    \\SO\n\
    \\ACK\EOT\a\EOT\NUL\STXT\DC2\EOT\146\ETX\ACK\DC1\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXT\SOH\DC2\EOT\146\ETX\ACK\v\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXT\STX\DC2\EOT\146\ETX\SO\DLE\n\
    \[\n\
    \\ACK\EOT\a\EOT\NUL\STXU\DC2\EOT\147\ETX\ACK\DC4\"K Next = 87;\n\
    \ Feel free to open a PR proposing new language-specific kinds.\n\
    \\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXU\SOH\DC2\EOT\147\ETX\ACK\SO\n\
    \\SI\n\
    \\a\EOT\a\EOT\NUL\STXU\STX\DC2\EOT\147\ETX\DC1\DC3\n\
    \\243\ETX\n\
    \\EOT\EOT\a\STX\EOT\DC2\EOT\160\ETX\STX\SUB\SUB\228\ETX (optional) The name of this symbol as it should be displayed to the user.\n\
    \ For example, the symbol \"com/example/MyClass#myMethod(+1).\" should have the\n\
    \ display name \"myMethod\". The `symbol` field is not a reliable source of\n\
    \ the display name for several reasons:\n\
    \\n\
    \ - Local symbols don't encode the name.\n\
    \ - Some languages have case-insensitive names, so the symbol is all-lowercase.\n\
    \ - The symbol may encode names with special characters that should not be\n\
    \   displayed to the user.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\EOT\ENQ\DC2\EOT\160\ETX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\a\STX\EOT\SOH\DC2\EOT\160\ETX\t\NAK\n\
    \\r\n\
    \\ENQ\EOT\a\STX\EOT\ETX\DC2\EOT\160\ETX\CAN\EM\n\
    \\196\ETX\n\
    \\EOT\EOT\a\STX\ENQ\DC2\EOT\167\ETX\STX'\SUB\181\ETX (optional) The signature of this symbol as it's displayed in API\n\
    \ documentation or in hover tooltips. For example, a Java method that adds\n\
    \ two numbers this would have `Document.language = \"java\"` and `Document.text\n\
    \ = \"void add(int a, int b)\". The `language` and `text` fields are required\n\
    \ while other fields such as `Documentation.occurrences` can be optionally\n\
    \ included to support hyperlinking referenced symbols in the signature.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ENQ\ACK\DC2\EOT\167\ETX\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ENQ\SOH\DC2\EOT\167\ETX\v\"\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ENQ\ETX\DC2\EOT\167\ETX%&\n\
    \\201\b\n\
    \\EOT\EOT\a\STX\ACK\DC2\EOT\185\ETX\STX\RS\SUB\186\b (optional) The enclosing symbol if this is a local symbol.  For non-local\n\
    \ symbols, the enclosing symbol should be parsed from the `symbol` field\n\
    \ using the `Descriptor` grammar.\n\
    \\n\
    \ The primary use-case for this field is to allow local symbol to be displayed\n\
    \ in a symbol hierarchy for API documentation. It's OK to leave this field\n\
    \ empty for local variables since local variables usually don't belong in API\n\
    \ documentation. However, in the situation that you wish to include a local\n\
    \ symbol in the hierarchy, then you can use `enclosing_symbol` to locate the\n\
    \ \"parent\" or \"owner\" of this local symbol. For example, a Java indexer may\n\
    \ choose to use local symbols for private class fields while providing an\n\
    \ `enclosing_symbol` to reference the enclosing class to allow the field to\n\
    \ be part of the class documentation hierarchy. From the perspective of an\n\
    \ author of an indexer, the decision to use a local symbol or global symbol\n\
    \ should exclusively be determined whether the local symbol is accessible\n\
    \ outside the document, not by the capability to find the enclosing\n\
    \ symbol.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ACK\ENQ\DC2\EOT\185\ETX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ACK\SOH\DC2\EOT\185\ETX\t\EM\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ACK\ETX\DC2\EOT\185\ETX\FS\GS\n\
    \\f\n\
    \\STX\EOT\b\DC2\ACK\189\ETX\NUL\245\ETX\SOH\n\
    \\v\n\
    \\ETX\EOT\b\SOH\DC2\EOT\189\ETX\b\DC4\n\
    \\f\n\
    \\EOT\EOT\b\STX\NUL\DC2\EOT\190\ETX\STX\DC4\n\
    \\r\n\
    \\ENQ\EOT\b\STX\NUL\ENQ\DC2\EOT\190\ETX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\b\STX\NUL\SOH\DC2\EOT\190\ETX\t\SI\n\
    \\r\n\
    \\ENQ\EOT\b\STX\NUL\ETX\DC2\EOT\190\ETX\DC2\DC3\n\
    \\222\b\n\
    \\EOT\EOT\b\STX\SOH\DC2\EOT\215\ETX\STX\CAN\SUB\207\b When resolving \"Find references\", this field documents what other symbols\n\
    \ should be included together with this symbol. For example, consider the\n\
    \ following TypeScript code that defines two symbols `Animal#sound()` and\n\
    \ `Dog#sound()`:\n\
    \ ```ts\n\
    \ interface Animal {\n\
    \           ^^^^^^ definition Animal#\n\
    \   sound(): string\n\
    \   ^^^^^ definition Animal#sound()\n\
    \ }\n\
    \ class Dog implements Animal {\n\
    \       ^^^ definition Dog#, relationships = [{symbol: \"Animal#\", is_implementation: true}]\n\
    \   public sound(): string { return \"woof\" }\n\
    \          ^^^^^ definition Dog#sound(), references_symbols = Animal#sound(), relationships = [{symbol: \"Animal#sound()\", is_implementation:true, is_reference: true}]\n\
    \ }\n\
    \ const animal: Animal = new Dog()\n\
    \               ^^^^^^ reference Animal#\n\
    \ console.log(animal.sound())\n\
    \                    ^^^^^ reference Animal#sound()\n\
    \ ```\n\
    \ Doing \"Find references\" on the symbol `Animal#sound()` should return\n\
    \ references to the `Dog#sound()` method as well. Vice-versa, doing \"Find\n\
    \ references\" on the `Dog#sound()` method should include references to the\n\
    \ `Animal#sound()` method as well.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\b\STX\SOH\ENQ\DC2\EOT\215\ETX\STX\ACK\n\
    \\r\n\
    \\ENQ\EOT\b\STX\SOH\SOH\DC2\EOT\215\ETX\a\DC3\n\
    \\r\n\
    \\ENQ\EOT\b\STX\SOH\ETX\DC2\EOT\215\ETX\SYN\ETB\n\
    \\238\ETX\n\
    \\EOT\EOT\b\STX\STX\DC2\EOT\224\ETX\STX\GS\SUB\223\ETX Similar to `is_reference` but for \"Find implementations\".\n\
    \ It's common for `is_implementation` and `is_reference` to both be true but\n\
    \ it's not always the case.\n\
    \ In the TypeScript example above, observe that `Dog#` has an\n\
    \ `is_implementation` relationship with `\"Animal#\"` but not `is_reference`.\n\
    \ This is because \"Find references\" on the \"Animal#\" symbol should not return\n\
    \ \"Dog#\". We only want \"Dog#\" to return as a result for \"Find\n\
    \ implementations\" on the \"Animal#\" symbol.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\b\STX\STX\ENQ\DC2\EOT\224\ETX\STX\ACK\n\
    \\r\n\
    \\ENQ\EOT\b\STX\STX\SOH\DC2\EOT\224\ETX\a\CAN\n\
    \\r\n\
    \\ENQ\EOT\b\STX\STX\ETX\DC2\EOT\224\ETX\ESC\FS\n\
    \P\n\
    \\EOT\EOT\b\STX\ETX\DC2\EOT\226\ETX\STX\RS\SUBB Similar to `references_symbols` but for \"Go to type definition\".\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\b\STX\ETX\ENQ\DC2\EOT\226\ETX\STX\ACK\n\
    \\r\n\
    \\ENQ\EOT\b\STX\ETX\SOH\DC2\EOT\226\ETX\a\EM\n\
    \\r\n\
    \\ENQ\EOT\b\STX\ETX\ETX\DC2\EOT\226\ETX\FS\GS\n\
    \\167\a\n\
    \\EOT\EOT\b\STX\EOT\DC2\EOT\243\ETX\STX\EM\SUB\213\ACK Allows overriding the behavior of \"Go to definition\" and \"Find references\"\n\
    \ for symbols which do not have a definition of their own or could\n\
    \ potentially have multiple definitions.\n\
    \\n\
    \ For example, in a language with single inheritance and no field overriding,\n\
    \ inherited fields can reuse the same symbol as the ancestor which declares\n\
    \ the field. In such a situation, is_definition is not needed.\n\
    \\n\
    \ On the other hand, in languages with single inheritance and some form\n\
    \ of mixins, you can use is_definition to relate the symbol to the\n\
    \ matching symbol in ancestor classes, and is_reference to relate the\n\
    \ symbol to the matching symbol in mixins.\n\
    \\n\
    \ NOTE: At the moment, due to limitations of the SCIP to LSIF conversion,\n\
    \ only global symbols in an index are allowed to use is_definition.\n\
    \ The relationship may not get recorded if either symbol is local.\n\
    \\"A Update registerInverseRelationships on adding a new field here.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\b\STX\EOT\ENQ\DC2\EOT\243\ETX\STX\ACK\n\
    \\r\n\
    \\ENQ\EOT\b\STX\EOT\SOH\DC2\EOT\243\ETX\a\DC4\n\
    \\r\n\
    \\ENQ\EOT\b\STX\EOT\ETX\DC2\EOT\243\ETX\ETB\CAN\n\
    \\136\ETX\n\
    \\STX\ENQ\ETX\DC2\ACK\252\ETX\NUL\146\EOT\SOH\SUB\249\STX SymbolRole declares what \"role\" a symbol has in an occurrence. A role is\n\
    \ encoded as a bitset where each bit represents a different role. For example,\n\
    \ to determine if the `Import` role is set, test whether the second bit of the\n\
    \ enum value is defined. In pseudocode, this can be implemented with the\n\
    \ logic: `const isImportRole = (role.value & SymbolRole.Import.value) > 0`.\n\
    \\n\
    \\v\n\
    \\ETX\ENQ\ETX\SOH\DC2\EOT\252\ETX\ENQ\SI\n\
    \v\n\
    \\EOT\ENQ\ETX\STX\NUL\DC2\EOT\255\ETX\STX\FS\SUBh This case is not meant to be used; it only exists to avoid an error\n\
    \ from the Protobuf code generator.\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\NUL\SOH\DC2\EOT\255\ETX\STX\ETB\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\NUL\STX\DC2\EOT\255\ETX\SUB\ESC\n\
    \T\n\
    \\EOT\ENQ\ETX\STX\SOH\DC2\EOT\129\EOT\STX\DC3\SUBF Is the symbol defined here? If not, then this is a symbol reference.\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\SOH\SOH\DC2\EOT\129\EOT\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\SOH\STX\DC2\EOT\129\EOT\SI\DC2\n\
    \,\n\
    \\EOT\ENQ\ETX\STX\STX\DC2\EOT\131\EOT\STX\SI\SUB\RS Is the symbol imported here?\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\STX\SOH\DC2\EOT\131\EOT\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\STX\STX\DC2\EOT\131\EOT\v\SO\n\
    \+\n\
    \\EOT\ENQ\ETX\STX\ETX\DC2\EOT\133\EOT\STX\DC4\SUB\GS Is the symbol written here?\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\ETX\SOH\DC2\EOT\133\EOT\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\ETX\STX\DC2\EOT\133\EOT\DLE\DC3\n\
    \(\n\
    \\EOT\ENQ\ETX\STX\EOT\DC2\EOT\135\EOT\STX\DC3\SUB\SUB Is the symbol read here?\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\EOT\SOH\DC2\EOT\135\EOT\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\EOT\STX\DC2\EOT\135\EOT\SI\DC2\n\
    \0\n\
    \\EOT\ENQ\ETX\STX\ENQ\DC2\EOT\137\EOT\STX\DC3\SUB\" Is the symbol in generated code?\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\ENQ\SOH\DC2\EOT\137\EOT\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\ENQ\STX\DC2\EOT\137\EOT\SO\DC2\n\
    \+\n\
    \\EOT\ENQ\ETX\STX\ACK\DC2\EOT\139\EOT\STX\SO\SUB\GS Is the symbol in test code?\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\ACK\SOH\DC2\EOT\139\EOT\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\ACK\STX\DC2\EOT\139\EOT\t\r\n\
    \\237\SOH\n\
    \\EOT\ENQ\ETX\STX\a\DC2\EOT\145\EOT\STX\ESC\SUB\222\SOH Is this a signature for a symbol that is defined elsewhere?\n\
    \\n\
    \ Applies to forward declarations for languages like C, C++\n\
    \ and Objective-C, as well as `val` declarations in interface\n\
    \ files in languages like SML and OCaml.\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\a\SOH\DC2\EOT\145\EOT\STX\DC3\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\a\STX\DC2\EOT\145\EOT\SYN\SUB\n\
    \\f\n\
    \\STX\ENQ\EOT\DC2\ACK\148\EOT\NUL\241\EOT\SOH\n\
    \\v\n\
    \\ETX\ENQ\EOT\SOH\DC2\EOT\148\EOT\ENQ\SI\n\
    \\v\n\
    \\ETX\ENQ\EOT\ETX\DC2\EOT\149\EOT\STX\FS\n\
    \\f\n\
    \\EOT\ENQ\EOT\ETX\STX\DC2\EOT\149\EOT\STX\FS\n\
    \\f\n\
    \\EOT\ENQ\EOT\STX\NUL\DC2\EOT\151\EOT\STX\FS\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\NUL\SOH\DC2\EOT\151\EOT\STX\ETB\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\NUL\STX\DC2\EOT\151\EOT\SUB\ESC\n\
    \;\n\
    \\EOT\ENQ\EOT\STX\SOH\DC2\EOT\154\EOT\STX\SO\SUB- Comment, including comment markers and text\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\SOH\SOH\DC2\EOT\154\EOT\STX\t\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\SOH\STX\DC2\EOT\154\EOT\f\r\n\
    \\ESC\n\
    \\EOT\ENQ\EOT\STX\STX\DC2\EOT\157\EOT\STX\ESC\SUB\r `;` `.` `,`\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\STX\SOH\DC2\EOT\157\EOT\STX\SYN\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\STX\STX\DC2\EOT\157\EOT\EM\SUB\n\
    \2\n\
    \\EOT\ENQ\EOT\STX\ETX\DC2\EOT\159\EOT\STX\EM\SUB$ (), {}, [] when used syntactically\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\ETX\SOH\DC2\EOT\159\EOT\STX\DC4\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\ETX\STX\DC2\EOT\159\EOT\ETB\CAN\n\
    \5\n\
    \\EOT\ENQ\EOT\STX\EOT\DC2\EOT\162\EOT\STX\SO\SUB' `if`, `else`, `return`, `class`, etc.\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\EOT\SOH\DC2\EOT\162\EOT\STX\t\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\EOT\STX\DC2\EOT\162\EOT\f\r\n\
    \\f\n\
    \\EOT\ENQ\EOT\STX\ENQ\DC2\EOT\163\EOT\STX*\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\ENQ\SOH\DC2\EOT\163\EOT\STX\DC3\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\ENQ\STX\DC2\EOT\163\EOT\SYN\ETB\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\ENQ\ETX\DC2\EOT\163\EOT\CAN)\n\
    \\SO\n\
    \\ACK\ENQ\EOT\STX\ENQ\ETX\SOH\DC2\EOT\163\EOT\EM(\n\
    \\RS\n\
    \\EOT\ENQ\EOT\STX\ACK\DC2\EOT\166\EOT\STX\EM\SUB\DLE `+`, `*`, etc.\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\ACK\SOH\DC2\EOT\166\EOT\STX\DC4\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\ACK\STX\DC2\EOT\166\EOT\ETB\CAN\n\
    \X\n\
    \\EOT\ENQ\EOT\STX\a\DC2\EOT\169\EOT\STX\DC1\SUBJ non-specific catch-all for any identifier not better described elsewhere\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\a\SOH\DC2\EOT\169\EOT\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\a\STX\DC2\EOT\169\EOT\SI\DLE\n\
    \N\n\
    \\EOT\ENQ\EOT\STX\b\DC2\EOT\171\EOT\STX\CAN\SUB@ Identifiers builtin to the language: `min`, `print` in Python.\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\b\SOH\DC2\EOT\171\EOT\STX\DC3\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\b\STX\DC2\EOT\171\EOT\SYN\ETB\n\
    \[\n\
    \\EOT\ENQ\EOT\STX\t\DC2\EOT\173\EOT\STX\NAK\SUBM Identifiers representing `null`-like values: `None` in Python, `nil` in Go.\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\t\SOH\DC2\EOT\173\EOT\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\t\STX\DC2\EOT\173\EOT\DC3\DC4\n\
    \.\n\
    \\EOT\ENQ\EOT\STX\n\
    \\DC2\EOT\175\EOT\STX\EM\SUB  `xyz` in `const xyz = \"hello\"`\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\n\
    \\SOH\DC2\EOT\175\EOT\STX\DC4\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\n\
    \\STX\DC2\EOT\175\EOT\ETB\CAN\n\
    \'\n\
    \\EOT\ENQ\EOT\STX\v\DC2\EOT\177\EOT\STX\US\SUB\EM `var X = \"hello\"` in Go\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\v\SOH\DC2\EOT\177\EOT\STX\EM\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\v\STX\DC2\EOT\177\EOT\FS\RS\n\
    \3\n\
    \\EOT\ENQ\EOT\STX\f\DC2\EOT\179\EOT\STX\ESC\SUB% Parameter definition and references\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\f\SOH\DC2\EOT\179\EOT\STX\NAK\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\f\STX\DC2\EOT\179\EOT\CAN\SUB\n\
    \X\n\
    \\EOT\ENQ\EOT\STX\r\DC2\EOT\181\EOT\STX\ETB\SUBJ Identifiers for variable definitions and references within a local scope\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\r\SOH\DC2\EOT\181\EOT\STX\DC1\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\r\STX\DC2\EOT\181\EOT\DC4\SYN\n\
    \K\n\
    \\EOT\ENQ\EOT\STX\SO\DC2\EOT\183\EOT\STX\SUB\SUB= Identifiers that shadow other identifiers in an outer scope\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\SO\SOH\DC2\EOT\183\EOT\STX\DC4\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\SO\STX\DC2\EOT\183\EOT\ETB\EM\n\
    \\205\SOH\n\
    \\EOT\ENQ\EOT\STX\SI\DC2\EOT\188\EOT\STX\ESC\SUB\190\SOH Identifier representing a unit of code abstraction and/or namespacing.\n\
    \\n\
    \ NOTE: This corresponds to a package in Go and JVM languages,\n\
    \ and a module in languages like Python and JavaScript.\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\SI\SOH\DC2\EOT\188\EOT\STX\NAK\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\SI\STX\DC2\EOT\188\EOT\CAN\SUB\n\
    \\f\n\
    \\EOT\ENQ\EOT\STX\DLE\DC2\EOT\189\EOT\STX*\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\DLE\SOH\DC2\EOT\189\EOT\STX\DC2\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\DLE\STX\DC2\EOT\189\EOT\NAK\ETB\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\DLE\ETX\DC2\EOT\189\EOT\CAN)\n\
    \\SO\n\
    \\ACK\ENQ\EOT\STX\DLE\ETX\SOH\DC2\EOT\189\EOT\EM(\n\
    \4\n\
    \\EOT\ENQ\EOT\STX\DC1\DC2\EOT\192\EOT\STX\SUB\SUB& Function references, including calls\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\DC1\SOH\DC2\EOT\192\EOT\STX\DC4\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\DC1\STX\DC2\EOT\192\EOT\ETB\EM\n\
    \(\n\
    \\EOT\ENQ\EOT\STX\DC2\DC2\EOT\194\EOT\STX$\SUB\SUB Function definition only\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\DC2\SOH\DC2\EOT\194\EOT\STX\RS\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\DC2\STX\DC2\EOT\194\EOT!#\n\
    \7\n\
    \\EOT\ENQ\EOT\STX\DC3\DC2\EOT\197\EOT\STX\ETB\SUB) Macro references, including invocations\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\DC3\SOH\DC2\EOT\197\EOT\STX\DC1\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\DC3\STX\DC2\EOT\197\EOT\DC4\SYN\n\
    \%\n\
    \\EOT\ENQ\EOT\STX\DC4\DC2\EOT\199\EOT\STX!\SUB\ETB Macro definition only\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\DC4\SOH\DC2\EOT\199\EOT\STX\ESC\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\DC4\STX\DC2\EOT\199\EOT\RS \n\
    \!\n\
    \\EOT\ENQ\EOT\STX\NAK\DC2\EOT\202\EOT\STX\SYN\SUB\DC3 non-builtin types\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\NAK\SOH\DC2\EOT\202\EOT\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\NAK\STX\DC2\EOT\202\EOT\DC3\NAK\n\
    \K\n\
    \\EOT\ENQ\EOT\STX\SYN\DC2\EOT\204\EOT\STX\GS\SUB= builtin types only, such as `str` for Python or `int` in Go\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\SYN\SOH\DC2\EOT\204\EOT\STX\ETB\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\SYN\STX\DC2\EOT\204\EOT\SUB\FS\n\
    \7\n\
    \\EOT\ENQ\EOT\STX\ETB\DC2\EOT\207\EOT\STX\ESC\SUB) Python decorators, c-like __attribute__\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\ETB\SOH\DC2\EOT\207\EOT\STX\NAK\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\ETB\STX\DC2\EOT\207\EOT\CAN\SUB\n\
    \\DC4\n\
    \\EOT\ENQ\EOT\STX\CAN\DC2\EOT\210\EOT\STX\DC3\SUB\ACK `\\b`\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\CAN\SOH\DC2\EOT\210\EOT\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\CAN\STX\DC2\EOT\210\EOT\DLE\DC2\n\
    \\CAN\n\
    \\EOT\ENQ\EOT\STX\EM\DC2\EOT\212\EOT\STX\NAK\SUB\n\
    \ `*`, `+`\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\EM\SOH\DC2\EOT\212\EOT\STX\SI\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\EM\STX\DC2\EOT\212\EOT\DC2\DC4\n\
    \\DC3\n\
    \\EOT\ENQ\EOT\STX\SUB\DC2\EOT\214\EOT\STX\NAK\SUB\ENQ `.`\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\SUB\SOH\DC2\EOT\214\EOT\STX\SI\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\SUB\STX\DC2\EOT\214\EOT\DC2\DC4\n\
    \\"\n\
    \\EOT\ENQ\EOT\STX\ESC\DC2\EOT\216\EOT\STX\SYN\SUB\DC4 `(`, `)`, `[`, `]`\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\ESC\SOH\DC2\EOT\216\EOT\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\ESC\STX\DC2\EOT\216\EOT\DC3\NAK\n\
    \\CAN\n\
    \\EOT\ENQ\EOT\STX\FS\DC2\EOT\218\EOT\STX\DC1\SUB\n\
    \ `|`, `-`\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\FS\SOH\DC2\EOT\218\EOT\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\FS\STX\DC2\EOT\218\EOT\SO\DLE\n\
    \0\n\
    \\EOT\ENQ\EOT\STX\GS\DC2\EOT\221\EOT\STX\NAK\SUB\" Literal strings: \"Hello, world!\"\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\GS\SOH\DC2\EOT\221\EOT\STX\SI\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\GS\STX\DC2\EOT\221\EOT\DC2\DC4\n\
    \-\n\
    \\EOT\ENQ\EOT\STX\RS\DC2\EOT\223\EOT\STX\ESC\SUB\US non-regex escapes: \"\\t\", \"\\n\"\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\RS\SOH\DC2\EOT\223\EOT\STX\NAK\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\RS\STX\DC2\EOT\223\EOT\CAN\SUB\n\
    \_\n\
    \\EOT\ENQ\EOT\STX\US\DC2\EOT\225\EOT\STX\FS\SUBQ datetimes within strings, special words within a string, `{}` in format strings\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\US\SOH\DC2\EOT\225\EOT\STX\SYN\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\US\STX\DC2\EOT\225\EOT\EM\ESC\n\
    \G\n\
    \\EOT\ENQ\EOT\STX \DC2\EOT\227\EOT\STX\CAN\SUB9 \"key\" in { \"key\": \"value\" }, useful for example in JSON\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX \SOH\DC2\EOT\227\EOT\STX\DC2\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX \STX\DC2\EOT\227\EOT\NAK\ETB\n\
    \V\n\
    \\EOT\ENQ\EOT\STX!\DC2\EOT\229\EOT\STX\CAN\SUBH 'c' or similar, in languages that differentiate strings and characters\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX!\SOH\DC2\EOT\229\EOT\STX\DC2\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX!\STX\DC2\EOT\229\EOT\NAK\ETB\n\
    \9\n\
    \\EOT\ENQ\EOT\STX\"\DC2\EOT\231\EOT\STX\SYN\SUB+ Literal numbers, both floats and integers\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\"\SOH\DC2\EOT\231\EOT\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\"\STX\DC2\EOT\231\EOT\DC3\NAK\n\
    \\US\n\
    \\EOT\ENQ\EOT\STX#\DC2\EOT\233\EOT\STX\SYN\SUB\DC1 `true`, `false`\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX#\SOH\DC2\EOT\233\EOT\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX#\STX\DC2\EOT\233\EOT\DC3\NAK\n\
    \&\n\
    \\EOT\ENQ\EOT\STX$\DC2\EOT\236\EOT\STX\v\SUB\CAN Used for XML-like tags\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX$\SOH\DC2\EOT\236\EOT\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX$\STX\DC2\EOT\236\EOT\b\n\
    \\n\
    \/\n\
    \\EOT\ENQ\EOT\STX%\DC2\EOT\238\EOT\STX\DC4\SUB! Attribute name in XML-like tags\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX%\SOH\DC2\EOT\238\EOT\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX%\STX\DC2\EOT\238\EOT\DC1\DC3\n\
    \,\n\
    \\EOT\ENQ\EOT\STX&\DC2\EOT\240\EOT\STX\DC4\SUB\RS Delimiters for XML-like tags\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX&\SOH\DC2\EOT\240\EOT\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX&\STX\DC2\EOT\240\EOT\DC1\DC3\n\
    \\249\SOH\n\
    \\STX\EOT\t\DC2\ACK\248\EOT\NUL\217\ENQ\SOH\SUB\234\SOH Occurrence associates a source position with a symbol and/or highlighting\n\
    \ information.\n\
    \\n\
    \ If possible, indexers should try to bundle logically related information\n\
    \ across occurrences into a single occurrence to reduce payload sizes.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\t\SOH\DC2\EOT\248\EOT\b\DC2\n\
    \\232\b\n\
    \\EOT\EOT\t\STX\NUL\DC2\EOT\144\ENQ\STX\ESC\SUB\217\b Half-open [start, end) range of this occurrence. Must be exactly three or four\n\
    \ elements:\n\
    \\n\
    \ - Four elements: `[startLine, startCharacter, endLine, endCharacter]`\n\
    \ - Three elements: `[startLine, startCharacter, endCharacter]`. The end line\n\
    \   is inferred to have the same value as the start line.\n\
    \\n\
    \ It is allowed for the range to be empty (i.e. start==end).\n\
    \\n\
    \ Line numbers and characters are always 0-based. Make sure to increment the\n\
    \ line/character values before displaying them in an editor-like UI because\n\
    \ editors conventionally use 1-based numbers.\n\
    \\n\
    \ The 'character' value is interpreted based on the PositionEncoding for\n\
    \ the Document.\n\
    \\n\
    \ Historical note: the original draft of this schema had a `Range` message\n\
    \ type with `start` and `end` fields of type `Position`, mirroring LSP.\n\
    \ Benchmarks revealed that this encoding was inefficient and that we could\n\
    \ reduce the total payload size of an index by 50% by using `repeated int32`\n\
    \ instead. The `repeated int32` encoding is admittedly more embarrassing to\n\
    \ work with in some programming languages but we hope the performance\n\
    \ improvements make up for it.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\NUL\EOT\DC2\EOT\144\ENQ\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\NUL\ENQ\DC2\EOT\144\ENQ\v\DLE\n\
    \\r\n\
    \\ENQ\EOT\t\STX\NUL\SOH\DC2\EOT\144\ENQ\DC1\SYN\n\
    \\r\n\
    \\ENQ\EOT\t\STX\NUL\ETX\DC2\EOT\144\ENQ\EM\SUB\n\
    \\138\SOH\n\
    \\EOT\EOT\t\STX\SOH\DC2\EOT\147\ENQ\STX\DC4\SUB| (optional) The symbol that appears at this position. See\n\
    \ `SymbolInformation.symbol` for how to format symbols as strings.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\SOH\ENQ\DC2\EOT\147\ENQ\STX\b\n\
    \\r\n\
    \\ENQ\EOT\t\STX\SOH\SOH\DC2\EOT\147\ENQ\t\SI\n\
    \\r\n\
    \\ENQ\EOT\t\STX\SOH\ETX\DC2\EOT\147\ENQ\DC2\DC3\n\
    \\151\SOH\n\
    \\EOT\EOT\t\STX\STX\DC2\EOT\150\ENQ\STX\EM\SUB\136\SOH (optional) Bitset containing `SymbolRole`s in this occurrence.\n\
    \ See `SymbolRole`'s documentation for how to read and write this field.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\STX\ENQ\DC2\EOT\150\ENQ\STX\a\n\
    \\r\n\
    \\ENQ\EOT\t\STX\STX\SOH\DC2\EOT\150\ENQ\b\DC4\n\
    \\r\n\
    \\ENQ\EOT\t\STX\STX\ETX\DC2\EOT\150\ENQ\ETB\CAN\n\
    \\241\ETX\n\
    \\EOT\EOT\t\STX\ETX\DC2\EOT\159\ENQ\STX-\SUB\226\ETX (optional) CommonMark-formatted documentation for this specific range. If\n\
    \ empty, the `Symbol.documentation` field is used instead. One example\n\
    \ where this field might be useful is when the symbol represents a generic\n\
    \ function (with abstract type parameters such as `List<T>`) and at this\n\
    \ occurrence we know the exact values (such as `List<String>`).\n\
    \\n\
    \ This field can also be used for dynamically or gradually typed languages,\n\
    \ which commonly allow for type-changing assignment.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ETX\EOT\DC2\EOT\159\ENQ\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ETX\ENQ\DC2\EOT\159\ENQ\v\DC1\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ETX\SOH\DC2\EOT\159\ENQ\DC2(\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ETX\ETX\DC2\EOT\159\ENQ+,\n\
    \X\n\
    \\EOT\EOT\t\STX\EOT\DC2\EOT\161\ENQ\STX\GS\SUBJ (optional) What syntax highlighting class should be used for this range?\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\EOT\ACK\DC2\EOT\161\ENQ\STX\f\n\
    \\r\n\
    \\ENQ\EOT\t\STX\EOT\SOH\DC2\EOT\161\ENQ\r\CAN\n\
    \\r\n\
    \\ENQ\EOT\t\STX\EOT\ETX\DC2\EOT\161\ENQ\ESC\FS\n\
    \W\n\
    \\EOT\EOT\t\STX\ENQ\DC2\EOT\163\ENQ\STX&\SUBI (optional) Diagnostics that have been reported for this specific range.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ENQ\EOT\DC2\EOT\163\ENQ\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ENQ\ACK\DC2\EOT\163\ENQ\v\NAK\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ENQ\SOH\DC2\EOT\163\ENQ\SYN!\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ENQ\ETX\DC2\EOT\163\ENQ$%\n\
    \\183\SO\n\
    \\EOT\EOT\t\STX\ACK\DC2\EOT\216\ENQ\STX%\SUB\168\SO (optional) Using the same encoding as the sibling `range` field, half-open\n\
    \ source range of the nearest non-trivial enclosing AST node. This range must\n\
    \ enclose the `range` field. Example applications that make use of the\n\
    \ enclosing_range field:\n\
    \\n\
    \ - Call hierarchies: to determine what symbols are references from the body\n\
    \   of a function\n\
    \ - Symbol outline: to display breadcrumbs from the cursor position to the\n\
    \   root of the file\n\
    \ - Expand selection: to select the nearest enclosing AST node.\n\
    \ - Highlight range: to indicate the AST expression that is associated with a\n\
    \   hover popover\n\
    \\n\
    \ For definition occurrences, the enclosing range should indicate the\n\
    \ start/end bounds of the entire definition AST node, including\n\
    \ documentation.\n\
    \ ```\n\
    \ const n = 3\n\
    \       ^ range\n\
    \ ^^^^^^^^^^^ enclosing_range\n\
    \\n\
    \ /** Parses the string into something */\n\
    \ ^ enclosing_range start --------------------------------------|\n\
    \ function parse(input string): string {                        |\n\
    \          ^^^^^ range                                          |\n\
    \     return input.slice(n)                                     |\n\
    \ }                                                             |\n\
    \ ^ enclosing_range end <---------------------------------------|\n\
    \ ```\n\
    \\n\
    \ Any attributes/decorators/attached macros should also be part of the\n\
    \ enclosing range.\n\
    \\n\
    \ ```python\n\
    \ @cache\n\
    \ ^ enclosing_range start---------------------|\n\
    \ def factorial(n):                           |\n\
    \     return n * factorial(n-1) if n else 1   |\n\
    \ < enclosing_range end-----------------------|\n\
    \\n\
    \ ```\n\
    \\n\
    \ For reference occurrences, the enclosing range should indicate the start/end\n\
    \ bounds of the parent expression.\n\
    \ ```\n\
    \ const a = a.b\n\
    \             ^ range\n\
    \           ^^^ enclosing_range\n\
    \ const b = a.b(41).f(42).g(43)\n\
    \                   ^ range\n\
    \           ^^^^^^^^^^^^^ enclosing_range\n\
    \ ```\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ACK\EOT\DC2\EOT\216\ENQ\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ACK\ENQ\DC2\EOT\216\ENQ\v\DLE\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ACK\SOH\DC2\EOT\216\ENQ\DC1 \n\
    \\r\n\
    \\ENQ\EOT\t\STX\ACK\ETX\DC2\EOT\216\ENQ#$\n\
    \w\n\
    \\STX\EOT\n\
    \\DC2\ACK\221\ENQ\NUL\232\ENQ\SOH\SUBi Represents a diagnostic, such as a compiler error or warning, which should be\n\
    \ reported for a document.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\n\
    \\SOH\DC2\EOT\221\ENQ\b\DC2\n\
    \W\n\
    \\EOT\EOT\n\
    \\STX\NUL\DC2\EOT\223\ENQ\STX\CAN\SUBI Should this diagnostic be reported as an error, warning, info, or hint?\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ACK\DC2\EOT\223\ENQ\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\NUL\SOH\DC2\EOT\223\ENQ\v\DC3\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ETX\DC2\EOT\223\ENQ\SYN\ETB\n\
    \]\n\
    \\EOT\EOT\n\
    \\STX\SOH\DC2\EOT\225\ENQ\STX\DC2\SUBO (optional) Code of this diagnostic, which might appear in the user interface.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\SOH\ENQ\DC2\EOT\225\ENQ\STX\b\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\SOH\SOH\DC2\EOT\225\ENQ\t\r\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\SOH\ETX\DC2\EOT\225\ENQ\DLE\DC1\n\
    \+\n\
    \\EOT\EOT\n\
    \\STX\STX\DC2\EOT\227\ENQ\STX\NAK\SUB\GS Message of this diagnostic.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\STX\ENQ\DC2\EOT\227\ENQ\STX\b\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\STX\SOH\DC2\EOT\227\ENQ\t\DLE\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\STX\ETX\DC2\EOT\227\ENQ\DC3\DC4\n\
    \~\n\
    \\EOT\EOT\n\
    \\STX\ETX\DC2\EOT\230\ENQ\STX\DC4\SUBp (optional) Human-readable string describing the source of this diagnostic, e.g.\n\
    \ 'typescript' or 'super lint'.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\ETX\ENQ\DC2\EOT\230\ENQ\STX\b\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\ETX\SOH\DC2\EOT\230\ENQ\t\SI\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\ETX\ETX\DC2\EOT\230\ENQ\DC2\DC3\n\
    \\f\n\
    \\EOT\EOT\n\
    \\STX\EOT\DC2\EOT\231\ENQ\STX\"\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\EOT\EOT\DC2\EOT\231\ENQ\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\EOT\ACK\DC2\EOT\231\ENQ\v\CAN\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\EOT\SOH\DC2\EOT\231\ENQ\EM\GS\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\EOT\ETX\DC2\EOT\231\ENQ !\n\
    \\f\n\
    \\STX\ENQ\ENQ\DC2\ACK\234\ENQ\NUL\240\ENQ\SOH\n\
    \\v\n\
    \\ETX\ENQ\ENQ\SOH\DC2\EOT\234\ENQ\ENQ\r\n\
    \\f\n\
    \\EOT\ENQ\ENQ\STX\NUL\DC2\EOT\235\ENQ\STX\SUB\n\
    \\r\n\
    \\ENQ\ENQ\ENQ\STX\NUL\SOH\DC2\EOT\235\ENQ\STX\NAK\n\
    \\r\n\
    \\ENQ\ENQ\ENQ\STX\NUL\STX\DC2\EOT\235\ENQ\CAN\EM\n\
    \\f\n\
    \\EOT\ENQ\ENQ\STX\SOH\DC2\EOT\236\ENQ\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\ENQ\STX\SOH\SOH\DC2\EOT\236\ENQ\STX\a\n\
    \\r\n\
    \\ENQ\ENQ\ENQ\STX\SOH\STX\DC2\EOT\236\ENQ\n\
    \\v\n\
    \\f\n\
    \\EOT\ENQ\ENQ\STX\STX\DC2\EOT\237\ENQ\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\ENQ\STX\STX\SOH\DC2\EOT\237\ENQ\STX\t\n\
    \\r\n\
    \\ENQ\ENQ\ENQ\STX\STX\STX\DC2\EOT\237\ENQ\f\r\n\
    \\f\n\
    \\EOT\ENQ\ENQ\STX\ETX\DC2\EOT\238\ENQ\STX\DC2\n\
    \\r\n\
    \\ENQ\ENQ\ENQ\STX\ETX\SOH\DC2\EOT\238\ENQ\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\ENQ\STX\ETX\STX\DC2\EOT\238\ENQ\DLE\DC1\n\
    \\f\n\
    \\EOT\ENQ\ENQ\STX\EOT\DC2\EOT\239\ENQ\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\ENQ\STX\EOT\SOH\DC2\EOT\239\ENQ\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\ENQ\STX\EOT\STX\DC2\EOT\239\ENQ\t\n\
    \\n\
    \\f\n\
    \\STX\ENQ\ACK\DC2\ACK\242\ENQ\NUL\246\ENQ\SOH\n\
    \\v\n\
    \\ETX\ENQ\ACK\SOH\DC2\EOT\242\ENQ\ENQ\DC2\n\
    \\f\n\
    \\EOT\ENQ\ACK\STX\NUL\DC2\EOT\243\ENQ\STX\US\n\
    \\r\n\
    \\ENQ\ENQ\ACK\STX\NUL\SOH\DC2\EOT\243\ENQ\STX\SUB\n\
    \\r\n\
    \\ENQ\ENQ\ACK\STX\NUL\STX\DC2\EOT\243\ENQ\GS\RS\n\
    \\f\n\
    \\EOT\ENQ\ACK\STX\SOH\DC2\EOT\244\ENQ\STX\DC2\n\
    \\r\n\
    \\ENQ\ENQ\ACK\STX\SOH\SOH\DC2\EOT\244\ENQ\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\ACK\STX\SOH\STX\DC2\EOT\244\ENQ\DLE\DC1\n\
    \\f\n\
    \\EOT\ENQ\ACK\STX\STX\DC2\EOT\245\ENQ\STX\DC1\n\
    \\r\n\
    \\ENQ\ENQ\ACK\STX\STX\SOH\DC2\EOT\245\ENQ\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\ACK\STX\STX\STX\DC2\EOT\245\ENQ\SI\DLE\n\
    \\208\ETX\n\
    \\STX\ENQ\a\DC2\ACK\254\ENQ\NUL\244\ACK\SOH\SUB\193\ETX Language standardises names of common programming languages that can be used\n\
    \ for the `Document.language` field. The primary purpose of this enum is to\n\
    \ prevent a situation where we have a single programming language ends up with\n\
    \ multiple string representations. For example, the C++ language uses the name\n\
    \ \"CPP\" in this enum and other names such as \"cpp\" are incompatible.\n\
    \ Feel free to send a pull-request to add missing programming languages.\n\
    \\n\
    \\v\n\
    \\ETX\ENQ\a\SOH\DC2\EOT\254\ENQ\ENQ\r\n\
    \\f\n\
    \\EOT\ENQ\a\STX\NUL\DC2\EOT\255\ENQ\STX\SUB\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\NUL\SOH\DC2\EOT\255\ENQ\STX\NAK\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\NUL\STX\DC2\EOT\255\ENQ\CAN\EM\n\
    \\f\n\
    \\EOT\ENQ\a\STX\SOH\DC2\EOT\128\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\SOH\SOH\DC2\EOT\128\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\SOH\STX\DC2\EOT\128\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STX\STX\DC2\EOT\129\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\STX\SOH\DC2\EOT\129\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\STX\STX\DC2\EOT\129\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STX\ETX\DC2\EOT\130\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\ETX\SOH\DC2\EOT\130\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\ETX\STX\DC2\EOT\130\ACK\b\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STX\EOT\DC2\EOT\131\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\EOT\SOH\DC2\EOT\131\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\EOT\STX\DC2\EOT\131\ACK\b\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STX\ENQ\DC2\EOT\132\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\ENQ\SOH\DC2\EOT\132\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\ENQ\STX\DC2\EOT\132\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STX\ACK\DC2\EOT\133\ACK\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\ACK\SOH\DC2\EOT\133\ACK\STX\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\ACK\STX\DC2\EOT\133\ACK\r\SI\n\
    \\f\n\
    \\EOT\ENQ\a\STX\a\DC2\EOT\134\ACK\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\a\SOH\DC2\EOT\134\ACK\STX\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\a\STX\DC2\EOT\134\ACK\r\SI\n\
    \\f\n\
    \\EOT\ENQ\a\STX\b\DC2\EOT\135\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\b\SOH\DC2\EOT\135\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\b\STX\DC2\EOT\135\ACK\b\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STX\t\DC2\EOT\136\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\t\SOH\DC2\EOT\136\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\t\STX\DC2\EOT\136\ACK\b\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STX\n\
    \\DC2\EOT\137\ACK\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\n\
    \\SOH\DC2\EOT\137\ACK\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\n\
    \\STX\DC2\EOT\137\ACK\v\r\n\
    \\f\n\
    \\EOT\ENQ\a\STX\v\DC2\EOT\138\ACK\STX\t\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\v\SOH\DC2\EOT\138\ACK\STX\ETX\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\v\STX\DC2\EOT\138\ACK\ACK\b\n\
    \\f\n\
    \\EOT\ENQ\a\STX\f\DC2\EOT\139\ACK\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\f\SOH\DC2\EOT\139\ACK\STX\a\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\f\STX\DC2\EOT\139\ACK\n\
    \\f\n\
    \H\n\
    \\EOT\ENQ\a\STX\r\DC2\EOT\140\ACK\STX\v\": C++ (the name \"CPP\" was chosen for consistency with LSP)\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\r\SOH\DC2\EOT\140\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\r\STX\DC2\EOT\140\ACK\b\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STX\SO\DC2\EOT\141\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\SO\SOH\DC2\EOT\141\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\SO\STX\DC2\EOT\141\ACK\b\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STX\SI\DC2\EOT\142\ACK\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\SI\SOH\DC2\EOT\142\ACK\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\SI\STX\DC2\EOT\142\ACK\v\f\n\
    \\f\n\
    \\EOT\ENQ\a\STX\DLE\DC2\EOT\143\ACK\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\DLE\SOH\DC2\EOT\143\ACK\STX\t\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\DLE\STX\DC2\EOT\143\ACK\f\r\n\
    \\f\n\
    \\EOT\ENQ\a\STX\DC1\DC2\EOT\144\ACK\STX\DC4\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\DC1\SOH\DC2\EOT\144\ACK\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\DC1\STX\DC2\EOT\144\ACK\DC1\DC3\n\
    \\f\n\
    \\EOT\ENQ\a\STX\DC2\DC2\EOT\145\ACK\STX\DC1\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\DC2\SOH\DC2\EOT\145\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\DC2\STX\DC2\EOT\145\ACK\SI\DLE\n\
    \\f\n\
    \\EOT\ENQ\a\STX\DC3\DC2\EOT\146\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\DC3\SOH\DC2\EOT\146\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\DC3\STX\DC2\EOT\146\ACK\b\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STX\DC4\DC2\EOT\147\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\DC4\SOH\DC2\EOT\147\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\DC4\STX\DC2\EOT\147\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STX\NAK\DC2\EOT\148\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\NAK\SOH\DC2\EOT\148\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\NAK\STX\DC2\EOT\148\ACK\t\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STX\SYN\DC2\EOT\149\ACK\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\SYN\SOH\DC2\EOT\149\ACK\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\SYN\STX\DC2\EOT\149\ACK\v\r\n\
    \\f\n\
    \\EOT\ENQ\a\STX\ETB\DC2\EOT\150\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\ETB\SOH\DC2\EOT\150\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\ETB\STX\DC2\EOT\150\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STX\CAN\DC2\EOT\151\ACK\STX\DC2\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\CAN\SOH\DC2\EOT\151\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\CAN\STX\DC2\EOT\151\ACK\SI\DC1\n\
    \\f\n\
    \\EOT\ENQ\a\STX\EM\DC2\EOT\152\ACK\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\EM\SOH\DC2\EOT\152\ACK\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\EM\STX\DC2\EOT\152\ACK\v\r\n\
    \\f\n\
    \\EOT\ENQ\a\STX\SUB\DC2\EOT\153\ACK\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\SUB\SOH\DC2\EOT\153\ACK\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\SUB\STX\DC2\EOT\153\ACK\v\r\n\
    \\f\n\
    \\EOT\ENQ\a\STX\ESC\DC2\EOT\154\ACK\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\ESC\SOH\DC2\EOT\154\ACK\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\ESC\STX\DC2\EOT\154\ACK\v\r\n\
    \\f\n\
    \\EOT\ENQ\a\STX\FS\DC2\EOT\155\ACK\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\FS\SOH\DC2\EOT\155\ACK\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\FS\STX\DC2\EOT\155\ACK\v\r\n\
    \\f\n\
    \\EOT\ENQ\a\STX\GS\DC2\EOT\156\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\GS\SOH\DC2\EOT\156\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\GS\STX\DC2\EOT\156\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STX\RS\DC2\EOT\157\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\RS\SOH\DC2\EOT\157\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\RS\STX\DC2\EOT\157\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STX\US\DC2\EOT\158\ACK\STX\SI\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\US\SOH\DC2\EOT\158\ACK\STX\t\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\US\STX\DC2\EOT\158\ACK\f\SO\n\
    \\f\n\
    \\EOT\ENQ\a\STX \DC2\EOT\159\ACK\STX\DC2\n\
    \\r\n\
    \\ENQ\ENQ\a\STX \SOH\DC2\EOT\159\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX \STX\DC2\EOT\159\ACK\SI\DC1\n\
    \\f\n\
    \\EOT\ENQ\a\STX!\DC2\EOT\160\ACK\STX\DC2\n\
    \\r\n\
    \\ENQ\ENQ\a\STX!\SOH\DC2\EOT\160\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX!\STX\DC2\EOT\160\ACK\SI\DC1\n\
    \\f\n\
    \\EOT\ENQ\a\STX\"\DC2\EOT\161\ACK\STX\DC2\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\"\SOH\DC2\EOT\161\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\"\STX\DC2\EOT\161\ACK\SI\DC1\n\
    \\f\n\
    \\EOT\ENQ\a\STX#\DC2\EOT\162\ACK\STX\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\a\STX#\SOH\DC2\EOT\162\ACK\STX\EOT\n\
    \\r\n\
    \\ENQ\ENQ\a\STX#\STX\DC2\EOT\162\ACK\a\t\n\
    \\f\n\
    \\EOT\ENQ\a\STX$\DC2\EOT\163\ACK\STX\SI\n\
    \\r\n\
    \\ENQ\ENQ\a\STX$\SOH\DC2\EOT\163\ACK\STX\t\n\
    \\r\n\
    \\ENQ\ENQ\a\STX$\STX\DC2\EOT\163\ACK\f\SO\n\
    \\f\n\
    \\EOT\ENQ\a\STX%\DC2\EOT\164\ACK\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\a\STX%\SOH\DC2\EOT\164\ACK\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\a\STX%\STX\DC2\EOT\164\ACK\v\f\n\
    \\f\n\
    \\EOT\ENQ\a\STX&\DC2\EOT\165\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX&\SOH\DC2\EOT\165\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STX&\STX\DC2\EOT\165\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STX'\DC2\EOT\166\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX'\SOH\DC2\EOT\166\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STX'\STX\DC2\EOT\166\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STX(\DC2\EOT\167\ACK\STX\DC2\n\
    \\r\n\
    \\ENQ\ENQ\a\STX(\SOH\DC2\EOT\167\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX(\STX\DC2\EOT\167\ACK\SI\DC1\n\
    \\f\n\
    \\EOT\ENQ\a\STX)\DC2\EOT\168\ACK\STX\SI\n\
    \\r\n\
    \\ENQ\ENQ\a\STX)\SOH\DC2\EOT\168\ACK\STX\t\n\
    \\r\n\
    \\ENQ\ENQ\a\STX)\STX\DC2\EOT\168\ACK\f\SO\n\
    \\f\n\
    \\EOT\ENQ\a\STX*\DC2\EOT\169\ACK\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\a\STX*\SOH\DC2\EOT\169\ACK\STX\a\n\
    \\r\n\
    \\ENQ\ENQ\a\STX*\STX\DC2\EOT\169\ACK\n\
    \\f\n\
    \\f\n\
    \\EOT\ENQ\a\STX+\DC2\EOT\170\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STX+\SOH\DC2\EOT\170\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STX+\STX\DC2\EOT\170\ACK\b\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STX,\DC2\EOT\171\ACK\STX\t\n\
    \\r\n\
    \\ENQ\ENQ\a\STX,\SOH\DC2\EOT\171\ACK\STX\ETX\n\
    \\r\n\
    \\ENQ\ENQ\a\STX,\STX\DC2\EOT\171\ACK\ACK\b\n\
    \\f\n\
    \\EOT\ENQ\a\STX-\DC2\EOT\172\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX-\SOH\DC2\EOT\172\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STX-\STX\DC2\EOT\172\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STX.\DC2\EOT\173\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STX.\SOH\DC2\EOT\173\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STX.\STX\DC2\EOT\173\ACK\t\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STX/\DC2\EOT\174\ACK\STX\DC2\n\
    \\r\n\
    \\ENQ\ENQ\a\STX/\SOH\DC2\EOT\174\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX/\STX\DC2\EOT\174\ACK\SI\DC1\n\
    \\f\n\
    \\EOT\ENQ\a\STX0\DC2\EOT\175\ACK\STX\ETB\n\
    \\r\n\
    \\ENQ\ENQ\a\STX0\SOH\DC2\EOT\175\ACK\STX\DC1\n\
    \\r\n\
    \\ENQ\ENQ\a\STX0\STX\DC2\EOT\175\ACK\DC4\SYN\n\
    \\f\n\
    \\EOT\ENQ\a\STX1\DC2\EOT\176\ACK\STX\SI\n\
    \\r\n\
    \\ENQ\ENQ\a\STX1\SOH\DC2\EOT\176\ACK\STX\t\n\
    \\r\n\
    \\ENQ\ENQ\a\STX1\STX\DC2\EOT\176\ACK\f\SO\n\
    \\f\n\
    \\EOT\ENQ\a\STX2\DC2\EOT\177\ACK\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\a\STX2\SOH\DC2\EOT\177\ACK\STX\a\n\
    \\r\n\
    \\ENQ\ENQ\a\STX2\STX\DC2\EOT\177\ACK\v\r\n\
    \\f\n\
    \\EOT\ENQ\a\STX3\DC2\EOT\178\ACK\STX\DC1\n\
    \\r\n\
    \\ENQ\ENQ\a\STX3\SOH\DC2\EOT\178\ACK\STX\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\a\STX3\STX\DC2\EOT\178\ACK\r\DLE\n\
    \\f\n\
    \\EOT\ENQ\a\STX4\DC2\EOT\179\ACK\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\a\STX4\SOH\DC2\EOT\179\ACK\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\a\STX4\STX\DC2\EOT\179\ACK\v\f\n\
    \\f\n\
    \\EOT\ENQ\a\STX5\DC2\EOT\180\ACK\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\a\STX5\SOH\DC2\EOT\180\ACK\STX\a\n\
    \\r\n\
    \\ENQ\ENQ\a\STX5\STX\DC2\EOT\180\ACK\n\
    \\f\n\
    \\f\n\
    \\EOT\ENQ\a\STX6\DC2\EOT\181\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX6\SOH\DC2\EOT\181\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STX6\STX\DC2\EOT\181\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STX7\DC2\EOT\182\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX7\SOH\DC2\EOT\182\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STX7\STX\DC2\EOT\182\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STX8\DC2\EOT\183\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STX8\SOH\DC2\EOT\183\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STX8\STX\DC2\EOT\183\ACK\b\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STX9\DC2\EOT\184\ACK\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\a\STX9\SOH\DC2\EOT\184\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STX9\STX\DC2\EOT\184\ACK\t\f\n\
    \\f\n\
    \\EOT\ENQ\a\STX:\DC2\EOT\185\ACK\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\a\STX:\SOH\DC2\EOT\185\ACK\STX\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\a\STX:\STX\DC2\EOT\185\ACK\r\SI\n\
    \\f\n\
    \\EOT\ENQ\a\STX;\DC2\EOT\186\ACK\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\a\STX;\SOH\DC2\EOT\186\ACK\STX\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\a\STX;\STX\DC2\EOT\186\ACK\r\SI\n\
    \\f\n\
    \\EOT\ENQ\a\STX<\DC2\EOT\187\ACK\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\a\STX<\SOH\DC2\EOT\187\ACK\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\a\STX<\STX\DC2\EOT\187\ACK\v\r\n\
    \(\n\
    \\EOT\ENQ\a\STX=\DC2\EOT\188\ACK\STX\SI\"\SUB https://nickel-lang.org/\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\a\STX=\SOH\DC2\EOT\188\ACK\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\a\STX=\STX\DC2\EOT\188\ACK\v\SO\n\
    \\f\n\
    \\EOT\ENQ\a\STX>\DC2\EOT\189\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STX>\SOH\DC2\EOT\189\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STX>\STX\DC2\EOT\189\ACK\b\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STX?\DC2\EOT\190\ACK\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\a\STX?\SOH\DC2\EOT\190\ACK\STX\a\n\
    \\r\n\
    \\ENQ\ENQ\a\STX?\STX\DC2\EOT\190\ACK\n\
    \\f\n\
    \\f\n\
    \\EOT\ENQ\a\STX@\DC2\EOT\191\ACK\STX\DC3\n\
    \\r\n\
    \\ENQ\ENQ\a\STX@\SOH\DC2\EOT\191\ACK\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\a\STX@\STX\DC2\EOT\191\ACK\DLE\DC2\n\
    \\f\n\
    \\EOT\ENQ\a\STXA\DC2\EOT\192\ACK\STX\NAK\n\
    \\r\n\
    \\ENQ\ENQ\a\STXA\SOH\DC2\EOT\192\ACK\STX\SI\n\
    \\r\n\
    \\ENQ\ENQ\a\STXA\STX\DC2\EOT\192\ACK\DC2\DC4\n\
    \\f\n\
    \\EOT\ENQ\a\STXB\DC2\EOT\193\ACK\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\a\STXB\SOH\DC2\EOT\193\ACK\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\a\STXB\STX\DC2\EOT\193\ACK\v\r\n\
    \\f\n\
    \\EOT\ENQ\a\STXC\DC2\EOT\194\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STXC\SOH\DC2\EOT\194\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STXC\STX\DC2\EOT\194\ACK\b\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STXD\DC2\EOT\195\ACK\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\a\STXD\SOH\DC2\EOT\195\ACK\STX\a\n\
    \\r\n\
    \\ENQ\ENQ\a\STXD\STX\DC2\EOT\195\ACK\n\
    \\f\n\
    \\f\n\
    \\EOT\ENQ\a\STXE\DC2\EOT\196\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STXE\SOH\DC2\EOT\196\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STXE\STX\DC2\EOT\196\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STXF\DC2\EOT\197\ACK\STX\DC2\n\
    \\r\n\
    \\ENQ\ENQ\a\STXF\SOH\DC2\EOT\197\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STXF\STX\DC2\EOT\197\ACK\SI\DC1\n\
    \\f\n\
    \\EOT\ENQ\a\STXG\DC2\EOT\198\ACK\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\a\STXG\SOH\DC2\EOT\198\ACK\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\a\STXG\STX\DC2\EOT\198\ACK\v\r\n\
    \\f\n\
    \\EOT\ENQ\a\STXH\DC2\EOT\199\ACK\STX\DC1\n\
    \\r\n\
    \\ENQ\ENQ\a\STXH\SOH\DC2\EOT\199\ACK\STX\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\a\STXH\STX\DC2\EOT\199\ACK\r\DLE\n\
    \\f\n\
    \\EOT\ENQ\a\STXI\DC2\EOT\200\ACK\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\a\STXI\SOH\DC2\EOT\200\ACK\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\a\STXI\STX\DC2\EOT\200\ACK\v\r\n\
    \\f\n\
    \\EOT\ENQ\a\STXJ\DC2\EOT\201\ACK\STX\t\n\
    \\r\n\
    \\ENQ\ENQ\a\STXJ\SOH\DC2\EOT\201\ACK\STX\ETX\n\
    \\r\n\
    \\ENQ\ENQ\a\STXJ\STX\DC2\EOT\201\ACK\ACK\b\n\
    \\f\n\
    \\EOT\ENQ\a\STXK\DC2\EOT\202\ACK\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\a\STXK\SOH\DC2\EOT\202\ACK\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\a\STXK\STX\DC2\EOT\202\ACK\v\r\n\
    \\f\n\
    \\EOT\ENQ\a\STXL\DC2\EOT\203\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STXL\SOH\DC2\EOT\203\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STXL\STX\DC2\EOT\203\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STXM\DC2\EOT\204\ACK\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\a\STXM\SOH\DC2\EOT\204\ACK\STX\a\n\
    \\r\n\
    \\ENQ\ENQ\a\STXM\STX\DC2\EOT\204\ACK\n\
    \\f\n\
    \2\n\
    \\EOT\ENQ\a\STXN\DC2\EOT\205\ACK\STX\SO\"$ Internal language for testing SCIP\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\a\STXN\SOH\DC2\EOT\205\ACK\STX\a\n\
    \\r\n\
    \\ENQ\ENQ\a\STXN\STX\DC2\EOT\205\ACK\n\
    \\r\n\
    \\f\n\
    \\EOT\ENQ\a\STXO\DC2\EOT\206\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STXO\SOH\DC2\EOT\206\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STXO\STX\DC2\EOT\206\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STXP\DC2\EOT\207\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STXP\SOH\DC2\EOT\207\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STXP\STX\DC2\EOT\207\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STXQ\DC2\EOT\208\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STXQ\SOH\DC2\EOT\208\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STXQ\STX\DC2\EOT\208\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STXR\DC2\EOT\209\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STXR\SOH\DC2\EOT\209\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STXR\STX\DC2\EOT\209\ACK\b\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STXS\DC2\EOT\210\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STXS\SOH\DC2\EOT\210\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STXS\STX\DC2\EOT\210\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STXT\DC2\EOT\211\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STXT\SOH\DC2\EOT\211\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STXT\STX\DC2\EOT\211\ACK\b\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STXU\DC2\EOT\212\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STXU\SOH\DC2\EOT\212\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STXU\STX\DC2\EOT\212\ACK\b\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STXV\DC2\EOT\213\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STXV\SOH\DC2\EOT\213\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STXV\STX\DC2\EOT\213\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STXW\DC2\EOT\214\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STXW\SOH\DC2\EOT\214\ACK\STX\a\n\
    \\r\n\
    \\ENQ\ENQ\a\STXW\STX\DC2\EOT\214\ACK\n\
    \\v\n\
    \\f\n\
    \\EOT\ENQ\a\STXX\DC2\EOT\215\ACK\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\a\STXX\SOH\DC2\EOT\215\ACK\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\a\STXX\STX\DC2\EOT\215\ACK\v\r\n\
    \\DC4\n\
    \\EOT\ENQ\a\STXY\DC2\EOT\216\ACK\STX\DC3\"\ACK Bash\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\a\STXY\SOH\DC2\EOT\216\ACK\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\a\STXY\STX\DC2\EOT\216\ACK\DLE\DC2\n\
    \\f\n\
    \\EOT\ENQ\a\STXZ\DC2\EOT\217\ACK\STX\SI\n\
    \\r\n\
    \\ENQ\ENQ\a\STXZ\SOH\DC2\EOT\217\ACK\STX\t\n\
    \\r\n\
    \\ENQ\ENQ\a\STXZ\STX\DC2\EOT\217\ACK\f\SO\n\
    \\f\n\
    \\EOT\ENQ\a\STX[\DC2\EOT\218\ACK\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\a\STX[\SOH\DC2\EOT\218\ACK\STX\a\n\
    \\r\n\
    \\ENQ\ENQ\a\STX[\STX\DC2\EOT\218\ACK\n\
    \\r\n\
    \\f\n\
    \\EOT\ENQ\a\STX\\\DC2\EOT\219\ACK\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\\\SOH\DC2\EOT\219\ACK\STX\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\a\STX\\\STX\DC2\EOT\219\ACK\r\SI\n\
    \\f\n\
    \\EOT\ENQ\a\STX]\DC2\EOT\220\ACK\STX\SI\n\
    \\r\n\
    \\ENQ\ENQ\a\STX]\SOH\DC2\EOT\220\ACK\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\a\STX]\STX\DC2\EOT\220\ACK\v\SO\n\
    \\f\n\
    \\EOT\ENQ\a\STX^\DC2\EOT\221\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX^\SOH\DC2\EOT\221\ACK\STX\a\n\
    \\r\n\
    \\ENQ\ENQ\a\STX^\STX\DC2\EOT\221\ACK\n\
    \\v\n\
    \\f\n\
    \\EOT\ENQ\a\STX_\DC2\EOT\222\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX_\SOH\DC2\EOT\222\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STX_\STX\DC2\EOT\222\ACK\b\v\n\
    \\f\n\
    \\EOT\ENQ\a\STX`\DC2\EOT\223\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STX`\SOH\DC2\EOT\223\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STX`\STX\DC2\EOT\223\ACK\t\v\n\
    \\f\n\
    \\EOT\ENQ\a\STXa\DC2\EOT\224\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STXa\SOH\DC2\EOT\224\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STXa\STX\DC2\EOT\224\ACK\b\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STXb\DC2\EOT\225\ACK\STX\SI\n\
    \\r\n\
    \\ENQ\ENQ\a\STXb\SOH\DC2\EOT\225\ACK\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\a\STXb\STX\DC2\EOT\225\ACK\v\SO\n\
    \\f\n\
    \\EOT\ENQ\a\STXc\DC2\EOT\226\ACK\STX\DC2\n\
    \\r\n\
    \\ENQ\ENQ\a\STXc\SOH\DC2\EOT\226\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STXc\STX\DC2\EOT\226\ACK\SI\DC1\n\
    \\f\n\
    \\EOT\ENQ\a\STXd\DC2\EOT\227\ACK\STX\ETB\n\
    \\r\n\
    \\ENQ\ENQ\a\STXd\SOH\DC2\EOT\227\ACK\STX\DC1\n\
    \\r\n\
    \\ENQ\ENQ\a\STXd\STX\DC2\EOT\227\ACK\DC4\SYN\n\
    \\f\n\
    \\EOT\ENQ\a\STXe\DC2\EOT\228\ACK\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\a\STXe\SOH\DC2\EOT\228\ACK\STX\t\n\
    \\r\n\
    \\ENQ\ENQ\a\STXe\STX\DC2\EOT\228\ACK\f\SI\n\
    \\f\n\
    \\EOT\ENQ\a\STXf\DC2\EOT\229\ACK\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\a\STXf\SOH\DC2\EOT\229\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STXf\STX\DC2\EOT\229\ACK\t\f\n\
    \\f\n\
    \\EOT\ENQ\a\STXg\DC2\EOT\230\ACK\STX\DC3\n\
    \\r\n\
    \\ENQ\ENQ\a\STXg\SOH\DC2\EOT\230\ACK\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\a\STXg\STX\DC2\EOT\230\ACK\DLE\DC2\n\
    \\f\n\
    \\EOT\ENQ\a\STXh\DC2\EOT\231\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STXh\SOH\DC2\EOT\231\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STXh\STX\DC2\EOT\231\ACK\b\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STXi\DC2\EOT\232\ACK\STX\SI\n\
    \\r\n\
    \\ENQ\ENQ\a\STXi\SOH\DC2\EOT\232\ACK\STX\t\n\
    \\r\n\
    \\ENQ\ENQ\a\STXi\STX\DC2\EOT\232\ACK\f\SO\n\
    \\f\n\
    \\EOT\ENQ\a\STXj\DC2\EOT\233\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STXj\SOH\DC2\EOT\233\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STXj\STX\DC2\EOT\233\ACK\b\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STXk\DC2\EOT\234\ACK\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\a\STXk\SOH\DC2\EOT\234\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STXk\STX\DC2\EOT\234\ACK\b\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\a\STXl\DC2\EOT\235\ACK\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\a\STXl\SOH\DC2\EOT\235\ACK\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\a\STXl\STX\DC2\EOT\235\ACK\t\v\n\
    \\147\ETX\n\
    \\EOT\ENQ\a\STXm\DC2\EOT\236\ACK\STX\v\"\132\ETX NextLanguage = 111;\n\
    \ Steps add a new language:\n\
    \ 1. Copy-paste the \"NextLanguage = N\" line above\n\
    \ 2. Increment \"NextLanguage = N\" to \"NextLanguage = N+1\"\n\
    \ 3. Replace \"NextLanguage = N\" with the name of the new language.\n\
    \ 4. Move the new language to the correct line above using alphabetical order\n\
    \ 5. (optional) Add a brief comment behind the language if the name is not self-explanatory\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\a\STXm\SOH\DC2\EOT\236\ACK\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\a\STXm\STX\DC2\EOT\236\ACK\b\n\
    \b\ACKproto3"
