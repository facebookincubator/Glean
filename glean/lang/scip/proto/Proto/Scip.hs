{- This file was auto-generated from scip.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Scip (
        Descriptor(), Descriptor'Suffix(..), Descriptor'Suffix(),
        Descriptor'Suffix'UnrecognizedValue, Diagnostic(),
        DiagnosticTag(..), DiagnosticTag(),
        DiagnosticTag'UnrecognizedValue, Document(), Index(), Metadata(),
        Occurrence(), Package(), ProtocolVersion(..), ProtocolVersion(),
        ProtocolVersion'UnrecognizedValue, Relationship(), Severity(..),
        Severity(), Severity'UnrecognizedValue, Symbol(),
        SymbolInformation(), SymbolRole(..), SymbolRole(),
        SymbolRole'UnrecognizedValue, SyntaxKind(..), SyntaxKind(),
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
      \\ACKsuffix\CAN\ETX \SOH(\SO2\ETB.scip.Descriptor.SuffixR\ACKsuffix\"\131\SOH\n\
      \\ACKSuffix\DC2\NAK\n\
      \\DC1UnspecifiedSuffix\DLE\NUL\DC2\v\n\
      \\aPackage\DLE\SOH\DC2\b\n\
      \\EOTType\DLE\STX\DC2\b\n\
      \\EOTTerm\DLE\ETX\DC2\n\
      \\n\
      \\ACKMethod\DLE\EOT\DC2\DC1\n\
      \\rTypeParameter\DLE\ENQ\DC2\r\n\
      \\tParameter\DLE\ACK\DC2\b\n\
      \\EOTMeta\DLE\a\DC2\t\n\
      \\ENQLocal\DLE\b"
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
    Descriptor'Package |
    Descriptor'Type |
    Descriptor'Term |
    Descriptor'Method |
    Descriptor'TypeParameter |
    Descriptor'Parameter |
    Descriptor'Meta |
    Descriptor'Local |
    Descriptor'Suffix'Unrecognized !Descriptor'Suffix'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum Descriptor'Suffix where
  maybeToEnum 0 = Prelude.Just Descriptor'UnspecifiedSuffix
  maybeToEnum 1 = Prelude.Just Descriptor'Package
  maybeToEnum 2 = Prelude.Just Descriptor'Type
  maybeToEnum 3 = Prelude.Just Descriptor'Term
  maybeToEnum 4 = Prelude.Just Descriptor'Method
  maybeToEnum 5 = Prelude.Just Descriptor'TypeParameter
  maybeToEnum 6 = Prelude.Just Descriptor'Parameter
  maybeToEnum 7 = Prelude.Just Descriptor'Meta
  maybeToEnum 8 = Prelude.Just Descriptor'Local
  maybeToEnum k
    = Prelude.Just
        (Descriptor'Suffix'Unrecognized
           (Descriptor'Suffix'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum Descriptor'UnspecifiedSuffix = "UnspecifiedSuffix"
  showEnum Descriptor'Package = "Package"
  showEnum Descriptor'Type = "Type"
  showEnum Descriptor'Term = "Term"
  showEnum Descriptor'Method = "Method"
  showEnum Descriptor'TypeParameter = "TypeParameter"
  showEnum Descriptor'Parameter = "Parameter"
  showEnum Descriptor'Meta = "Meta"
  showEnum Descriptor'Local = "Local"
  showEnum
    (Descriptor'Suffix'Unrecognized (Descriptor'Suffix'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "UnspecifiedSuffix"
    = Prelude.Just Descriptor'UnspecifiedSuffix
    | (Prelude.==) k "Package" = Prelude.Just Descriptor'Package
    | (Prelude.==) k "Type" = Prelude.Just Descriptor'Type
    | (Prelude.==) k "Term" = Prelude.Just Descriptor'Term
    | (Prelude.==) k "Method" = Prelude.Just Descriptor'Method
    | (Prelude.==) k "TypeParameter"
    = Prelude.Just Descriptor'TypeParameter
    | (Prelude.==) k "Parameter" = Prelude.Just Descriptor'Parameter
    | (Prelude.==) k "Meta" = Prelude.Just Descriptor'Meta
    | (Prelude.==) k "Local" = Prelude.Just Descriptor'Local
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded Descriptor'Suffix where
  minBound = Descriptor'UnspecifiedSuffix
  maxBound = Descriptor'Local
instance Prelude.Enum Descriptor'Suffix where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum Suffix: " (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum Descriptor'UnspecifiedSuffix = 0
  fromEnum Descriptor'Package = 1
  fromEnum Descriptor'Type = 2
  fromEnum Descriptor'Term = 3
  fromEnum Descriptor'Method = 4
  fromEnum Descriptor'TypeParameter = 5
  fromEnum Descriptor'Parameter = 6
  fromEnum Descriptor'Meta = 7
  fromEnum Descriptor'Local = 8
  fromEnum
    (Descriptor'Suffix'Unrecognized (Descriptor'Suffix'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ Descriptor'Local
    = Prelude.error
        "Descriptor'Suffix.succ: bad argument Descriptor'Local. This value would be out of bounds."
  succ Descriptor'UnspecifiedSuffix = Descriptor'Package
  succ Descriptor'Package = Descriptor'Type
  succ Descriptor'Type = Descriptor'Term
  succ Descriptor'Term = Descriptor'Method
  succ Descriptor'Method = Descriptor'TypeParameter
  succ Descriptor'TypeParameter = Descriptor'Parameter
  succ Descriptor'Parameter = Descriptor'Meta
  succ Descriptor'Meta = Descriptor'Local
  succ (Descriptor'Suffix'Unrecognized _)
    = Prelude.error
        "Descriptor'Suffix.succ: bad argument: unrecognized value"
  pred Descriptor'UnspecifiedSuffix
    = Prelude.error
        "Descriptor'Suffix.pred: bad argument Descriptor'UnspecifiedSuffix. This value would be out of bounds."
  pred Descriptor'Package = Descriptor'UnspecifiedSuffix
  pred Descriptor'Type = Descriptor'Package
  pred Descriptor'Term = Descriptor'Type
  pred Descriptor'Method = Descriptor'Term
  pred Descriptor'TypeParameter = Descriptor'Method
  pred Descriptor'Parameter = Descriptor'TypeParameter
  pred Descriptor'Meta = Descriptor'Parameter
  pred Descriptor'Local = Descriptor'Meta
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
     
         * 'Proto.Scip_Fields.relativePath' @:: Lens' Document Data.Text.Text@
         * 'Proto.Scip_Fields.occurrences' @:: Lens' Document [Occurrence]@
         * 'Proto.Scip_Fields.vec'occurrences' @:: Lens' Document (Data.Vector.Vector Occurrence)@
         * 'Proto.Scip_Fields.symbols' @:: Lens' Document [SymbolInformation]@
         * 'Proto.Scip_Fields.vec'symbols' @:: Lens' Document (Data.Vector.Vector SymbolInformation)@ -}
data Document
  = Document'_constructor {_Document'relativePath :: !Data.Text.Text,
                           _Document'occurrences :: !(Data.Vector.Vector Occurrence),
                           _Document'symbols :: !(Data.Vector.Vector SymbolInformation),
                           _Document'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Document where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
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
instance Data.ProtoLens.Message Document where
  messageName _ = Data.Text.pack "scip.Document"
  packedMessageDescriptor _
    = "\n\
      \\bDocument\DC2#\n\
      \\rrelative_path\CAN\SOH \SOH(\tR\frelativePath\DC22\n\
      \\voccurrences\CAN\STX \ETX(\v2\DLE.scip.OccurrenceR\voccurrences\DC21\n\
      \\asymbols\CAN\ETX \ETX(\v2\ETB.scip.SymbolInformationR\asymbols"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
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
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, relativePath__field_descriptor),
           (Data.ProtoLens.Tag 2, occurrences__field_descriptor),
           (Data.ProtoLens.Tag 3, symbols__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Document'_unknownFields
        (\ x__ y__ -> x__ {_Document'_unknownFields = y__})
  defMessage
    = Document'_constructor
        {_Document'relativePath = Data.ProtoLens.fieldDefault,
         _Document'occurrences = Data.Vector.Generic.empty,
         _Document'symbols = Data.Vector.Generic.empty,
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
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData Document where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Document'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Document'relativePath x__)
                (Control.DeepSeq.deepseq
                   (_Document'occurrences x__)
                   (Control.DeepSeq.deepseq (_Document'symbols x__) ())))
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
         * 'Proto.Scip_Fields.vec'diagnostics' @:: Lens' Occurrence (Data.Vector.Vector Diagnostic)@ -}
data Occurrence
  = Occurrence'_constructor {_Occurrence'range :: !(Data.Vector.Unboxed.Vector Data.Int.Int32),
                             _Occurrence'symbol :: !Data.Text.Text,
                             _Occurrence'symbolRoles :: !Data.Int.Int32,
                             _Occurrence'overrideDocumentation :: !(Data.Vector.Vector Data.Text.Text),
                             _Occurrence'syntaxKind :: !SyntaxKind,
                             _Occurrence'diagnostics :: !(Data.Vector.Vector Diagnostic),
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
      \\vdiagnostics\CAN\ACK \ETX(\v2\DLE.scip.DiagnosticR\vdiagnostics"
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
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, range__field_descriptor),
           (Data.ProtoLens.Tag 2, symbol__field_descriptor),
           (Data.ProtoLens.Tag 3, symbolRoles__field_descriptor),
           (Data.ProtoLens.Tag 4, overrideDocumentation__field_descriptor),
           (Data.ProtoLens.Tag 5, syntaxKind__field_descriptor),
           (Data.ProtoLens.Tag 6, diagnostics__field_descriptor)]
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
         _Occurrence'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Occurrence
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Diagnostic
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Text.Text
                -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Int.Int32
                   -> Data.ProtoLens.Encoding.Bytes.Parser Occurrence
        loop
          x
          mutable'diagnostics
          mutable'overrideDocumentation
          mutable'range
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'diagnostics <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                              (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                 mutable'diagnostics)
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
                                 (Data.ProtoLens.Field.field @"vec'overrideDocumentation")
                                 frozen'overrideDocumentation
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"vec'range") frozen'range x))))
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
                                loop x mutable'diagnostics mutable'overrideDocumentation v
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
                                loop x mutable'diagnostics mutable'overrideDocumentation y
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
                                  mutable'diagnostics mutable'overrideDocumentation mutable'range
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "symbol_roles"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"symbolRoles") y x)
                                  mutable'diagnostics mutable'overrideDocumentation mutable'range
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
                                loop x mutable'diagnostics v mutable'range
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
                                  mutable'diagnostics mutable'overrideDocumentation mutable'range
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
                                loop x v mutable'overrideDocumentation mutable'range
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'diagnostics mutable'overrideDocumentation mutable'range
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'diagnostics <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       Data.ProtoLens.Encoding.Growing.new
              mutable'overrideDocumentation <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                 Data.ProtoLens.Encoding.Growing.new
              mutable'range <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              loop
                Data.ProtoLens.defMessage mutable'diagnostics
                mutable'overrideDocumentation mutable'range)
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
                            (Data.ProtoLens.Encoding.Wire.buildFieldSet
                               (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))))
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
                            (Control.DeepSeq.deepseq (_Occurrence'diagnostics x__) ()))))))
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
         * 'Proto.Scip_Fields.isTypeDefinition' @:: Lens' Relationship Prelude.Bool@ -}
data Relationship
  = Relationship'_constructor {_Relationship'symbol :: !Data.Text.Text,
                               _Relationship'isReference :: !Prelude.Bool,
                               _Relationship'isImplementation :: !Prelude.Bool,
                               _Relationship'isTypeDefinition :: !Prelude.Bool,
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
instance Data.ProtoLens.Message Relationship where
  messageName _ = Data.Text.pack "scip.Relationship"
  packedMessageDescriptor _
    = "\n\
      \\fRelationship\DC2\SYN\n\
      \\ACKsymbol\CAN\SOH \SOH(\tR\ACKsymbol\DC2!\n\
      \\fis_reference\CAN\STX \SOH(\bR\visReference\DC2+\n\
      \\DC1is_implementation\CAN\ETX \SOH(\bR\DLEisImplementation\DC2,\n\
      \\DC2is_type_definition\CAN\EOT \SOH(\bR\DLEisTypeDefinition"
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
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, symbol__field_descriptor),
           (Data.ProtoLens.Tag 2, isReference__field_descriptor),
           (Data.ProtoLens.Tag 3, isImplementation__field_descriptor),
           (Data.ProtoLens.Tag 4, isTypeDefinition__field_descriptor)]
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
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
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
                         (_Relationship'isTypeDefinition x__) ()))))
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
         * 'Proto.Scip_Fields.vec'relationships' @:: Lens' SymbolInformation (Data.Vector.Vector Relationship)@ -}
data SymbolInformation
  = SymbolInformation'_constructor {_SymbolInformation'symbol :: !Data.Text.Text,
                                    _SymbolInformation'documentation :: !(Data.Vector.Vector Data.Text.Text),
                                    _SymbolInformation'relationships :: !(Data.Vector.Vector Relationship),
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
instance Data.ProtoLens.Message SymbolInformation where
  messageName _ = Data.Text.pack "scip.SymbolInformation"
  packedMessageDescriptor _
    = "\n\
      \\DC1SymbolInformation\DC2\SYN\n\
      \\ACKsymbol\CAN\SOH \SOH(\tR\ACKsymbol\DC2$\n\
      \\rdocumentation\CAN\ETX \ETX(\tR\rdocumentation\DC28\n\
      \\rrelationships\CAN\EOT \ETX(\v2\DC2.scip.RelationshipR\rrelationships"
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
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, symbol__field_descriptor),
           (Data.ProtoLens.Tag 3, documentation__field_descriptor),
           (Data.ProtoLens.Tag 4, relationships__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SymbolInformation'_unknownFields
        (\ x__ y__ -> x__ {_SymbolInformation'_unknownFields = y__})
  defMessage
    = SymbolInformation'_constructor
        {_SymbolInformation'symbol = Data.ProtoLens.fieldDefault,
         _SymbolInformation'documentation = Data.Vector.Generic.empty,
         _SymbolInformation'relationships = Data.Vector.Generic.empty,
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
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
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
                      (_SymbolInformation'relationships x__) ())))
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
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded SymbolRole where
  minBound = UnspecifiedSymbolRole
  maxBound = Test
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
  fromEnum (SymbolRole'Unrecognized (SymbolRole'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ Test
    = Prelude.error
        "SymbolRole.succ: bad argument Test. This value would be out of bounds."
  succ UnspecifiedSymbolRole = Definition
  succ Definition = Import
  succ Import = WriteAccess
  succ WriteAccess = ReadAccess
  succ ReadAccess = Generated
  succ Generated = Test
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
    IdentifierKeyword |
    IdentifierOperator |
    Identifier |
    IdentifierBuiltin |
    IdentifierNull |
    IdentifierConstant |
    IdentifierMutableGlobal |
    IdentifierParameter |
    IdentifierLocal |
    IdentifierShadowed |
    IdentifierModule |
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
  maybeToEnum 4 = Prelude.Just IdentifierKeyword
  maybeToEnum 5 = Prelude.Just IdentifierOperator
  maybeToEnum 6 = Prelude.Just Identifier
  maybeToEnum 7 = Prelude.Just IdentifierBuiltin
  maybeToEnum 8 = Prelude.Just IdentifierNull
  maybeToEnum 9 = Prelude.Just IdentifierConstant
  maybeToEnum 10 = Prelude.Just IdentifierMutableGlobal
  maybeToEnum 11 = Prelude.Just IdentifierParameter
  maybeToEnum 12 = Prelude.Just IdentifierLocal
  maybeToEnum 13 = Prelude.Just IdentifierShadowed
  maybeToEnum 14 = Prelude.Just IdentifierModule
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
  showEnum IdentifierKeyword = "IdentifierKeyword"
  showEnum IdentifierOperator = "IdentifierOperator"
  showEnum Identifier = "Identifier"
  showEnum IdentifierBuiltin = "IdentifierBuiltin"
  showEnum IdentifierNull = "IdentifierNull"
  showEnum IdentifierConstant = "IdentifierConstant"
  showEnum IdentifierMutableGlobal = "IdentifierMutableGlobal"
  showEnum IdentifierParameter = "IdentifierParameter"
  showEnum IdentifierLocal = "IdentifierLocal"
  showEnum IdentifierShadowed = "IdentifierShadowed"
  showEnum IdentifierModule = "IdentifierModule"
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
  fromEnum IdentifierKeyword = 4
  fromEnum IdentifierOperator = 5
  fromEnum Identifier = 6
  fromEnum IdentifierBuiltin = 7
  fromEnum IdentifierNull = 8
  fromEnum IdentifierConstant = 9
  fromEnum IdentifierMutableGlobal = 10
  fromEnum IdentifierParameter = 11
  fromEnum IdentifierLocal = 12
  fromEnum IdentifierShadowed = 13
  fromEnum IdentifierModule = 14
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
  succ PunctuationBracket = IdentifierKeyword
  succ IdentifierKeyword = IdentifierOperator
  succ IdentifierOperator = Identifier
  succ Identifier = IdentifierBuiltin
  succ IdentifierBuiltin = IdentifierNull
  succ IdentifierNull = IdentifierConstant
  succ IdentifierConstant = IdentifierMutableGlobal
  succ IdentifierMutableGlobal = IdentifierParameter
  succ IdentifierParameter = IdentifierLocal
  succ IdentifierLocal = IdentifierShadowed
  succ IdentifierShadowed = IdentifierModule
  succ IdentifierModule = IdentifierFunction
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
  pred IdentifierKeyword = PunctuationBracket
  pred IdentifierOperator = IdentifierKeyword
  pred Identifier = IdentifierOperator
  pred IdentifierBuiltin = Identifier
  pred IdentifierNull = IdentifierBuiltin
  pred IdentifierConstant = IdentifierNull
  pred IdentifierMutableGlobal = IdentifierConstant
  pred IdentifierParameter = IdentifierMutableGlobal
  pred IdentifierLocal = IdentifierParameter
  pred IdentifierShadowed = IdentifierLocal
  pred IdentifierModule = IdentifierShadowed
  pred IdentifierFunction = IdentifierModule
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
    \\targuments\CAN\ETX \ETX(\tR\targuments\"\150\SOH\n\
    \\bDocument\DC2#\n\
    \\rrelative_path\CAN\SOH \SOH(\tR\frelativePath\DC22\n\
    \\voccurrences\CAN\STX \ETX(\v2\DLE.scip.OccurrenceR\voccurrences\DC21\n\
    \\asymbols\CAN\ETX \ETX(\v2\ETB.scip.SymbolInformationR\asymbols\"}\n\
    \\ACKSymbol\DC2\SYN\n\
    \\ACKscheme\CAN\SOH \SOH(\tR\ACKscheme\DC2'\n\
    \\apackage\CAN\STX \SOH(\v2\r.scip.PackageR\apackage\DC22\n\
    \\vdescriptors\CAN\ETX \ETX(\v2\DLE.scip.DescriptorR\vdescriptors\"Q\n\
    \\aPackage\DC2\CAN\n\
    \\amanager\CAN\SOH \SOH(\tR\amanager\DC2\DC2\n\
    \\EOTname\CAN\STX \SOH(\tR\EOTname\DC2\CAN\n\
    \\aversion\CAN\ETX \SOH(\tR\aversion\"\253\SOH\n\
    \\n\
    \Descriptor\DC2\DC2\n\
    \\EOTname\CAN\SOH \SOH(\tR\EOTname\DC2$\n\
    \\rdisambiguator\CAN\STX \SOH(\tR\rdisambiguator\DC2/\n\
    \\ACKsuffix\CAN\ETX \SOH(\SO2\ETB.scip.Descriptor.SuffixR\ACKsuffix\"\131\SOH\n\
    \\ACKSuffix\DC2\NAK\n\
    \\DC1UnspecifiedSuffix\DLE\NUL\DC2\v\n\
    \\aPackage\DLE\SOH\DC2\b\n\
    \\EOTType\DLE\STX\DC2\b\n\
    \\EOTTerm\DLE\ETX\DC2\n\
    \\n\
    \\ACKMethod\DLE\EOT\DC2\DC1\n\
    \\rTypeParameter\DLE\ENQ\DC2\r\n\
    \\tParameter\DLE\ACK\DC2\b\n\
    \\EOTMeta\DLE\a\DC2\t\n\
    \\ENQLocal\DLE\b\"\139\SOH\n\
    \\DC1SymbolInformation\DC2\SYN\n\
    \\ACKsymbol\CAN\SOH \SOH(\tR\ACKsymbol\DC2$\n\
    \\rdocumentation\CAN\ETX \ETX(\tR\rdocumentation\DC28\n\
    \\rrelationships\CAN\EOT \ETX(\v2\DC2.scip.RelationshipR\rrelationships\"\164\SOH\n\
    \\fRelationship\DC2\SYN\n\
    \\ACKsymbol\CAN\SOH \SOH(\tR\ACKsymbol\DC2!\n\
    \\fis_reference\CAN\STX \SOH(\bR\visReference\DC2+\n\
    \\DC1is_implementation\CAN\ETX \SOH(\bR\DLEisImplementation\DC2,\n\
    \\DC2is_type_definition\CAN\EOT \SOH(\bR\DLEisTypeDefinition\"\251\SOH\n\
    \\n\
    \Occurrence\DC2\DC4\n\
    \\ENQrange\CAN\SOH \ETX(\ENQR\ENQrange\DC2\SYN\n\
    \\ACKsymbol\CAN\STX \SOH(\tR\ACKsymbol\DC2!\n\
    \\fsymbol_roles\CAN\ETX \SOH(\ENQR\vsymbolRoles\DC25\n\
    \\SYNoverride_documentation\CAN\EOT \ETX(\tR\NAKoverrideDocumentation\DC21\n\
    \\vsyntax_kind\CAN\ENQ \SOH(\SO2\DLE.scip.SyntaxKindR\n\
    \syntaxKind\DC22\n\
    \\vdiagnostics\CAN\ACK \ETX(\v2\DLE.scip.DiagnosticR\vdiagnostics\"\167\SOH\n\
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
    \\ENQUTF16\DLE\STX*}\n\
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
    \\EOTTest\DLE *\184\ACK\n\
    \\n\
    \SyntaxKind\DC2\EM\n\
    \\NAKUnspecifiedSyntaxKind\DLE\NUL\DC2\v\n\
    \\aComment\DLE\SOH\DC2\CAN\n\
    \\DC4PunctuationDelimiter\DLE\STX\DC2\SYN\n\
    \\DC2PunctuationBracket\DLE\ETX\DC2\NAK\n\
    \\DC1IdentifierKeyword\DLE\EOT\DC2\SYN\n\
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
    \\DC2IdentifierShadowed\DLE\r\DC2\DC4\n\
    \\DLEIdentifierModule\DLE\SO\DC2\SYN\n\
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
    \\fTagDelimiter\DLE$*V\n\
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
    \Deprecated\DLE\STXB/Z-github.com/sourcegraph/scip/bindings/go/scip/J\185\DEL\n\
    \\a\DC2\ENQ\n\
    \\NUL\242\STX\SOH\n\
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
    \\209\ETX\n\
    \\STX\EOT\NUL\DC2\EOT\SYN\NUL!\SOH\SUB\196\ETX Index represents a complete SCIP index for a workspace this is rooted at a\n\
    \ single directory. An Index message payload can have a large memory footprint\n\
    \ and it's therefore recommended to emit and consume an Index payload one field\n\
    \ value at a time.  To permit streaming consumption of an Index payload, the\n\
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
    \\248\STX\n\
    \\EOT\EOT\NUL\STX\STX\DC2\ETX \STX2\SUB\234\STX (optional) Symbols that are referenced from this index but are defined in\n\
    \ an external package (a separate `Index` message).  Leave this field empty\n\
    \ if you assume the external package will get indexed separately. If the\n\
    \ external package won't get indexed for some reason then you can use this\n\
    \ field to provide hover documentation for those external symbols.\n\
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
    \\STX\EOT\SOH\DC2\EOT#\NUL/\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX#\b\DLE\n\
    \N\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX%\STX\RS\SUBA Which version of this protocol was used to generate this index?\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ACK\DC2\ETX%\STX\DC1\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX%\DC2\EM\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX%\FS\GS\n\
    \C\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETX'\STX\EM\SUB6 Information about the tool that produced this index.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ACK\DC2\ETX'\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETX'\v\DC4\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETX'\ETB\CAN\n\
    \\162\SOH\n\
    \\EOT\EOT\SOH\STX\STX\DC2\ETX+\STX\SUB\SUB\148\SOH URI-encoded absolute path to the root directory of this index. All\n\
    \ documents in this index must appear in a subdirectory of this root\n\
    \ directory.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ENQ\DC2\ETX+\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\SOH\DC2\ETX+\t\NAK\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ETX\DC2\ETX+\CAN\EM\n\
    \l\n\
    \\EOT\EOT\SOH\STX\ETX\DC2\ETX.\STX*\SUB_ Text encoding of the source files on disk that are referenced from\n\
    \ `Document.relative_path`.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\ETX\ACK\DC2\ETX.\STX\SO\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\ETX\SOH\DC2\ETX.\SI%\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\ETX\ETX\DC2\ETX.()\n\
    \\n\
    \\n\
    \\STX\ENQ\NUL\DC2\EOT1\NUL3\SOH\n\
    \\n\
    \\n\
    \\ETX\ENQ\NUL\SOH\DC2\ETX1\ENQ\DC4\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\NUL\DC2\ETX2\STX!\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\NUL\SOH\DC2\ETX2\STX\FS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\NUL\STX\DC2\ETX2\US \n\
    \\n\
    \\n\
    \\STX\ENQ\SOH\DC2\EOT5\NUL9\SOH\n\
    \\n\
    \\n\
    \\ETX\ENQ\SOH\SOH\DC2\ETX5\ENQ\DC1\n\
    \\v\n\
    \\EOT\ENQ\SOH\STX\NUL\DC2\ETX6\STX\RS\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\NUL\SOH\DC2\ETX6\STX\EM\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\NUL\STX\DC2\ETX6\FS\GS\n\
    \\v\n\
    \\EOT\ENQ\SOH\STX\SOH\DC2\ETX7\STX\v\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\SOH\SOH\DC2\ETX7\STX\ACK\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\SOH\STX\DC2\ETX7\t\n\
    \\n\
    \\v\n\
    \\EOT\ENQ\SOH\STX\STX\DC2\ETX8\STX\f\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\STX\SOH\DC2\ETX8\STX\a\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\STX\STX\DC2\ETX8\n\
    \\v\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT;\NULB\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX;\b\DLE\n\
    \<\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX=\STX\DC2\SUB/ Name of the indexer that produced this index.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETX=\STX\b\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX=\t\r\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX=\DLE\DC1\n\
    \?\n\
    \\EOT\EOT\STX\STX\SOH\DC2\ETX?\STX\NAK\SUB2 Version of the indexer that produced this index.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ENQ\DC2\ETX?\STX\b\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\ETX?\t\DLE\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\ETX?\DC3\DC4\n\
    \L\n\
    \\EOT\EOT\STX\STX\STX\DC2\ETXA\STX \SUB? Command-line arguments that were used to invoke this indexer.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\EOT\DC2\ETXA\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\ENQ\DC2\ETXA\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\SOH\DC2\ETXA\DC2\ESC\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\ETX\DC2\ETXA\RS\US\n\
    \H\n\
    \\STX\EOT\ETX\DC2\EOTE\NULN\SOH\SUB< Document defines the metadata about a source file on disk.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETXE\b\DLE\n\
    \\205\SOH\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETXI\STX\ESC\SUB\191\SOH (Required) Path to the text document relative to the directory supplied in\n\
    \ the associated `Metadata.project_root`. Not URI-encoded. This value should\n\
    \ not begin with a directory separator.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ENQ\DC2\ETXI\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETXI\t\SYN\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETXI\EM\SUB\n\
    \4\n\
    \\EOT\EOT\ETX\STX\SOH\DC2\ETXK\STX&\SUB' Occurrences that appear in this file.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\EOT\DC2\ETXK\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ACK\DC2\ETXK\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\SOH\DC2\ETXK\SYN!\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ETX\DC2\ETXK$%\n\
    \=\n\
    \\EOT\EOT\ETX\STX\STX\DC2\ETXM\STX)\SUB0 Symbols that are defined within this document.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\EOT\DC2\ETXM\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\ACK\DC2\ETXM\v\FS\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\SOH\DC2\ETXM\GS$\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\STX\ETX\DC2\ETXM'(\n\
    \\231\f\n\
    \\STX\EOT\EOT\DC2\EOTm\NULq\SOH\SUB\218\f Symbol is similar to a URI, it identifies a class, method, or a local\n\
    \ variable. `SymbolInformation` contains rich metadata about symbols such as\n\
    \ the docstring.\n\
    \\n\
    \ Symbol has a standardized string representation, which can be used\n\
    \ interchangeably with `Symbol`. The syntax for Symbol is the following:\n\
    \ ```\n\
    \   <symbol>               ::= <scheme> ' ' <package> ' ' { <descriptor> } | 'local ' <local-id>\n\
    \   <package>              ::= <manager> ' ' <package-name> ' ' <version>\n\
    \   <scheme>               ::= any UTF-8, escape spaces with double space.\n\
    \   <manager>              ::= same as above, use the placeholder '.' to indicate an empty value\n\
    \   <package-name>         ::= same as above\n\
    \   <version>              ::= same as above\n\
    \   <descriptor>           ::= <package> | <type> | <term> | <method> | <type-parameter> | <parameter> | <meta>\n\
    \   <package>              ::= <name> '/'\n\
    \   <type>                 ::= <name> '#'\n\
    \   <term>                 ::= <name> '.'\n\
    \   <meta>                 ::= <name> ':'\n\
    \   <method>               ::= <name> '(' <method-disambiguator> ').'\n\
    \   <type-parameter>       ::= '[' <name> ']'\n\
    \   <parameter>            ::= '(' <name> ')'\n\
    \   <name>                 ::= <identifier>\n\
    \   <method-disambiguator> ::= <simple-identifier>\n\
    \   <identifier>           ::= <simple-identifier> | <escaped-identifier>\n\
    \   <simple-identifier>    ::= { <identifier-character> }\n\
    \   <identifier-character> ::= '_' | '+' | '-' | '$' | ASCII letter or digit\n\
    \   <escaped-identifier>   ::= '`' { <escaped-character> } '`'\n\
    \   <escaped-characters>   ::= any UTF-8 character, escape backticks with double backtick.\n\
    \ ```\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETXm\b\SO\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETXn\STX\DC4\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ENQ\DC2\ETXn\STX\b\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETXn\t\SI\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETXn\DC2\DC3\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\SOH\DC2\ETXo\STX\SYN\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ACK\DC2\ETXo\STX\t\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\SOH\DC2\ETXo\n\
    \\DC1\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ETX\DC2\ETXo\DC4\NAK\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\STX\DC2\ETXp\STX&\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\EOT\DC2\ETXp\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\ACK\DC2\ETXp\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\SOH\DC2\ETXp\SYN!\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\ETX\DC2\ETXp$%\n\
    \\n\
    \\n\
    \\STX\EOT\ENQ\DC2\EOTs\NULw\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETXs\b\SI\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\NUL\DC2\ETXt\STX\NAK\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ENQ\DC2\ETXt\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\SOH\DC2\ETXt\t\DLE\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ETX\DC2\ETXt\DC3\DC4\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\SOH\DC2\ETXu\STX\DC2\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\ENQ\DC2\ETXu\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\SOH\DC2\ETXu\t\r\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\SOH\ETX\DC2\ETXu\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\STX\DC2\ETXv\STX\NAK\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\STX\ENQ\DC2\ETXv\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\STX\SOH\DC2\ETXv\t\DLE\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\STX\ETX\DC2\ETXv\DC3\DC4\n\
    \\v\n\
    \\STX\EOT\ACK\DC2\ENQy\NUL\137\SOH\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ACK\SOH\DC2\ETXy\b\DC2\n\
    \\r\n\
    \\EOT\EOT\ACK\EOT\NUL\DC2\ENQz\STX\133\SOH\ETX\n\
    \\f\n\
    \\ENQ\EOT\ACK\EOT\NUL\SOH\DC2\ETXz\a\r\n\
    \\r\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\NUL\DC2\ETX{\EOT\SUB\n\
    \\SO\n\
    \\a\EOT\ACK\EOT\NUL\STX\NUL\SOH\DC2\ETX{\EOT\NAK\n\
    \\SO\n\
    \\a\EOT\ACK\EOT\NUL\STX\NUL\STX\DC2\ETX{\CAN\EM\n\
    \\r\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\SOH\DC2\ETX|\EOT\DLE\n\
    \\SO\n\
    \\a\EOT\ACK\EOT\NUL\STX\SOH\SOH\DC2\ETX|\EOT\v\n\
    \\SO\n\
    \\a\EOT\ACK\EOT\NUL\STX\SOH\STX\DC2\ETX|\SO\SI\n\
    \\r\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\STX\DC2\ETX}\EOT\r\n\
    \\SO\n\
    \\a\EOT\ACK\EOT\NUL\STX\STX\SOH\DC2\ETX}\EOT\b\n\
    \\SO\n\
    \\a\EOT\ACK\EOT\NUL\STX\STX\STX\DC2\ETX}\v\f\n\
    \\r\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\ETX\DC2\ETX~\EOT\r\n\
    \\SO\n\
    \\a\EOT\ACK\EOT\NUL\STX\ETX\SOH\DC2\ETX~\EOT\b\n\
    \\SO\n\
    \\a\EOT\ACK\EOT\NUL\STX\ETX\STX\DC2\ETX~\v\f\n\
    \\r\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\EOT\DC2\ETX\DEL\EOT\SI\n\
    \\SO\n\
    \\a\EOT\ACK\EOT\NUL\STX\EOT\SOH\DC2\ETX\DEL\EOT\n\
    \\n\
    \\SO\n\
    \\a\EOT\ACK\EOT\NUL\STX\EOT\STX\DC2\ETX\DEL\r\SO\n\
    \\SO\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\ENQ\DC2\EOT\128\SOH\EOT\SYN\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\ENQ\SOH\DC2\EOT\128\SOH\EOT\DC1\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\ENQ\STX\DC2\EOT\128\SOH\DC4\NAK\n\
    \\SO\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\ACK\DC2\EOT\129\SOH\EOT\DC2\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\ACK\SOH\DC2\EOT\129\SOH\EOT\r\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\ACK\STX\DC2\EOT\129\SOH\DLE\DC1\n\
    \.\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\a\DC2\EOT\131\SOH\EOT\r\SUB\RS Can be used for any purpose.\n\
    \\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\a\SOH\DC2\EOT\131\SOH\EOT\b\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\a\STX\DC2\EOT\131\SOH\v\f\n\
    \\SO\n\
    \\ACK\EOT\ACK\EOT\NUL\STX\b\DC2\EOT\132\SOH\EOT\SO\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\b\SOH\DC2\EOT\132\SOH\EOT\t\n\
    \\SI\n\
    \\a\EOT\ACK\EOT\NUL\STX\b\STX\DC2\EOT\132\SOH\f\r\n\
    \\f\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\EOT\134\SOH\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\NUL\ENQ\DC2\EOT\134\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\EOT\134\SOH\t\r\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\EOT\134\SOH\DLE\DC1\n\
    \\f\n\
    \\EOT\EOT\ACK\STX\SOH\DC2\EOT\135\SOH\STX\ESC\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\SOH\ENQ\DC2\EOT\135\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\SOH\SOH\DC2\EOT\135\SOH\t\SYN\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\SOH\ETX\DC2\EOT\135\SOH\EM\SUB\n\
    \\f\n\
    \\EOT\EOT\ACK\STX\STX\DC2\EOT\136\SOH\STX\DC4\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\STX\ACK\DC2\EOT\136\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\STX\SOH\DC2\EOT\136\SOH\t\SI\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\STX\ETX\DC2\EOT\136\SOH\DC2\DC3\n\
    \\131\SOH\n\
    \\STX\EOT\a\DC2\ACK\141\SOH\NUL\152\SOH\SOH\SUBu SymbolInformation defines metadata about a symbol, such as the symbol's\n\
    \ docstring or what package it's defined it.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\a\SOH\DC2\EOT\141\SOH\b\EM\n\
    \\160\SOH\n\
    \\EOT\EOT\a\STX\NUL\DC2\EOT\144\SOH\STX\DC4\SUB\145\SOH Identifier of this symbol, which can be referenced from `Occurence.symbol`.\n\
    \ The string must be formatted according to the grammar in `Symbol`.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\NUL\ENQ\DC2\EOT\144\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\a\STX\NUL\SOH\DC2\EOT\144\SOH\t\SI\n\
    \\r\n\
    \\ENQ\EOT\a\STX\NUL\ETX\DC2\EOT\144\SOH\DC2\DC3\n\
    \\182\STX\n\
    \\EOT\EOT\a\STX\SOH\DC2\EOT\149\SOH\STX$\SUB\167\STX (optional, but strongly recommended) The markdown-formatted documentation\n\
    \ for this symbol. This field is repeated to allow different kinds of\n\
    \ documentation.  For example, it's nice to include both the signature of a\n\
    \ method (parameters and return type) along with the accompanying docstring.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\EOT\DC2\EOT\149\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\ENQ\DC2\EOT\149\SOH\v\DC1\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\SOH\DC2\EOT\149\SOH\DC2\US\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\ETX\DC2\EOT\149\SOH\"#\n\
    \^\n\
    \\EOT\EOT\a\STX\STX\DC2\EOT\151\SOH\STX*\SUBP (optional) Relationships to other symbols (e.g., implements, type definition).\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\STX\EOT\DC2\EOT\151\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\STX\ACK\DC2\EOT\151\SOH\v\ETB\n\
    \\r\n\
    \\ENQ\EOT\a\STX\STX\SOH\DC2\EOT\151\SOH\CAN%\n\
    \\r\n\
    \\ENQ\EOT\a\STX\STX\ETX\DC2\EOT\151\SOH()\n\
    \\f\n\
    \\STX\EOT\b\DC2\ACK\154\SOH\NUL\192\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\b\SOH\DC2\EOT\154\SOH\b\DC4\n\
    \\f\n\
    \\EOT\EOT\b\STX\NUL\DC2\EOT\155\SOH\STX\DC4\n\
    \\r\n\
    \\ENQ\EOT\b\STX\NUL\ENQ\DC2\EOT\155\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\b\STX\NUL\SOH\DC2\EOT\155\SOH\t\SI\n\
    \\r\n\
    \\ENQ\EOT\b\STX\NUL\ETX\DC2\EOT\155\SOH\DC2\DC3\n\
    \\143\b\n\
    \\EOT\EOT\b\STX\SOH\DC2\EOT\180\SOH\STX\CAN\SUB\128\b When resolving \"Find references\", this field documents what other symbols\n\
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
    \       ^^^ definition Dog#, implementation_symbols = Animal#\n\
    \   public sound(): string { return \"woof\" }\n\
    \          ^^^^^ definition Dog#sound(), references_symbols = Animal#sound(), implementation_symbols = Animal#sound()\n\
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
    \\ENQ\EOT\b\STX\SOH\ENQ\DC2\EOT\180\SOH\STX\ACK\n\
    \\r\n\
    \\ENQ\EOT\b\STX\SOH\SOH\DC2\EOT\180\SOH\a\DC3\n\
    \\r\n\
    \\ENQ\EOT\b\STX\SOH\ETX\DC2\EOT\180\SOH\SYN\ETB\n\
    \\186\EOT\n\
    \\EOT\EOT\b\STX\STX\DC2\EOT\189\SOH\STX\GS\SUB\171\EOT Similar to `references_symbols` but for \"Go to implementation\".\n\
    \ It's common for the `implementation_symbols` and `references_symbols` fields\n\
    \ have the same values but that's not always the case.\n\
    \ In the TypeScript example above, observe that `implementation_symbols` has\n\
    \ the value `\"Animal#\"` for the \"Dog#\" symbol while `references_symbols` is\n\
    \ empty. When requesting \"Find references\" on the \"Animal#\" symbol we don't\n\
    \ want to include references to \"Dog#\" even if \"Go to implementation\" on the\n\
    \ \"Animal#\" symbol should navigate to the \"Dog#\" symbol.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\b\STX\STX\ENQ\DC2\EOT\189\SOH\STX\ACK\n\
    \\r\n\
    \\ENQ\EOT\b\STX\STX\SOH\DC2\EOT\189\SOH\a\CAN\n\
    \\r\n\
    \\ENQ\EOT\b\STX\STX\ETX\DC2\EOT\189\SOH\ESC\FS\n\
    \P\n\
    \\EOT\EOT\b\STX\ETX\DC2\EOT\191\SOH\STX\RS\SUBB Similar to `references_symbols` but for \"Go to type definition\".\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\b\STX\ETX\ENQ\DC2\EOT\191\SOH\STX\ACK\n\
    \\r\n\
    \\ENQ\EOT\b\STX\ETX\SOH\DC2\EOT\191\SOH\a\EM\n\
    \\r\n\
    \\ENQ\EOT\b\STX\ETX\ETX\DC2\EOT\191\SOH\FS\GS\n\
    \\138\ETX\n\
    \\STX\ENQ\STX\DC2\ACK\199\SOH\NUL\213\SOH\SOH\SUB\251\STX SymbolRole declares what \"role\" a symbol has in an occurrence.  A role is\n\
    \ encoded as a bitmask where each bit represents a different role. For example,\n\
    \ to determine if the `Import` role is set test whether the second bit of the\n\
    \ enum value is defined. In psuedo-code, this can be implemented with the\n\
    \ logic: `const isImportRole = (role.value & SymbolRole.Import.value) > 0`.\n\
    \\n\
    \\v\n\
    \\ETX\ENQ\STX\SOH\DC2\EOT\199\SOH\ENQ\SI\n\
    \\f\n\
    \\EOT\ENQ\STX\STX\NUL\DC2\EOT\200\SOH\STX\FS\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\NUL\SOH\DC2\EOT\200\SOH\STX\ETB\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\NUL\STX\DC2\EOT\200\SOH\SUB\ESC\n\
    \T\n\
    \\EOT\ENQ\STX\STX\SOH\DC2\EOT\202\SOH\STX\DC3\SUBF Is the symbol defined here? If not, then this is a symbol reference.\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\SOH\SOH\DC2\EOT\202\SOH\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\SOH\STX\DC2\EOT\202\SOH\SI\DC2\n\
    \,\n\
    \\EOT\ENQ\STX\STX\STX\DC2\EOT\204\SOH\STX\SI\SUB\RS Is the symbol imported here?\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\STX\SOH\DC2\EOT\204\SOH\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\STX\STX\DC2\EOT\204\SOH\v\SO\n\
    \+\n\
    \\EOT\ENQ\STX\STX\ETX\DC2\EOT\206\SOH\STX\DC4\SUB\GS Is the symbol written here?\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\ETX\SOH\DC2\EOT\206\SOH\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\ETX\STX\DC2\EOT\206\SOH\DLE\DC3\n\
    \(\n\
    \\EOT\ENQ\STX\STX\EOT\DC2\EOT\208\SOH\STX\DC3\SUB\SUB Is the symbol read here?\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\EOT\SOH\DC2\EOT\208\SOH\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\EOT\STX\DC2\EOT\208\SOH\SI\DC2\n\
    \0\n\
    \\EOT\ENQ\STX\STX\ENQ\DC2\EOT\210\SOH\STX\DC3\SUB\" Is the symbol in generated code?\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\ENQ\SOH\DC2\EOT\210\SOH\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\ENQ\STX\DC2\EOT\210\SOH\SO\DC2\n\
    \+\n\
    \\EOT\ENQ\STX\STX\ACK\DC2\EOT\212\SOH\STX\SO\SUB\GS Is the symbol in test code?\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\ACK\SOH\DC2\EOT\212\SOH\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\STX\STX\ACK\STX\DC2\EOT\212\SOH\t\r\n\
    \\f\n\
    \\STX\ENQ\ETX\DC2\ACK\215\SOH\NUL\173\STX\SOH\n\
    \\v\n\
    \\ETX\ENQ\ETX\SOH\DC2\EOT\215\SOH\ENQ\SI\n\
    \\f\n\
    \\EOT\ENQ\ETX\STX\NUL\DC2\EOT\216\SOH\STX\FS\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\NUL\SOH\DC2\EOT\216\SOH\STX\ETB\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\NUL\STX\DC2\EOT\216\SOH\SUB\ESC\n\
    \;\n\
    \\EOT\ENQ\ETX\STX\SOH\DC2\EOT\219\SOH\STX\SO\SUB- Comment, including comment markers and text\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\SOH\SOH\DC2\EOT\219\SOH\STX\t\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\SOH\STX\DC2\EOT\219\SOH\f\r\n\
    \\ESC\n\
    \\EOT\ENQ\ETX\STX\STX\DC2\EOT\222\SOH\STX\ESC\SUB\r `;` `.` `,`\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\STX\SOH\DC2\EOT\222\SOH\STX\SYN\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\STX\STX\DC2\EOT\222\SOH\EM\SUB\n\
    \2\n\
    \\EOT\ENQ\ETX\STX\ETX\DC2\EOT\224\SOH\STX\EM\SUB$ (), {}, [] when used syntactically\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\ETX\SOH\DC2\EOT\224\SOH\STX\DC4\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\ETX\STX\DC2\EOT\224\SOH\ETB\CAN\n\
    \5\n\
    \\EOT\ENQ\ETX\STX\EOT\DC2\EOT\227\SOH\STX\CAN\SUB' `if`, `else`, `return`, `class`, etc.\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\EOT\SOH\DC2\EOT\227\SOH\STX\DC3\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\EOT\STX\DC2\EOT\227\SOH\SYN\ETB\n\
    \\RS\n\
    \\EOT\ENQ\ETX\STX\ENQ\DC2\EOT\230\SOH\STX\EM\SUB\DLE `+`, `*`, etc.\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\ENQ\SOH\DC2\EOT\230\SOH\STX\DC4\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\ENQ\STX\DC2\EOT\230\SOH\ETB\CAN\n\
    \X\n\
    \\EOT\ENQ\ETX\STX\ACK\DC2\EOT\233\SOH\STX\DC1\SUBJ non-specific catch-all for any identifier not better described elsewhere\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\ACK\SOH\DC2\EOT\233\SOH\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\ACK\STX\DC2\EOT\233\SOH\SI\DLE\n\
    \N\n\
    \\EOT\ENQ\ETX\STX\a\DC2\EOT\235\SOH\STX\CAN\SUB@ Identifiers builtin to the language: `min`, `print` in Python.\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\a\SOH\DC2\EOT\235\SOH\STX\DC3\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\a\STX\DC2\EOT\235\SOH\SYN\ETB\n\
    \[\n\
    \\EOT\ENQ\ETX\STX\b\DC2\EOT\237\SOH\STX\NAK\SUBM Identifiers representing `null`-like values: `None` in Python, `nil` in Go.\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\b\SOH\DC2\EOT\237\SOH\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\b\STX\DC2\EOT\237\SOH\DC3\DC4\n\
    \.\n\
    \\EOT\ENQ\ETX\STX\t\DC2\EOT\239\SOH\STX\EM\SUB  `xyz` in `const xyz = \"hello\"`\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\t\SOH\DC2\EOT\239\SOH\STX\DC4\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\t\STX\DC2\EOT\239\SOH\ETB\CAN\n\
    \'\n\
    \\EOT\ENQ\ETX\STX\n\
    \\DC2\EOT\241\SOH\STX\US\SUB\EM `var X = \"hello\"` in Go\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\n\
    \\SOH\DC2\EOT\241\SOH\STX\EM\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\n\
    \\STX\DC2\EOT\241\SOH\FS\RS\n\
    \8\n\
    \\EOT\ENQ\ETX\STX\v\DC2\EOT\243\SOH\STX\ESC\SUB* both parameter definition and references\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\v\SOH\DC2\EOT\243\SOH\STX\NAK\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\v\STX\DC2\EOT\243\SOH\CAN\SUB\n\
    \X\n\
    \\EOT\ENQ\ETX\STX\f\DC2\EOT\245\SOH\STX\ETB\SUBJ identifiers for variable definitions and references within a local scope\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\f\SOH\DC2\EOT\245\SOH\STX\DC1\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\f\STX\DC2\EOT\245\SOH\DC4\SYN\n\
    \T\n\
    \\EOT\ENQ\ETX\STX\r\DC2\EOT\247\SOH\STX\SUB\SUBF Used when identifier shadowes some other identifier within the scope\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\r\SOH\DC2\EOT\247\SOH\STX\DC4\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\r\STX\DC2\EOT\247\SOH\ETB\EM\n\
    \\RS\n\
    \\EOT\ENQ\ETX\STX\SO\DC2\EOT\249\SOH\STX\CAN\SUB\DLE `package main`\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\SO\SOH\DC2\EOT\249\SOH\STX\DC2\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\SO\STX\DC2\EOT\249\SOH\NAK\ETB\n\
    \'\n\
    \\EOT\ENQ\ETX\STX\SI\DC2\EOT\252\SOH\STX\SUB\SUB\EM Function call/reference\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\SI\SOH\DC2\EOT\252\SOH\STX\DC4\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\SI\STX\DC2\EOT\252\SOH\ETB\EM\n\
    \(\n\
    \\EOT\ENQ\ETX\STX\DLE\DC2\EOT\254\SOH\STX$\SUB\SUB Function definition only\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\DLE\SOH\DC2\EOT\254\SOH\STX\RS\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\DLE\STX\DC2\EOT\254\SOH!#\n\
    \$\n\
    \\EOT\ENQ\ETX\STX\DC1\DC2\EOT\129\STX\STX\ETB\SUB\SYN Macro call/reference\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\DC1\SOH\DC2\EOT\129\STX\STX\DC1\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\DC1\STX\DC2\EOT\129\STX\DC4\SYN\n\
    \%\n\
    \\EOT\ENQ\ETX\STX\DC2\DC2\EOT\131\STX\STX!\SUB\ETB Macro definition only\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\DC2\SOH\DC2\EOT\131\STX\STX\ESC\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\DC2\STX\DC2\EOT\131\STX\RS \n\
    \7\n\
    \\EOT\ENQ\ETX\STX\DC3\DC2\EOT\134\STX\STX\SYN\SUB) non-builtin types, including namespaces\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\DC3\SOH\DC2\EOT\134\STX\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\DC3\STX\DC2\EOT\134\STX\DC3\NAK\n\
    \K\n\
    \\EOT\ENQ\ETX\STX\DC4\DC2\EOT\136\STX\STX\GS\SUB= builtin types only, such as `str` for Python or `int` in Go\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\DC4\SOH\DC2\EOT\136\STX\STX\ETB\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\DC4\STX\DC2\EOT\136\STX\SUB\FS\n\
    \7\n\
    \\EOT\ENQ\ETX\STX\NAK\DC2\EOT\139\STX\STX\ESC\SUB) Python decorators, c-like __attribute__\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\NAK\SOH\DC2\EOT\139\STX\STX\NAK\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\NAK\STX\DC2\EOT\139\STX\CAN\SUB\n\
    \\DC4\n\
    \\EOT\ENQ\ETX\STX\SYN\DC2\EOT\142\STX\STX\DC3\SUB\ACK `\\b`\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\SYN\SOH\DC2\EOT\142\STX\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\SYN\STX\DC2\EOT\142\STX\DLE\DC2\n\
    \\CAN\n\
    \\EOT\ENQ\ETX\STX\ETB\DC2\EOT\144\STX\STX\NAK\SUB\n\
    \ `*`, `+`\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\ETB\SOH\DC2\EOT\144\STX\STX\SI\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\ETB\STX\DC2\EOT\144\STX\DC2\DC4\n\
    \\DC3\n\
    \\EOT\ENQ\ETX\STX\CAN\DC2\EOT\146\STX\STX\NAK\SUB\ENQ `.`\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\CAN\SOH\DC2\EOT\146\STX\STX\SI\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\CAN\STX\DC2\EOT\146\STX\DC2\DC4\n\
    \\"\n\
    \\EOT\ENQ\ETX\STX\EM\DC2\EOT\148\STX\STX\SYN\SUB\DC4 `(`, `)`, `[`, `]`\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\EM\SOH\DC2\EOT\148\STX\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\EM\STX\DC2\EOT\148\STX\DC3\NAK\n\
    \\CAN\n\
    \\EOT\ENQ\ETX\STX\SUB\DC2\EOT\150\STX\STX\DC1\SUB\n\
    \ `|`, `-`\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\SUB\SOH\DC2\EOT\150\STX\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\SUB\STX\DC2\EOT\150\STX\SO\DLE\n\
    \0\n\
    \\EOT\ENQ\ETX\STX\ESC\DC2\EOT\153\STX\STX\NAK\SUB\" Literal strings: \"Hello, world!\"\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\ESC\SOH\DC2\EOT\153\STX\STX\SI\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\ESC\STX\DC2\EOT\153\STX\DC2\DC4\n\
    \-\n\
    \\EOT\ENQ\ETX\STX\FS\DC2\EOT\155\STX\STX\ESC\SUB\US non-regex escapes: \"\\t\", \"\\n\"\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\FS\SOH\DC2\EOT\155\STX\STX\NAK\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\FS\STX\DC2\EOT\155\STX\CAN\SUB\n\
    \_\n\
    \\EOT\ENQ\ETX\STX\GS\DC2\EOT\157\STX\STX\FS\SUBQ datetimes within strings, special words within a string, `{}` in format strings\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\GS\SOH\DC2\EOT\157\STX\STX\SYN\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\GS\STX\DC2\EOT\157\STX\EM\ESC\n\
    \G\n\
    \\EOT\ENQ\ETX\STX\RS\DC2\EOT\159\STX\STX\CAN\SUB9 \"key\" in { \"key\": \"value\" }, useful for example in JSON\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\RS\SOH\DC2\EOT\159\STX\STX\DC2\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\RS\STX\DC2\EOT\159\STX\NAK\ETB\n\
    \V\n\
    \\EOT\ENQ\ETX\STX\US\DC2\EOT\161\STX\STX\CAN\SUBH 'c' or similar, in languages that differentiate strings and characters\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\US\SOH\DC2\EOT\161\STX\STX\DC2\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\US\STX\DC2\EOT\161\STX\NAK\ETB\n\
    \9\n\
    \\EOT\ENQ\ETX\STX \DC2\EOT\163\STX\STX\SYN\SUB+ Literal numbers, both floats and integers\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX \SOH\DC2\EOT\163\STX\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX \STX\DC2\EOT\163\STX\DC3\NAK\n\
    \\US\n\
    \\EOT\ENQ\ETX\STX!\DC2\EOT\165\STX\STX\SYN\SUB\DC1 `true`, `false`\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX!\SOH\DC2\EOT\165\STX\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX!\STX\DC2\EOT\165\STX\DC3\NAK\n\
    \&\n\
    \\EOT\ENQ\ETX\STX\"\DC2\EOT\168\STX\STX\v\SUB\CAN Used for XML-like tags\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\"\SOH\DC2\EOT\168\STX\STX\ENQ\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX\"\STX\DC2\EOT\168\STX\b\n\
    \\n\
    \/\n\
    \\EOT\ENQ\ETX\STX#\DC2\EOT\170\STX\STX\DC4\SUB! Attribute name in XML-like tags\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX#\SOH\DC2\EOT\170\STX\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX#\STX\DC2\EOT\170\STX\DC1\DC3\n\
    \,\n\
    \\EOT\ENQ\ETX\STX$\DC2\EOT\172\STX\STX\DC4\SUB\RS Delimiters for XML-like tags\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX$\SOH\DC2\EOT\172\STX\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\ETX\STX$\STX\DC2\EOT\172\STX\DC1\DC3\n\
    \g\n\
    \\STX\EOT\t\DC2\ACK\177\STX\NUL\213\STX\SOH\SUBY Occurrence associates a source position with a symbol and/or highlighting\n\
    \ information.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\t\SOH\DC2\EOT\177\STX\b\DC2\n\
    \\199\a\n\
    \\EOT\EOT\t\STX\NUL\DC2\EOT\196\STX\STX\ESC\SUB\184\a Source position of this occurrence. Must be exactly three or four\n\
    \ elements:\n\
    \\n\
    \ - Four elements: `[startLine, startCharacter, endLine, endCharacter]`\n\
    \ - Three elements: `[startLine, startCharacter, endCharacter]`. The end line\n\
    \   is inferred to have the same value as the start line.\n\
    \\n\
    \ Line numbers and characters are always 0-based. Make sure to increment the\n\
    \ line/character values before displaying them in an editor-like UI because\n\
    \ editors conventionally use 1-based numbers.\n\
    \\n\
    \ Historical note: the original draft of this schema had a `Range` message\n\
    \ type with `start` and `end` fields of type `Position`, mirroring LSP.\n\
    \ Benchmarks revealed that this encoding was inefficient and that we could\n\
    \ reduce the total payload size of an index by 50% by using `repeated int32`\n\
    \ instead.  The `repeated int32` encoding is admittedly more embarrassing to\n\
    \ work with in some programming languages but we hope the performance\n\
    \ improvements make up for it.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\NUL\EOT\DC2\EOT\196\STX\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\NUL\ENQ\DC2\EOT\196\STX\v\DLE\n\
    \\r\n\
    \\ENQ\EOT\t\STX\NUL\SOH\DC2\EOT\196\STX\DC1\SYN\n\
    \\r\n\
    \\ENQ\EOT\t\STX\NUL\ETX\DC2\EOT\196\STX\EM\SUB\n\
    \\138\SOH\n\
    \\EOT\EOT\t\STX\SOH\DC2\EOT\199\STX\STX\DC4\SUB| (optional) The symbol that appears at this position. See\n\
    \ `SymbolInformation.symbol` for how to format symbols as strings.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\SOH\ENQ\DC2\EOT\199\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\t\STX\SOH\SOH\DC2\EOT\199\STX\t\SI\n\
    \\r\n\
    \\ENQ\EOT\t\STX\SOH\ETX\DC2\EOT\199\STX\DC2\DC3\n\
    \\138\SOH\n\
    \\EOT\EOT\t\STX\STX\DC2\EOT\202\STX\STX\EM\SUB| (optional) Bitmask for what `SymbolRole` apply to this occurrence. See\n\
    \ `SymbolRole` for how to read and write this field.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\STX\ENQ\DC2\EOT\202\STX\STX\a\n\
    \\r\n\
    \\ENQ\EOT\t\STX\STX\SOH\DC2\EOT\202\STX\b\DC4\n\
    \\r\n\
    \\ENQ\EOT\t\STX\STX\ETX\DC2\EOT\202\STX\ETB\CAN\n\
    \\240\STX\n\
    \\EOT\EOT\t\STX\ETX\DC2\EOT\208\STX\STX-\SUB\225\STX (optional) Markdown-formatted documentation for this specific range.  If\n\
    \ empty, the `Symbol.documentation` field is used instead. One example\n\
    \ where this field might be useful is when the symbol represents a generic\n\
    \ function (with abstract type parameters such as `List<T>`) and at this\n\
    \ occurrence we know the exact values (such as `List<String>`).\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ETX\EOT\DC2\EOT\208\STX\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ETX\ENQ\DC2\EOT\208\STX\v\DC1\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ETX\SOH\DC2\EOT\208\STX\DC2(\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ETX\ETX\DC2\EOT\208\STX+,\n\
    \X\n\
    \\EOT\EOT\t\STX\EOT\DC2\EOT\210\STX\STX\GS\SUBJ (optional) What syntax highlighting class should be used for this range?\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\EOT\ACK\DC2\EOT\210\STX\STX\f\n\
    \\r\n\
    \\ENQ\EOT\t\STX\EOT\SOH\DC2\EOT\210\STX\r\CAN\n\
    \\r\n\
    \\ENQ\EOT\t\STX\EOT\ETX\DC2\EOT\210\STX\ESC\FS\n\
    \L\n\
    \\EOT\EOT\t\STX\ENQ\DC2\EOT\212\STX\STX&\SUB> Diagnostics that have been reported for this specific range.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ENQ\EOT\DC2\EOT\212\STX\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ENQ\ACK\DC2\EOT\212\STX\v\NAK\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ENQ\SOH\DC2\EOT\212\STX\SYN!\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ENQ\ETX\DC2\EOT\212\STX$%\n\
    \w\n\
    \\STX\EOT\n\
    \\DC2\ACK\217\STX\NUL\228\STX\SOH\SUBi Represents a diagnostic, such as a compiler error or warning, which should be\n\
    \ reported for a document.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\n\
    \\SOH\DC2\EOT\217\STX\b\DC2\n\
    \W\n\
    \\EOT\EOT\n\
    \\STX\NUL\DC2\EOT\219\STX\STX\CAN\SUBI Should this diagnostic be reported as an error, warning, info, or hint?\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ACK\DC2\EOT\219\STX\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\NUL\SOH\DC2\EOT\219\STX\v\DC3\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ETX\DC2\EOT\219\STX\SYN\ETB\n\
    \R\n\
    \\EOT\EOT\n\
    \\STX\SOH\DC2\EOT\221\STX\STX\DC2\SUBD Code of this diagnostic, which might appear in the user interface.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\SOH\ENQ\DC2\EOT\221\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\SOH\SOH\DC2\EOT\221\STX\t\r\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\SOH\ETX\DC2\EOT\221\STX\DLE\DC1\n\
    \+\n\
    \\EOT\EOT\n\
    \\STX\STX\DC2\EOT\223\STX\STX\NAK\SUB\GS Message of this diagnostic.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\STX\ENQ\DC2\EOT\223\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\STX\SOH\DC2\EOT\223\STX\t\DLE\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\STX\ETX\DC2\EOT\223\STX\DC3\DC4\n\
    \s\n\
    \\EOT\EOT\n\
    \\STX\ETX\DC2\EOT\226\STX\STX\DC4\SUBe Human-readable string describing the source of this diagnostic, e.g.\n\
    \ 'typescript' or 'super lint'.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\ETX\ENQ\DC2\EOT\226\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\ETX\SOH\DC2\EOT\226\STX\t\SI\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\ETX\ETX\DC2\EOT\226\STX\DC2\DC3\n\
    \\f\n\
    \\EOT\EOT\n\
    \\STX\EOT\DC2\EOT\227\STX\STX\"\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\EOT\EOT\DC2\EOT\227\STX\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\EOT\ACK\DC2\EOT\227\STX\v\CAN\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\EOT\SOH\DC2\EOT\227\STX\EM\GS\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\EOT\ETX\DC2\EOT\227\STX !\n\
    \\f\n\
    \\STX\ENQ\EOT\DC2\ACK\230\STX\NUL\236\STX\SOH\n\
    \\v\n\
    \\ETX\ENQ\EOT\SOH\DC2\EOT\230\STX\ENQ\r\n\
    \\f\n\
    \\EOT\ENQ\EOT\STX\NUL\DC2\EOT\231\STX\STX\SUB\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\NUL\SOH\DC2\EOT\231\STX\STX\NAK\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\NUL\STX\DC2\EOT\231\STX\CAN\EM\n\
    \\f\n\
    \\EOT\ENQ\EOT\STX\SOH\DC2\EOT\232\STX\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\SOH\SOH\DC2\EOT\232\STX\STX\a\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\SOH\STX\DC2\EOT\232\STX\n\
    \\v\n\
    \\f\n\
    \\EOT\ENQ\EOT\STX\STX\DC2\EOT\233\STX\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\STX\SOH\DC2\EOT\233\STX\STX\t\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\STX\STX\DC2\EOT\233\STX\f\r\n\
    \\f\n\
    \\EOT\ENQ\EOT\STX\ETX\DC2\EOT\234\STX\STX\DC2\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\ETX\SOH\DC2\EOT\234\STX\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\ETX\STX\DC2\EOT\234\STX\DLE\DC1\n\
    \\f\n\
    \\EOT\ENQ\EOT\STX\EOT\DC2\EOT\235\STX\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\EOT\SOH\DC2\EOT\235\STX\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\EOT\STX\EOT\STX\DC2\EOT\235\STX\t\n\
    \\n\
    \\f\n\
    \\STX\ENQ\ENQ\DC2\ACK\238\STX\NUL\242\STX\SOH\n\
    \\v\n\
    \\ETX\ENQ\ENQ\SOH\DC2\EOT\238\STX\ENQ\DC2\n\
    \\f\n\
    \\EOT\ENQ\ENQ\STX\NUL\DC2\EOT\239\STX\STX\US\n\
    \\r\n\
    \\ENQ\ENQ\ENQ\STX\NUL\SOH\DC2\EOT\239\STX\STX\SUB\n\
    \\r\n\
    \\ENQ\ENQ\ENQ\STX\NUL\STX\DC2\EOT\239\STX\GS\RS\n\
    \\f\n\
    \\EOT\ENQ\ENQ\STX\SOH\DC2\EOT\240\STX\STX\DC2\n\
    \\r\n\
    \\ENQ\ENQ\ENQ\STX\SOH\SOH\DC2\EOT\240\STX\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\ENQ\STX\SOH\STX\DC2\EOT\240\STX\DLE\DC1\n\
    \\f\n\
    \\EOT\ENQ\ENQ\STX\STX\DC2\EOT\241\STX\STX\DC1\n\
    \\r\n\
    \\ENQ\ENQ\ENQ\STX\STX\SOH\DC2\EOT\241\STX\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\ENQ\STX\STX\STX\DC2\EOT\241\STX\SI\DLEb\ACKproto3"