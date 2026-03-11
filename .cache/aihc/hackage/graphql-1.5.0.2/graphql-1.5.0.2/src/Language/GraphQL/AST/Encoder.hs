{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE Safe #-}

-- | This module defines a minifier and a printer for the @GraphQL@ language.
module Language.GraphQL.AST.Encoder
    ( Formatter
    , definition
    , directive
    , document
    , minified
    , operationType
    , pretty
    , type'
    , typeSystemDefinition
    , value
    ) where

import Data.Foldable (fold, Foldable (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Lazy.Text
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import qualified Language.GraphQL.AST.Document as Full
import qualified Language.GraphQL.AST.DirectiveLocation as DirectiveLocation

-- | Instructs the encoder whether the GraphQL document should be minified or
--   pretty printed.
--
--   Use 'pretty' or 'minified' to construct the formatter.
data Formatter
    = Minified
    | Pretty !Word

-- | Constructs a formatter for pretty printing.
pretty :: Formatter
pretty = Pretty 0

-- | Constructs a formatter for minifying.
minified :: Formatter
minified = Minified

-- | Converts a Document' into a string.
document :: Formatter -> Full.Document -> Lazy.Text
document formatter defs
    | Pretty _ <- formatter = Lazy.Text.intercalate "\n" encodeDocument
    | Minified <-formatter = Lazy.Text.snoc (mconcat encodeDocument) '\n'
  where
    encodeDocument = foldr executableDefinition [] defs
    executableDefinition (Full.ExecutableDefinition executableDefinition') acc =
        definition formatter executableDefinition' : acc
    executableDefinition (Full.TypeSystemDefinition typeSystemDefinition' _location) acc =
        typeSystemDefinition formatter typeSystemDefinition' : acc
    executableDefinition (Full.TypeSystemExtension typeSystemExtension' _location) acc =
        typeSystemExtension formatter typeSystemExtension' : acc

directiveLocation :: DirectiveLocation.DirectiveLocation -> Lazy.Text
directiveLocation = Lazy.Text.pack . show

withLineBreak :: Formatter -> Lazy.Text.Text -> Lazy.Text.Text
withLineBreak formatter encodeDefinition
    | Pretty _ <- formatter = Lazy.Text.snoc encodeDefinition '\n'
    | Minified <- formatter = encodeDefinition

typeSystemExtension :: Formatter -> Full.TypeSystemExtension -> Lazy.Text
typeSystemExtension formatter = \case
    Full.SchemaExtension schemaExtension' ->
        schemaExtension formatter schemaExtension'
    Full.TypeExtension typeExtension' -> typeExtension formatter typeExtension'

schemaExtension :: Formatter -> Full.SchemaExtension -> Lazy.Text
schemaExtension formatter = \case
    Full.SchemaOperationExtension operationDirectives operationTypeDefinitions' ->
        withLineBreak formatter
            $ "extend schema "
            <> optempty (directives formatter) operationDirectives
            <> bracesList formatter (operationTypeDefinition formatter) (NonEmpty.toList operationTypeDefinitions')
    Full.SchemaDirectivesExtension operationDirectives -> "extend schema "
        <> optempty (directives formatter) (NonEmpty.toList operationDirectives)

typeExtension :: Formatter -> Full.TypeExtension -> Lazy.Text
typeExtension formatter = \case
    Full.ScalarTypeExtension name' directives'
        -> "extend scalar "
        <> Lazy.Text.fromStrict name'
        <> directives formatter (NonEmpty.toList directives')
    Full.ObjectTypeFieldsDefinitionExtension name' ifaces' directives' fields'
        -> "extend type "
        <> Lazy.Text.fromStrict name'
        <> optempty (" " <>) (implementsInterfaces ifaces')
        <> optempty (directives formatter) directives'
        <> eitherFormat formatter " " ""
        <> bracesList formatter (fieldDefinition nextFormatter) (NonEmpty.toList fields')
    Full.ObjectTypeDirectivesExtension name' ifaces' directives'
        -> "extend type "
        <> Lazy.Text.fromStrict name'
        <> optempty (" " <>) (implementsInterfaces ifaces')
        <> optempty (directives formatter) (NonEmpty.toList directives')
    Full.ObjectTypeImplementsInterfacesExtension name' ifaces'
        -> "extend type "
        <> Lazy.Text.fromStrict name'
        <> optempty (" " <>) (implementsInterfaces ifaces')
    Full.InterfaceTypeFieldsDefinitionExtension name' directives' fields'
        -> "extend interface "
        <> Lazy.Text.fromStrict name'
        <> optempty (directives formatter) directives'
        <> eitherFormat formatter " " ""
        <> bracesList formatter (fieldDefinition nextFormatter) (NonEmpty.toList fields')
    Full.InterfaceTypeDirectivesExtension name' directives'
        -> "extend interface "
        <> Lazy.Text.fromStrict name'
        <> optempty (directives formatter) (NonEmpty.toList directives')
    Full.UnionTypeUnionMemberTypesExtension name' directives' members'
        -> "extend union "
        <> Lazy.Text.fromStrict name'
        <> optempty (directives formatter) directives'
        <> eitherFormat formatter " " ""
        <> unionMemberTypes formatter members'
    Full.UnionTypeDirectivesExtension name' directives'
        -> "extend union "
        <> Lazy.Text.fromStrict name'
        <> optempty (directives formatter) (NonEmpty.toList directives')
    Full.EnumTypeEnumValuesDefinitionExtension name' directives' members'
        -> "extend enum "
        <> Lazy.Text.fromStrict name'
        <> optempty (directives formatter) directives'
        <> eitherFormat formatter " " ""
        <> bracesList formatter (enumValueDefinition formatter) (NonEmpty.toList members')
    Full.EnumTypeDirectivesExtension name' directives'
        -> "extend enum "
        <> Lazy.Text.fromStrict name'
        <> optempty (directives formatter) (NonEmpty.toList directives')
    Full.InputObjectTypeInputFieldsDefinitionExtension name' directives' fields'
        -> "extend input "
        <> Lazy.Text.fromStrict name'
        <> optempty (directives formatter) directives'
        <> eitherFormat formatter " " ""
        <> bracesList formatter (inputValueDefinition nextFormatter) (NonEmpty.toList fields')
    Full.InputObjectTypeDirectivesExtension name' directives'
        -> "extend input "
        <> Lazy.Text.fromStrict name'
        <> optempty (directives formatter) (NonEmpty.toList directives')
  where
    nextFormatter = incrementIndent formatter

-- | Converts a t'Full.TypeSystemDefinition' into a string.
typeSystemDefinition :: Formatter -> Full.TypeSystemDefinition -> Lazy.Text
typeSystemDefinition formatter = \case
    Full.SchemaDefinition operationDirectives operationTypeDefinitions' ->
        withLineBreak formatter
            $ "schema "
            <> optempty (directives formatter) operationDirectives
            <> bracesList formatter (operationTypeDefinition formatter) (NonEmpty.toList operationTypeDefinitions')
    Full.TypeDefinition typeDefinition' -> typeDefinition formatter typeDefinition'
    Full.DirectiveDefinition description' name' arguments' repeatable locations
        -> description formatter description'
        <> "@"
        <> Lazy.Text.fromStrict name'
        <> argumentsDefinition formatter arguments'
        <> (if repeatable then " repeatable" else mempty)
        <> " on"
        <> pipeList formatter (directiveLocation <$> locations)

operationTypeDefinition :: Formatter -> Full.OperationTypeDefinition -> Lazy.Text.Text
operationTypeDefinition formatter (Full.OperationTypeDefinition operationType' namedType')
    = indentLine (incrementIndent formatter)
    <> operationType formatter operationType'
    <> colon formatter
    <> Lazy.Text.fromStrict namedType'

fieldDefinition :: Formatter -> Full.FieldDefinition -> Lazy.Text.Text
fieldDefinition formatter fieldDefinition' =
    let Full.FieldDefinition description' name' arguments' type'' directives' = fieldDefinition'
     in optempty (description formatter) description'
            <> indentLine formatter
            <> Lazy.Text.fromStrict name'
            <> argumentsDefinition formatter arguments'
            <> colon formatter
            <> type' type''
            <> optempty (directives formatter) directives'

argumentsDefinition :: Formatter -> Full.ArgumentsDefinition -> Lazy.Text.Text
argumentsDefinition formatter (Full.ArgumentsDefinition arguments') =
    parensCommas formatter (argumentDefinition formatter) arguments'

argumentDefinition :: Formatter -> Full.InputValueDefinition -> Lazy.Text.Text
argumentDefinition formatter definition' =
    let Full.InputValueDefinition description' name' type'' defaultValue' directives' = definition'
     in optempty (description formatter) description'
            <> Lazy.Text.fromStrict name'
            <> colon formatter
            <> type' type''
            <> maybe mempty (defaultValue formatter . Full.node) defaultValue'
            <> directives formatter directives'

inputValueDefinition :: Formatter -> Full.InputValueDefinition -> Lazy.Text.Text
inputValueDefinition formatter definition' =
    let Full.InputValueDefinition description' name' type'' defaultValue' directives' = definition'
     in optempty (description formatter) description'
            <> indentLine formatter
            <> Lazy.Text.fromStrict name'
            <> colon formatter
            <> type' type''
            <> maybe mempty (defaultValue formatter . Full.node) defaultValue'
            <> directives formatter directives'

typeDefinition :: Formatter -> Full.TypeDefinition -> Lazy.Text
typeDefinition formatter = \case
    Full.ScalarTypeDefinition description' name' directives'
        -> optempty (description formatter) description'
        <> "scalar "
        <> Lazy.Text.fromStrict name'
        <> optempty (directives formatter) directives'
    Full.ObjectTypeDefinition description' name' ifaces' directives' fields'
        -> optempty (description formatter) description'
        <> "type "
        <> Lazy.Text.fromStrict name'
        <> optempty (" " <>) (implementsInterfaces ifaces')
        <> optempty (directives formatter) directives'
        <> eitherFormat formatter " " ""
        <> bracesList formatter (fieldDefinition nextFormatter) fields'
    Full.InterfaceTypeDefinition description' name' ifaces' directives' fields'
        -> optempty (description formatter) description'
        <> "interface "
        <> Lazy.Text.fromStrict name'
        <> optempty (" " <>) (implementsInterfaces ifaces')
        <> optempty (directives formatter) directives'
        <> eitherFormat formatter " " ""
        <> bracesList formatter (fieldDefinition nextFormatter) fields'
    Full.UnionTypeDefinition description' name' directives' members'
        -> optempty (description formatter) description'
        <> "union "
        <> Lazy.Text.fromStrict name'
        <> optempty (directives formatter) directives'
        <> eitherFormat formatter " " ""
        <> unionMemberTypes formatter members'
    Full.EnumTypeDefinition description' name' directives' members'
        -> optempty (description formatter) description'
        <> "enum "
        <> Lazy.Text.fromStrict name'
        <> optempty (directives formatter) directives'
        <> eitherFormat formatter " " ""
        <> bracesList formatter (enumValueDefinition formatter) members'
    Full.InputObjectTypeDefinition description' name' directives' fields'
        -> optempty (description formatter) description'
        <> "input "
        <> Lazy.Text.fromStrict name'
        <> optempty (directives formatter) directives'
        <> eitherFormat formatter " " ""
        <> bracesList formatter (inputValueDefinition nextFormatter) fields'
  where
    nextFormatter = incrementIndent formatter

implementsInterfaces :: Foldable t => Full.ImplementsInterfaces t -> Lazy.Text
implementsInterfaces (Full.ImplementsInterfaces interfaces)
    | null interfaces = mempty
    | otherwise = Lazy.Text.fromStrict
        $ Text.append "implements "
        $ Text.intercalate " & "
        $ toList interfaces

unionMemberTypes :: Foldable t => Formatter -> Full.UnionMemberTypes t -> Lazy.Text
unionMemberTypes formatter (Full.UnionMemberTypes memberTypes)
    | null memberTypes = mempty
    | otherwise = Lazy.Text.append "="
        $ pipeList formatter
        $ Lazy.Text.fromStrict
        <$> toList memberTypes

pipeList :: Foldable t => Formatter -> t Lazy.Text -> Lazy.Text
pipeList Minified =  (" " <>) . Lazy.Text.intercalate " | " . toList
pipeList (Pretty _) =  Lazy.Text.concat
    . fmap (("\n" <> indentSymbol <> "| ") <>)
    . toList

enumValueDefinition :: Formatter -> Full.EnumValueDefinition -> Lazy.Text
enumValueDefinition (Pretty _) enumValue =
    let Full.EnumValueDefinition description' name' directives' = enumValue
        formatter = Pretty 1
     in description formatter description'
        <> indentLine formatter
        <> Lazy.Text.fromStrict name'
        <> directives formatter directives'
enumValueDefinition Minified enumValue =
    let Full.EnumValueDefinition description' name' directives' = enumValue
     in description Minified description'
        <> Lazy.Text.fromStrict name'
        <> directives Minified directives'

description :: Formatter -> Full.Description -> Lazy.Text.Text
description _formatter (Full.Description Nothing) = ""
description formatter (Full.Description (Just description')) =
    stringValue formatter description'

-- | Converts a t'Full.ExecutableDefinition' into a string.
definition :: Formatter -> Full.ExecutableDefinition -> Lazy.Text
definition formatter x
    | Pretty _ <- formatter = Lazy.Text.snoc (encodeDefinition x) '\n'
    | Minified <- formatter = encodeDefinition x
  where
    encodeDefinition (Full.DefinitionOperation operation)
        = operationDefinition formatter operation
    encodeDefinition (Full.DefinitionFragment fragment)
        = fragmentDefinition formatter fragment

-- | Converts a 'Full.OperationDefinition into a string.
operationDefinition :: Formatter -> Full.OperationDefinition -> Lazy.Text
operationDefinition formatter = \case
    Full.SelectionSet sels _ -> selectionSet formatter sels
    Full.OperationDefinition Full.Query name vars dirs sels _ ->
        "query " <> root name vars dirs sels
    Full.OperationDefinition Full.Mutation name vars dirs sels _ ->
        "mutation " <> root name vars dirs sels
    Full.OperationDefinition Full.Subscription name vars dirs sels _ ->
        "subscription " <> root name vars dirs sels
  where
    -- | Converts a Query or Mutation into a string.
    root :: Maybe Full.Name ->
        [Full.VariableDefinition] ->
        [Full.Directive] ->
        Full.SelectionSet ->
        Lazy.Text
    root name vars dirs sels
        = Lazy.Text.fromStrict (fold name)
        <> optempty (variableDefinitions formatter) vars
        <> optempty (directives formatter) dirs
        <> eitherFormat formatter " " mempty
        <> selectionSet formatter sels

variableDefinitions :: Formatter -> [Full.VariableDefinition] -> Lazy.Text
variableDefinitions formatter
    = parensCommas formatter $ variableDefinition formatter

variableDefinition :: Formatter -> Full.VariableDefinition -> Lazy.Text
variableDefinition formatter variableDefinition' =
    let Full.VariableDefinition variableName variableType defaultValue' _ =
            variableDefinition'
     in variable variableName
    <> colon formatter
    <> type' variableType
    <> maybe mempty (defaultValue formatter . Full.node) defaultValue'

defaultValue :: Formatter -> Full.ConstValue -> Lazy.Text
defaultValue formatter val
    = eitherFormat formatter " = " "="
    <> value formatter (fromConstValue val)

variable :: Full.Name -> Lazy.Text
variable var = "$" <> Lazy.Text.fromStrict var

selectionSet :: Formatter -> Full.SelectionSet -> Lazy.Text
selectionSet formatter
    = bracesList formatter (selection formatter)
    . NonEmpty.toList

selectionSetOpt :: Formatter -> Full.SelectionSetOpt -> Lazy.Text
selectionSetOpt formatter = bracesList formatter $ selection formatter

indentSymbol :: Lazy.Text
indentSymbol = "  "

indent :: (Integral a) => a -> Lazy.Text
indent indentation = Lazy.Text.replicate (fromIntegral indentation) indentSymbol

selection :: Formatter -> Full.Selection -> Lazy.Text
selection formatter = Lazy.Text.append (indentLine formatter')
    . encodeSelection
  where
    encodeSelection (Full.FieldSelection fieldSelection) =
        field formatter' fieldSelection
    encodeSelection (Full.InlineFragmentSelection fragmentSelection) =
        inlineFragment formatter' fragmentSelection
    encodeSelection (Full.FragmentSpreadSelection fragmentSelection) =
        fragmentSpread formatter' fragmentSelection
    formatter' = incrementIndent formatter

indentLine :: Formatter -> Lazy.Text
indentLine formatter
    | Pretty indentation <- formatter = indent indentation
    | otherwise = ""

incrementIndent :: Formatter -> Formatter
incrementIndent formatter
    | Pretty indentation <- formatter = Pretty $ indentation + 1
    | otherwise = Minified

colon :: Formatter -> Lazy.Text
colon formatter = eitherFormat formatter ": " ":"

-- | Converts Field into a string.
field :: Formatter -> Full.Field -> Lazy.Text
field formatter (Full.Field alias name args dirs set _)
    = optempty prependAlias (fold alias)
    <> Lazy.Text.fromStrict name
    <> optempty (arguments formatter) args
    <> optempty (directives formatter) dirs
    <> optempty selectionSetOpt' set
  where
    prependAlias aliasName = Lazy.Text.fromStrict aliasName <>  colon formatter
    selectionSetOpt' = (eitherFormat formatter " " "" <>)
        . selectionSetOpt formatter

arguments :: Formatter -> [Full.Argument] -> Lazy.Text
arguments formatter = parensCommas formatter $ argument formatter

argument :: Formatter -> Full.Argument -> Lazy.Text
argument formatter (Full.Argument name value' _)
    = Lazy.Text.fromStrict name
    <> colon formatter
    <> value formatter (Full.node value')

-- * Fragments

fragmentSpread :: Formatter -> Full.FragmentSpread -> Lazy.Text
fragmentSpread formatter (Full.FragmentSpread name directives' _)
    = "..." <> Lazy.Text.fromStrict name
    <> optempty (directives formatter) directives'

inlineFragment :: Formatter -> Full.InlineFragment -> Lazy.Text
inlineFragment formatter (Full.InlineFragment typeCondition directives' selections _)
    = "... on "
    <> Lazy.Text.fromStrict (fold typeCondition)
    <> directives formatter directives'
    <> eitherFormat formatter " " mempty
    <> selectionSet formatter selections

fragmentDefinition :: Formatter -> Full.FragmentDefinition -> Lazy.Text
fragmentDefinition formatter (Full.FragmentDefinition name tc dirs sels _)
    = "fragment " <> Lazy.Text.fromStrict name
    <> " on " <> Lazy.Text.fromStrict tc
    <> optempty (directives formatter) dirs
    <> eitherFormat formatter " " mempty
    <> selectionSet formatter sels

-- * Miscellaneous

-- | Converts a 'Full.Directive' into a string.
directive :: Formatter -> Full.Directive -> Lazy.Text
directive formatter (Full.Directive name args _)
    = "@" <> Lazy.Text.fromStrict name <> optempty (arguments formatter) args

directives :: Formatter -> [Full.Directive] -> Lazy.Text
directives Minified values = spaces (directive Minified) values
directives formatter values
    | null values = ""
    | otherwise = Lazy.Text.cons ' ' $ spaces (directive formatter) values

-- | Converts a 'Full.Value' into a string.
value :: Formatter -> Full.Value -> Lazy.Text
value _ (Full.Variable x) = variable x
value _ (Full.Int x) = Builder.toLazyText $ decimal x
value _ (Full.Float x) = Builder.toLazyText $ realFloat x
value _ (Full.Boolean  x) = booleanValue x
value _ Full.Null = "null"
value formatter (Full.String string) = stringValue formatter string
value _ (Full.Enum x) = Lazy.Text.fromStrict x
value formatter (Full.List x) = listValue formatter x
value formatter (Full.Object x) = objectValue formatter x

fromConstValue :: Full.ConstValue -> Full.Value
fromConstValue (Full.ConstInt x) = Full.Int x
fromConstValue (Full.ConstFloat x) = Full.Float x
fromConstValue (Full.ConstBoolean  x) = Full.Boolean x
fromConstValue Full.ConstNull = Full.Null
fromConstValue (Full.ConstString string) = Full.String string
fromConstValue (Full.ConstEnum x) = Full.Enum x
fromConstValue (Full.ConstList x) = Full.List $ fmap fromConstValue <$> x
fromConstValue (Full.ConstObject x) = Full.Object $ fromConstObjectField <$> x
  where
    fromConstObjectField Full.ObjectField{value = value', ..} =
        Full.ObjectField name (fromConstValue <$> value') location

booleanValue :: Bool -> Lazy.Text
booleanValue True  = "true"
booleanValue False = "false"

quote :: Builder.Builder
quote = Builder.singleton '\"'

oneLine :: Text -> Builder
oneLine string = quote <> Text.foldr merge quote string
  where
    merge = mappend . Builder.fromString . Full.escape

stringValue :: Formatter -> Text -> Lazy.Text
stringValue Minified string = Builder.toLazyText $ oneLine string
stringValue (Pretty indentation) string =
  if hasEscaped string
  then stringValue Minified string
  else Builder.toLazyText $ encoded lines'
    where
      isWhiteSpace char = char == ' ' || char == '\t'
      isNewline char = char == '\n' || char == '\r'
      hasEscaped = Text.any (not . isAllowed)
      isAllowed char =
          char == '\t' || isNewline char || (char >= '\x0020' && char /= '\x007F')

      tripleQuote = Builder.fromText "\"\"\""
      newline = Builder.singleton '\n'

      strip = Text.dropWhile isWhiteSpace . Text.dropWhileEnd isWhiteSpace
      lines' = map Builder.fromText $ Text.split isNewline (Text.replace "\r\n" "\n" $ strip string)
      encoded [] = oneLine string
      encoded [_] = oneLine string
      encoded lines'' = tripleQuote <> newline
        <> transformLines lines''
        <> Builder.fromLazyText (indent indentation) <> tripleQuote
      transformLines = foldr transformLine mempty
      transformLine "" acc = newline <> acc
      transformLine line' acc
            = Builder.fromLazyText (indent (indentation + 1))
            <> line' <> newline <> acc

listValue :: Formatter -> [Full.Node Full.Value] -> Lazy.Text
listValue formatter = bracketsCommas formatter $ value formatter . Full.node

objectValue :: Formatter -> [Full.ObjectField Full.Value] -> Lazy.Text
objectValue formatter = intercalate $ objectField formatter
  where
    intercalate f
        = braces
        . Lazy.Text.intercalate (eitherFormat formatter ", " ",")
        . fmap f

objectField :: Formatter -> Full.ObjectField Full.Value -> Lazy.Text
objectField formatter (Full.ObjectField name (Full.Node value' _) _) =
    Lazy.Text.fromStrict name <> colon formatter <> value formatter value'

-- | Converts a 'Full.Type' a type into a string.
type' :: Full.Type -> Lazy.Text
type' (Full.TypeNamed x) = Lazy.Text.fromStrict x
type' (Full.TypeList x) = listType x
type' (Full.TypeNonNull x) = nonNullType x

listType :: Full.Type -> Lazy.Text
listType x = brackets (type' x)

nonNullType :: Full.NonNullType -> Lazy.Text
nonNullType (Full.NonNullTypeNamed x) = Lazy.Text.fromStrict x <> "!"
nonNullType (Full.NonNullTypeList x) = listType x <> "!"

-- | Produces lowercase operation type: query, mutation or subscription.
operationType :: Formatter -> Full.OperationType -> Lazy.Text
operationType _formatter Full.Query = "query"
operationType _formatter Full.Mutation = "mutation"
operationType _formatter Full.Subscription = "subscription"

-- * Internal

between :: Char -> Char -> Lazy.Text -> Lazy.Text
between open close = Lazy.Text.cons open . (`Lazy.Text.snoc` close)

parens :: Lazy.Text -> Lazy.Text
parens = between '(' ')'

brackets :: Lazy.Text -> Lazy.Text
brackets = between '[' ']'

braces :: Lazy.Text -> Lazy.Text
braces = between '{' '}'

spaces :: forall a. (a -> Lazy.Text) -> [a] -> Lazy.Text
spaces f = Lazy.Text.intercalate "\SP" . fmap f

parensCommas :: forall a. Formatter -> (a -> Lazy.Text) -> [a] -> Lazy.Text
parensCommas formatter f
    = parens
    . Lazy.Text.intercalate (eitherFormat formatter ", " ",")
    . fmap f

bracketsCommas :: Formatter -> (a -> Lazy.Text) -> [a] -> Lazy.Text
bracketsCommas formatter f
    = brackets
    . Lazy.Text.intercalate (eitherFormat formatter ", " ",")
    . fmap f

bracesList :: forall a. Formatter -> (a -> Lazy.Text) -> [a] -> Lazy.Text
bracesList (Pretty intendation) f xs
    = Lazy.Text.snoc (Lazy.Text.intercalate "\n" content) '\n'
    <> (Lazy.Text.snoc $ Lazy.Text.replicate (fromIntegral intendation) "  ") '}'
  where
    content = "{" : fmap f xs
bracesList Minified f xs = braces $ Lazy.Text.intercalate "," $ fmap f xs

optempty :: (Eq a, Monoid a, Monoid b) => (a -> b) -> a -> b
optempty f xs = if xs == mempty then mempty else f xs

eitherFormat :: forall a. Formatter -> a -> a -> a
eitherFormat (Pretty _) x _ = x
eitherFormat Minified _ x = x
