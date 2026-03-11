{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE Safe #-}

-- | @GraphQL@ document parser.
module Language.GraphQL.AST.Parser
    ( document
    ) where

import Control.Applicative (Alternative(..), optional)
import Control.Applicative.Combinators (sepBy1)
import qualified Control.Applicative.Combinators.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Language.GraphQL.AST.DirectiveLocation as Directive
import Language.GraphQL.AST.DirectiveLocation (DirectiveLocation)
import qualified Language.GraphQL.AST.Document as Full
import Language.GraphQL.AST.Lexer
import Text.Megaparsec
    ( MonadParsec(..)
    , SourcePos(..)
    , getSourcePos
    , lookAhead
    , option
    , try
    , unPos
    , (<?>)
    )
import Data.Maybe (isJust)

-- | Parser for the GraphQL documents.
document :: Parser Full.Document
document = unicodeBOM
    *> spaceConsumer
    *> lexeme (NonEmpty.some definition)

definition :: Parser Full.Definition
definition = Full.ExecutableDefinition <$> executableDefinition
    <|> typeSystemDefinition'
    <|> typeSystemExtension'
    <?> "Definition"
  where
    typeSystemDefinition' = do
        location <- getLocation
        definition' <- typeSystemDefinition
        pure $ Full.TypeSystemDefinition definition' location
    typeSystemExtension' = do
        location <- getLocation
        definition' <- typeSystemExtension
        pure $ Full.TypeSystemExtension definition' location

getLocation :: Parser Full.Location
getLocation = fromSourcePosition <$> getSourcePos
  where
    fromSourcePosition SourcePos{..} =
        Full.Location (wordFromPosition sourceLine) (wordFromPosition sourceColumn)
    wordFromPosition = fromIntegral . unPos

executableDefinition :: Parser Full.ExecutableDefinition
executableDefinition = Full.DefinitionOperation <$> operationDefinition
    <|> Full.DefinitionFragment  <$> fragmentDefinition
    <?> "ExecutableDefinition"

typeSystemDefinition :: Parser Full.TypeSystemDefinition
typeSystemDefinition = schemaDefinition
    <|> typeSystemDefinitionWithDescription
    <?> "TypeSystemDefinition"
  where
    typeSystemDefinitionWithDescription = description
        >>= liftA2 (<|>) typeDefinition' directiveDefinition
    typeDefinition' description' = Full.TypeDefinition
        <$> typeDefinition description'

typeSystemExtension :: Parser Full.TypeSystemExtension
typeSystemExtension = Full.SchemaExtension <$> schemaExtension
    <|> Full.TypeExtension <$> typeExtension
    <?> "TypeSystemExtension"

directiveDefinition :: Full.Description -> Parser Full.TypeSystemDefinition
directiveDefinition description' = Full.DirectiveDefinition description'
    <$ symbol "directive"
    <* at
    <*> name
    <*> argumentsDefinition
    <*> (isJust <$> optional (symbol "repeatable"))
    <* symbol "on"
    <*> directiveLocations
    <?> "DirectiveDefinition"

directiveLocations :: Parser (NonEmpty DirectiveLocation)
directiveLocations = optional pipe
    *> directiveLocation `NonEmpty.sepBy1` pipe
    <?> "DirectiveLocations"

directiveLocation :: Parser DirectiveLocation
directiveLocation = e (Directive.Query <$ symbol "QUERY")
    <|> e (Directive.Mutation <$ symbol "MUTATION")
    <|> e (Directive.Subscription <$ symbol "SUBSCRIPTION")
    <|> t (Directive.FieldDefinition <$ symbol "FIELD_DEFINITION")
    <|> e (Directive.Field <$ symbol "FIELD")
    <|> e (Directive.FragmentDefinition <$ "FRAGMENT_DEFINITION")
    <|> e (Directive.FragmentSpread <$ "FRAGMENT_SPREAD")
    <|> e (Directive.InlineFragment <$ "INLINE_FRAGMENT")
    <|> t (Directive.Schema <$ symbol "SCHEMA")
    <|> t (Directive.Scalar <$ symbol "SCALAR")
    <|> t (Directive.Object <$ symbol "OBJECT")
    <|> t (Directive.ArgumentDefinition <$ symbol "ARGUMENT_DEFINITION")
    <|> t (Directive.Interface <$ symbol "INTERFACE")
    <|> t (Directive.Union <$ symbol "UNION")
    <|> t (Directive.EnumValue <$ symbol "ENUM_VALUE")
    <|> t (Directive.Enum <$ symbol "ENUM")
    <|> t (Directive.InputObject <$ symbol "INPUT_OBJECT")
    <|> t (Directive.InputFieldDefinition <$ symbol "INPUT_FIELD_DEFINITION")
    <?> "DirectiveLocation"
  where
    e = fmap Directive.ExecutableDirectiveLocation
    t = fmap Directive.TypeSystemDirectiveLocation

typeDefinition :: Full.Description -> Parser Full.TypeDefinition
typeDefinition description' = scalarTypeDefinition description'
    <|> objectTypeDefinition description'
    <|> interfaceTypeDefinition description'
    <|> unionTypeDefinition description'
    <|> enumTypeDefinition description'
    <|> inputObjectTypeDefinition description'
    <?> "TypeDefinition"

typeExtension :: Parser Full.TypeExtension
typeExtension = scalarTypeExtension
    <|> objectTypeExtension
    <|> interfaceTypeExtension
    <|> unionTypeExtension
    <|> enumTypeExtension
    <|> inputObjectTypeExtension
    <?> "TypeExtension"

scalarTypeDefinition :: Full.Description -> Parser Full.TypeDefinition
scalarTypeDefinition description' = Full.ScalarTypeDefinition description'
    <$ symbol "scalar"
    <*> name
    <*> directives
    <?> "ScalarTypeDefinition"

scalarTypeExtension :: Parser Full.TypeExtension
scalarTypeExtension = extend "scalar" "ScalarTypeExtension"
    $ (Full.ScalarTypeExtension <$> name <*> NonEmpty.some directive) :| []

objectTypeDefinition :: Full.Description -> Parser Full.TypeDefinition
objectTypeDefinition description' = Full.ObjectTypeDefinition description'
    <$ symbol "type"
    <*> name
    <*> option (Full.ImplementsInterfaces []) (implementsInterfaces sepBy1)
    <*> directives
    <*> braces (many fieldDefinition)
    <?> "ObjectTypeDefinition"

objectTypeExtension :: Parser Full.TypeExtension
objectTypeExtension = extend "type" "ObjectTypeExtension"
    $ fieldsDefinitionExtension :|
        [ directivesExtension
        , implementsInterfacesExtension
        ]
  where
    fieldsDefinitionExtension = Full.ObjectTypeFieldsDefinitionExtension
        <$> name
        <*> option (Full.ImplementsInterfaces []) (implementsInterfaces sepBy1)
        <*> directives
        <*> braces (NonEmpty.some fieldDefinition)
    directivesExtension = Full.ObjectTypeDirectivesExtension
        <$> name
        <*> option (Full.ImplementsInterfaces []) (implementsInterfaces sepBy1)
        <*> NonEmpty.some directive
    implementsInterfacesExtension = Full.ObjectTypeImplementsInterfacesExtension
        <$> name
        <*> implementsInterfaces NonEmpty.sepBy1

description :: Parser Full.Description
description = Full.Description
    <$> optional stringValue
    <?> "Description"

unionTypeDefinition :: Full.Description -> Parser Full.TypeDefinition
unionTypeDefinition description' = Full.UnionTypeDefinition description'
    <$ symbol "union"
    <*> name
    <*> directives
    <*> option (Full.UnionMemberTypes []) (unionMemberTypes sepBy1)
    <?> "UnionTypeDefinition"

unionTypeExtension :: Parser Full.TypeExtension
unionTypeExtension = extend "union" "UnionTypeExtension"
    $ unionMemberTypesExtension :| [directivesExtension]
  where
    unionMemberTypesExtension = Full.UnionTypeUnionMemberTypesExtension
        <$> name
        <*> directives
        <*> unionMemberTypes NonEmpty.sepBy1
    directivesExtension = Full.UnionTypeDirectivesExtension
        <$> name
        <*> NonEmpty.some directive

unionMemberTypes ::
    Foldable t =>
    (Parser Text -> Parser Text -> Parser (t Full.NamedType)) ->
    Parser (Full.UnionMemberTypes t)
unionMemberTypes sepBy' = Full.UnionMemberTypes
    <$ equals
    <* optional pipe
    <*> name `sepBy'` pipe
    <?> "UnionMemberTypes"

interfaceTypeDefinition :: Full.Description -> Parser Full.TypeDefinition
interfaceTypeDefinition description' = Full.InterfaceTypeDefinition description'
    <$ symbol "interface"
    <*> name
    <*> option (Full.ImplementsInterfaces []) (implementsInterfaces sepBy1)
    <*> directives
    <*> braces (many fieldDefinition)
    <?> "InterfaceTypeDefinition"

interfaceTypeExtension :: Parser Full.TypeExtension
interfaceTypeExtension = extend "interface" "InterfaceTypeExtension"
    $ fieldsDefinitionExtension :| [directivesExtension]
  where
    fieldsDefinitionExtension = Full.InterfaceTypeFieldsDefinitionExtension
        <$> name
        <*> directives
        <*> braces (NonEmpty.some fieldDefinition)
    directivesExtension = Full.InterfaceTypeDirectivesExtension
        <$> name
        <*> NonEmpty.some directive

enumTypeDefinition :: Full.Description -> Parser Full.TypeDefinition
enumTypeDefinition description' = Full.EnumTypeDefinition description'
    <$ symbol "enum"
    <*> name
    <*> directives
    <*> listOptIn braces enumValueDefinition
    <?> "EnumTypeDefinition"

enumTypeExtension :: Parser Full.TypeExtension
enumTypeExtension = extend "enum" "EnumTypeExtension"
    $ enumValuesDefinitionExtension :| [directivesExtension]
  where
    enumValuesDefinitionExtension = Full.EnumTypeEnumValuesDefinitionExtension
        <$> name
        <*> directives
        <*> braces (NonEmpty.some enumValueDefinition)
    directivesExtension = Full.EnumTypeDirectivesExtension
        <$> name
        <*> NonEmpty.some directive

inputObjectTypeDefinition :: Full.Description -> Parser Full.TypeDefinition
inputObjectTypeDefinition description' = Full.InputObjectTypeDefinition description'
    <$ symbol "input"
    <*> name
    <*> directives
    <*> listOptIn braces inputValueDefinition
    <?> "InputObjectTypeDefinition"

inputObjectTypeExtension :: Parser Full.TypeExtension
inputObjectTypeExtension = extend "input" "InputObjectTypeExtension"
    $ inputFieldsDefinitionExtension :| [directivesExtension]
  where
    inputFieldsDefinitionExtension = Full.InputObjectTypeInputFieldsDefinitionExtension
        <$> name
        <*> directives
        <*> braces (NonEmpty.some inputValueDefinition)
    directivesExtension = Full.InputObjectTypeDirectivesExtension
        <$> name
        <*> NonEmpty.some directive

enumValueDefinition :: Parser Full.EnumValueDefinition
enumValueDefinition = Full.EnumValueDefinition
    <$> description
    <*> enumValue
    <*> directives
    <?> "EnumValueDefinition"

implementsInterfaces ::
    Foldable t =>
    (Parser Text -> Parser Text -> Parser (t Full.NamedType)) ->
    Parser (Full.ImplementsInterfaces t)
implementsInterfaces sepBy' = Full.ImplementsInterfaces
    <$ symbol "implements"
    <* optional amp
    <*> name `sepBy'` amp
    <?> "ImplementsInterfaces"

inputValueDefinition :: Parser Full.InputValueDefinition
inputValueDefinition = Full.InputValueDefinition
    <$> description
    <*> name
    <* colon
    <*> type'
    <*> defaultValue
    <*> directives
    <?> "InputValueDefinition"

argumentsDefinition :: Parser Full.ArgumentsDefinition
argumentsDefinition = Full.ArgumentsDefinition
    <$> listOptIn parens inputValueDefinition
    <?> "ArgumentsDefinition"

fieldDefinition :: Parser Full.FieldDefinition
fieldDefinition = Full.FieldDefinition
    <$> description
    <*> name
    <*> argumentsDefinition
    <* colon
    <*> type'
    <*> directives
    <?> "FieldDefinition"

schemaDefinition :: Parser Full.TypeSystemDefinition
schemaDefinition = Full.SchemaDefinition
    <$ symbol "schema"
    <*> directives
    <*> operationTypeDefinitions
    <?> "SchemaDefinition"

operationTypeDefinitions :: Parser (NonEmpty Full.OperationTypeDefinition)
operationTypeDefinitions = braces $ NonEmpty.some operationTypeDefinition

schemaExtension :: Parser Full.SchemaExtension
schemaExtension = extend "schema" "SchemaExtension"
    $ schemaOperationExtension :| [directivesExtension]
  where
    directivesExtension = Full.SchemaDirectivesExtension
        <$> NonEmpty.some directive
    schemaOperationExtension = Full.SchemaOperationExtension
        <$> directives
        <*> operationTypeDefinitions

operationTypeDefinition :: Parser Full.OperationTypeDefinition
operationTypeDefinition = Full.OperationTypeDefinition
    <$> operationType <* colon
    <*> name
    <?> "OperationTypeDefinition"

operationDefinition :: Parser Full.OperationDefinition
operationDefinition = shorthand
    <|> operationDefinition'
    <?> "OperationDefinition"
  where
    shorthand = do
        location <- getLocation
        selectionSet' <- selectionSet
        pure $ Full.SelectionSet selectionSet' location
    operationDefinition' = do
        location <- getLocation
        operationType' <- operationType
        operationName <- optional name
        variableDefinitions' <- variableDefinitions
        directives' <- directives
        selectionSet' <- selectionSet
        pure $ Full.OperationDefinition
            operationType'
            operationName
            variableDefinitions'
            directives'
            selectionSet'
            location

operationType :: Parser Full.OperationType
operationType = Full.Query <$ symbol "query"
    <|> Full.Mutation <$ symbol "mutation"
    <|> Full.Subscription <$ symbol "subscription"
    <?> "OperationType"

selectionSet :: Parser Full.SelectionSet
selectionSet = braces (NonEmpty.some selection) <?> "SelectionSet"

selectionSetOpt :: Parser Full.SelectionSetOpt
selectionSetOpt = listOptIn braces selection <?> "SelectionSet"

selection :: Parser Full.Selection
selection = Full.FieldSelection <$> field
    <|> Full.FragmentSpreadSelection <$> try fragmentSpread
    <|> Full.InlineFragmentSelection <$> inlineFragment
    <?> "Selection"

field :: Parser Full.Field
field = label "Field" $ do
    location <- getLocation
    alias' <- optional alias
    name' <- name
    arguments' <- arguments
    directives' <- directives
    selectionSetOpt' <- selectionSetOpt
    pure $ Full.Field alias' name' arguments' directives' selectionSetOpt' location

alias :: Parser Full.Name
alias = try (name <* colon) <?> "Alias"

arguments :: Parser [Full.Argument]
arguments = listOptIn parens argument <?> "Arguments"

argument :: Parser Full.Argument
argument = label "Argument" $ do
    location <- getLocation
    name' <- name
    colon
    value' <- valueNode value
    pure $ Full.Argument name' value' location

fragmentSpread :: Parser Full.FragmentSpread
fragmentSpread = label "FragmentSpread" $ do
    location <- getLocation
    _ <- spread
    fragmentName' <- fragmentName
    directives' <- directives
    pure $ Full.FragmentSpread fragmentName' directives' location

inlineFragment :: Parser Full.InlineFragment
inlineFragment = label "InlineFragment" $ do
    location <- getLocation
    _ <- spread
    typeCondition' <- optional typeCondition
    directives' <- directives
    selectionSet' <- selectionSet
    pure $ Full.InlineFragment typeCondition' directives' selectionSet' location

fragmentDefinition :: Parser Full.FragmentDefinition
fragmentDefinition =  label "FragmentDefinition" $ do
    location <- getLocation
    _ <- symbol "fragment"
    fragmentName' <- name
    typeCondition' <- typeCondition
    directives' <- directives
    selectionSet' <- selectionSet
    pure $ Full.FragmentDefinition
        fragmentName' typeCondition' directives' selectionSet' location

fragmentName :: Parser Full.Name
fragmentName = but (symbol "on") *> name <?> "FragmentName"

typeCondition :: Parser Full.TypeCondition
typeCondition = symbol "on" *> name <?> "TypeCondition"

valueNode :: forall a. Parser a -> Parser (Full.Node a)
valueNode valueParser = do
    location <- getLocation
    value' <- valueParser
    pure $ Full.Node value' location

value :: Parser Full.Value
value = Full.Variable <$> variable
    <|> Full.Float <$> try float
    <|> Full.Int <$> integer
    <|> Full.Boolean <$> booleanValue
    <|> Full.Null <$  nullValue
    <|> Full.String <$> stringValue
    <|> Full.Enum <$> try enumValue
    <|> Full.List <$> brackets (many $ valueNode value)
    <|> Full.Object <$> braces (many $ objectField $ valueNode value)
    <?> "Value"

constValue :: Parser Full.ConstValue
constValue = Full.ConstFloat <$> try float
    <|> Full.ConstInt <$> integer
    <|> Full.ConstBoolean <$> booleanValue
    <|> Full.ConstNull <$ nullValue
    <|> Full.ConstString <$> stringValue
    <|> Full.ConstEnum <$> try enumValue
    <|> Full.ConstList <$> brackets (many $ valueNode constValue)
    <|> Full.ConstObject <$> braces (many $ objectField $ valueNode constValue)
    <?> "Value"

booleanValue :: Parser Bool
booleanValue = True  <$ symbol "true"
    <|> False <$ symbol "false"
    <?> "BooleanValue"

enumValue :: Parser Full.Name
enumValue = but (symbol "true")
    *> but (symbol "false")
    *> but (symbol "null")
    *> name
    <?> "EnumValue"

stringValue :: Parser Text
stringValue = blockString <|> string <?> "StringValue"

nullValue :: Parser Text
nullValue = symbol "null" <?> "NullValue"

objectField :: forall a. Parser (Full.Node a) -> Parser (Full.ObjectField a)
objectField valueParser = label "ObjectField" $ do
    location <- getLocation
    fieldName <- name
    colon
    fieldValue <- valueParser
    pure $ Full.ObjectField fieldName fieldValue location

variableDefinitions :: Parser [Full.VariableDefinition]
variableDefinitions = listOptIn parens variableDefinition
    <?> "VariableDefinitions"

variableDefinition :: Parser Full.VariableDefinition
variableDefinition = label "VariableDefinition" $ do
    location <- getLocation
    variableName <- variable
    colon
    variableType <- type'
    variableValue <- defaultValue
    pure $ Full.VariableDefinition variableName variableType variableValue location

variable :: Parser Full.Name
variable = dollar *> name <?> "Variable"

defaultValue :: Parser (Maybe (Full.Node Full.ConstValue))
defaultValue = optional (equals *> valueNode constValue) <?> "DefaultValue"

type' :: Parser Full.Type
type' = try (Full.TypeNonNull <$> nonNullType)
    <|> Full.TypeList <$> brackets type'
    <|> Full.TypeNamed <$> name
    <?> "Type"

nonNullType :: Parser Full.NonNullType
nonNullType = Full.NonNullTypeNamed <$> name <* bang
    <|> Full.NonNullTypeList  <$> brackets type'  <* bang
    <?> "NonNullType"

directives :: Parser [Full.Directive]
directives = many directive <?> "Directives"

directive :: Parser Full.Directive
directive = label "Directive" $ do
    location <- getLocation
    at
    directiveName <- name
    directiveArguments <- arguments
    pure $ Full.Directive directiveName directiveArguments location

listOptIn :: (Parser [a] -> Parser [a]) -> Parser a -> Parser [a]
listOptIn surround = option [] . surround . some

-- Hack to reverse parser success
but :: Parser a -> Parser ()
but pn = False <$ lookAhead pn <|> pure True >>= \case
    False -> empty
    True  -> pure ()
