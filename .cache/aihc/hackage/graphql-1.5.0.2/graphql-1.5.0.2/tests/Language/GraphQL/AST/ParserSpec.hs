{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.AST.ParserSpec
    ( spec
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as Text
import Language.GraphQL.AST.Document
import qualified Language.GraphQL.AST.DirectiveLocation as DirLoc
import Language.GraphQL.AST.Parser
import Test.Hspec (Spec, describe, it, context)
import Test.Hspec.Megaparsec
    ( shouldParse
    , shouldFailOn
    , parseSatisfies
    , shouldSucceedOn
    )
import Text.Megaparsec (parse)
import Test.QuickCheck (property, NonEmptyList (..), mapSize)
import Language.GraphQL.AST.Arbitrary

spec :: Spec
spec = describe "Parser" $ do
    it "accepts BOM header" $
        parse document "" `shouldSucceedOn` "\xfeff{foo}"

    context "Arguments" $ do
        it "accepts block strings as argument" $
            parse document "" `shouldSucceedOn`
                "{ hello(text: \"\"\"Argument\"\"\") }"

        it "accepts strings as argument" $
            parse document "" `shouldSucceedOn` "{ hello(text: \"Argument\") }"

        it "accepts int as argument" $
            parse document "" `shouldSucceedOn` "{ user(id: 4) }"

        it "accepts boolean as argument" $
            parse document "" `shouldSucceedOn`
                "{ hello(flag: true) { field1 } }"

        it "accepts float as argument" $
            parse document "" `shouldSucceedOn`
                "{ body(height: 172.5) { height } }"

        it "accepts empty list as argument" $
            parse document "" `shouldSucceedOn` "{ query(list: []) { field1 } }"

        it "accepts two required arguments" $
            parse document "" `shouldSucceedOn`
                "mutation auth($username: String!, $password: String!) { test }"

        it "accepts two string arguments" $
            parse document "" `shouldSucceedOn`
                "mutation auth { test(username: \"username\", password: \"password\") }"

        it "accepts two block string arguments" $
            let given = "mutation auth {\n\
                    \  test(username: \"\"\"username\"\"\", password: \"\"\"password\"\"\")\n\
                    \}"
             in parse document "" `shouldSucceedOn` given

        it "fails to parse an empty argument list in parens" $
            parse document "" `shouldFailOn` "{ test() }"

        it "accepts any arguments" $ mapSize (const 10) $ property $ \xs ->
            let arguments' = map printArgument
                    $ getNonEmpty (xs :: NonEmptyList (AnyArgument AnyValue))
                query' = "query(" <> Text.intercalate ", " arguments' <> ")"
             in parse document "" `shouldSucceedOn` ("{ " <> query' <> " }")

    it "parses minimal schema definition" $
        parse document "" `shouldSucceedOn` "schema { query: Query }"

    it "parses minimal scalar definition" $
        parse document "" `shouldSucceedOn` "scalar Time"

    it "parses ImplementsInterfaces" $
        parse document "" `shouldSucceedOn`
            "type Person implements NamedEntity & ValuedEntity {\n\
            \  name: String\n\
            \}"

    it "parses a  type without ImplementsInterfaces" $
        parse document "" `shouldSucceedOn`
            "type Person {\n\
            \  name: String\n\
            \}"

    it "parses ArgumentsDefinition in an ObjectDefinition" $
        parse document "" `shouldSucceedOn`
            "type Person {\n\
            \  name(first: String, last: String): String\n\
            \}"

    it "parses minimal union type definition" $
        parse document "" `shouldSucceedOn`
            "union SearchResult = Photo | Person"

    it "parses minimal interface type definition" $
        parse document "" `shouldSucceedOn`
            "interface NamedEntity {\n\
            \  name: String\n\
            \}"

    it "parses ImplementsInterfaces on interfaces" $
        parse document "" `shouldSucceedOn`
            "interface Person implements NamedEntity & ValuedEntity {\n\
            \  name: String\n\
            \}"

    it "parses minimal enum type definition" $
        parse document "" `shouldSucceedOn`
            "enum Direction {\n\
            \  NORTH\n\
            \  EAST\n\
            \  SOUTH\n\
            \  WEST\n\
            \}"

    it "parses minimal input object type definition" $
        parse document "" `shouldSucceedOn`
            "input Point2D {\n\
            \  x: Float\n\
            \  y: Float\n\
            \}"

    it "parses minimal input enum definition with an optional pipe" $
        parse document "" `shouldSucceedOn`
            "directive @example on\n\
            \  | FIELD\n\
            \  | FRAGMENT_SPREAD"

    it "parses two minimal directive definitions" $
        let directive name' loc = TypeSystemDefinition
                $ DirectiveDefinition
                    (Description Nothing)
                    name'
                    (ArgumentsDefinition [])
                    False
                    (loc :| [])
            example1 = directive "example1"
                (DirLoc.TypeSystemDirectiveLocation DirLoc.FieldDefinition)
                (Location {line = 1, column = 1})
            example2 = directive "example2"
                (DirLoc.ExecutableDirectiveLocation DirLoc.Field)
                (Location {line = 2, column = 1})
            testSchemaExtension = example1 :| [example2]
            query = Text.unlines
                [ "directive @example1 on FIELD_DEFINITION"
                , "directive @example2 on FIELD"
                ]
         in parse document "" query `shouldParse` testSchemaExtension

    it "parses a directive definition with a default empty list argument" $
        let argumentValue = Just
                $ Node (ConstList [])
                $ Location{ line = 1, column = 33 }
            loc = DirLoc.TypeSystemDirectiveLocation DirLoc.FieldDefinition
            argumentValueDefinition = InputValueDefinition
                (Description Nothing)
                "foo"
                (TypeList (TypeNamed "String"))
                argumentValue
                []
            definition = DirectiveDefinition
                (Description Nothing)
                "test"
                (ArgumentsDefinition [argumentValueDefinition])
                False
                (loc :| [])
            directive = TypeSystemDefinition definition
                $ Location{ line = 1, column = 1 }
            query = "directive @test(foo: [String] = []) on FIELD_DEFINITION"
         in parse document "" query `shouldParse` (directive :| [])

    it "parses schema extension with a new directive" $
        parse document "" `shouldSucceedOn` "extend schema @newDirective"

    it "parses schema extension with an operation type definition" $
        parse document "" `shouldSucceedOn` "extend schema { query: Query }"

    it "parses schema extension with an operation type and directive" $
        let newDirective = Directive "newDirective" [] $ Location 1 15
            schemaExtension = SchemaExtension
                $ SchemaOperationExtension [newDirective]
                $ OperationTypeDefinition Query "Query" :| []
            testSchemaExtension = TypeSystemExtension schemaExtension
                $ Location 1 1
            query = "extend schema @newDirective { query: Query }"
         in parse document "" query `shouldParse` (testSchemaExtension :| [])

    it "parses a repeatable directive definition" $
        let given = "directive @test repeatable on FIELD_DEFINITION"
            isRepeatable (TypeSystemDefinition definition' _ :| [])
                | DirectiveDefinition _ _ _ repeatable _ <- definition' = repeatable
            isRepeatable _ = False
         in parse document "" given `parseSatisfies` isRepeatable

    it "parses an object extension" $
        parse document "" `shouldSucceedOn`
            "extend type Story { isHiddenLocally: Boolean }"

    it "rejects variables in DefaultValue" $
        parse document "" `shouldFailOn`
            "query ($book: String = \"Zarathustra\", $author: String = $book) {\n\
            \  title\n\
            \}"

    it "rejects empty selection set" $
       parse document "" `shouldFailOn` "query { innerField {} }"

    it "parses documents beginning with a comment" $
        parse document "" `shouldSucceedOn`
            "\"\"\"\n\
            \Query\n\
            \\"\"\"\n\
            \type Query {\n\
            \  queryField: String\n\
            \}"

    it "parses subscriptions" $
        parse document "" `shouldSucceedOn`
            "subscription NewMessages {\n\
            \  newMessage(roomId: 123) {\n\
            \    sender\n\
            \  }\n\
            \}"
