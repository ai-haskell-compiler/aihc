{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.AST.EncoderSpec
    ( spec
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Language.GraphQL.AST.Document as Full
import Language.GraphQL.AST.Encoder
import Test.Hspec (Spec, context, describe, it, shouldBe, shouldStartWith, shouldEndWith, shouldNotContain)
import Test.QuickCheck (choose, oneof, forAll)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Language.GraphQL.AST.DirectiveLocation as DirectiveLocation

spec :: Spec
spec = do
    describe "value" $ do
        context "minified" $ do
            it "encodes null" $
                value minified Full.Null `shouldBe` "null"
            it "escapes \\" $
                value minified (Full.String "\\") `shouldBe` "\"\\\\\""
            it "escapes double quotes" $
                value minified (Full.String "\"") `shouldBe` "\"\\\"\""
            it "escapes \\f" $
                value minified (Full.String "\f") `shouldBe` "\"\\f\""
            it "escapes \\n" $
                value minified (Full.String "\n") `shouldBe` "\"\\n\""
            it "escapes \\r" $
                value minified (Full.String "\r") `shouldBe` "\"\\r\""
            it "escapes \\t" $
                value minified (Full.String "\t") `shouldBe` "\"\\t\""
            it "escapes backspace" $
                value minified (Full.String "a\bc") `shouldBe` "\"a\\bc\""
            context "escapes Unicode for chars less than 0010" $ do
                it "Null" $ value minified (Full.String "\x0000") `shouldBe` "\"\\u0000\""
                it "bell" $ value minified (Full.String "\x0007") `shouldBe` "\"\\u0007\""
            context "escapes Unicode for char less than 0020" $ do
                it "DLE" $ value minified (Full.String "\x0010") `shouldBe` "\"\\u0010\""
                it "EM" $ value minified (Full.String "\x0019") `shouldBe` "\"\\u0019\""
            context "encodes without escape" $ do
                it "space" $ value minified (Full.String "\x0020") `shouldBe` "\" \""
                it "~" $ value minified (Full.String "\x007E") `shouldBe` "\"~\""

        context "pretty" $ do
            it "encodes null" $
                value pretty Full.Null `shouldBe` "null"

            it "uses strings for short string values" $
                value pretty (Full.String "Short text") `shouldBe` "\"Short text\""
            it "uses block strings for text with new lines, with newline symbol" $
                let expected = "\"\"\"\n\
                        \  Line 1\n\
                        \  Line 2\n\
                        \\"\"\""
                    actual = value pretty $ Full.String "Line 1\nLine 2"
                 in actual `shouldBe` expected
            it "uses block strings for text with new lines, with CR symbol" $
                let expected = "\"\"\"\n\
                        \  Line 1\n\
                        \  Line 2\n\
                        \\"\"\""
                    actual = value pretty $ Full.String "Line 1\rLine 2"
                 in actual `shouldBe` expected
            it "uses block strings for text with new lines, with CR symbol followed by newline" $
                let expected = "\"\"\"\n\
                        \  Line 1\n\
                        \  Line 2\n\
                        \\"\"\""
                    actual = value pretty $ Full.String "Line 1\r\nLine 2"
                 in actual `shouldBe` expected
            it "encodes as one line string if has escaped symbols" $ do
                let genNotAllowedSymbol = oneof
                        [ choose ('\x0000', '\x0008')
                        , choose ('\x000B', '\x000C')
                        , choose ('\x000E', '\x001F')
                        , pure '\x007F'
                        ]
                forAll genNotAllowedSymbol $ \x -> do
                    let rawValue = "Short \n" <> Text.Lazy.cons x "text"
                        encoded = Text.Lazy.unpack
                            $ value pretty
                            $ Full.String
                            $ Text.Lazy.toStrict rawValue
                    shouldStartWith encoded "\""
                    shouldEndWith encoded "\""
                    shouldNotContain encoded "\"\"\""

            it "Hello world" $
                let actual = value pretty
                        $ Full.String "Hello,\n  World!\n\nYours,\n  GraphQL."
                    expected = "\"\"\"\n\
                        \  Hello,\n\
                        \    World!\n\
                        \\n\
                        \  Yours,\n\
                        \    GraphQL.\n\
                        \\"\"\""
                  in actual `shouldBe` expected

            it "has only newlines" $
                let actual = value pretty $ Full.String "\n"
                    expected = "\"\"\"\n\n\n\"\"\""
                 in actual `shouldBe` expected
            it "has newlines and one symbol at the begining" $
                let actual = value pretty $ Full.String "a\n\n"
                    expected = "\"\"\"\n\
                        \  a\n\
                        \\n\
                        \\n\
                        \\"\"\""
                 in actual `shouldBe` expected
            it "has newlines and one symbol at the end" $
                let actual = value pretty $ Full.String "\n\na"
                    expected = "\"\"\"\n\
                        \\n\
                        \\n\
                        \  a\n\
                        \\"\"\""
                 in actual `shouldBe` expected
            it "has newlines and one symbol in the middle" $
                let actual = value pretty $ Full.String "\na\n"
                    expected = "\"\"\"\n\
                        \\n\
                        \  a\n\
                        \\n\
                        \\"\"\""
                 in actual `shouldBe` expected
            it "skip trailing whitespaces" $
                let actual = value pretty $ Full.String "  Short\ntext    "
                    expected = "\"\"\"\n\
                        \  Short\n\
                        \  text\n\
                        \\"\"\""
                 in actual `shouldBe` expected

    describe "definition" $
        it "indents block strings in arguments" $
            let location = Full.Location 0 0
                argumentValue = Full.Node (Full.String "line1\nline2") location
                arguments = [Full.Argument "message" argumentValue location]
                field = Full.Field Nothing "field" arguments [] [] location
                fieldSelection = pure $ Full.FieldSelection field
                operation = Full.DefinitionOperation
                    $ Full.SelectionSet fieldSelection location
                expected = "{\n\
                    \  field(message: \"\"\"\n\
                    \    line1\n\
                    \    line2\n\
                    \  \"\"\")\n\
                    \}\n"
                actual = definition pretty operation
             in actual `shouldBe` expected

    describe "operationType" $
        it "produces lowercase mutation operation type" $
            let actual = operationType pretty Full.Mutation
             in actual `shouldBe` "mutation"

    describe "typeSystemDefinition" $ do
        it "produces a schema with an indented operation type definition" $
            let queryType = Full.OperationTypeDefinition Full.Query "QueryRootType"
                mutationType = Full.OperationTypeDefinition Full.Mutation "MutationType"
                operations = queryType :| pure mutationType
                definition' = Full.SchemaDefinition [] operations
                expected = "schema {\n\
                    \  query: QueryRootType\n\
                    \  mutation: MutationType\n\
                    \}\n"
                actual = typeSystemDefinition pretty definition'
             in actual `shouldBe` expected

        it "encodes a scalar type definition" $
            let uuidType = Full.ScalarTypeDefinition mempty "UUID" mempty
                definition' = Full.TypeDefinition uuidType
                expected = "scalar UUID"
                actual = typeSystemDefinition pretty definition'
             in actual `shouldBe` expected

        it "encodes an interface definition" $
            let someType = Full.TypeNamed "String"
                argument = Full.InputValueDefinition mempty "arg" someType Nothing mempty
                arguments = Full.ArgumentsDefinition [argument]
                definition' = Full.TypeDefinition
                    $ Full.InterfaceTypeDefinition mempty "UUID" (Full.ImplementsInterfaces []) mempty
                    $ pure
                    $ Full.FieldDefinition mempty "value" arguments someType mempty
                expected = "interface UUID {\n\
                    \  value(arg: String): String\n\
                    \}"
                actual = typeSystemDefinition pretty definition'
             in actual `shouldBe` expected

        it "encodes an union definition" $
            let definition' = Full.TypeDefinition
                    $ Full.UnionTypeDefinition mempty "SearchResult" mempty
                    $ Full.UnionMemberTypes ["Photo", "Person"]
                expected = "union SearchResult =\n\
                    \  | Photo\n\
                    \  | Person"
                actual = typeSystemDefinition pretty definition'
             in actual `shouldBe` expected

        it "encodes an enum definition" $
            let values =
                    [ Full.EnumValueDefinition mempty "NORTH" mempty
                    , Full.EnumValueDefinition mempty "EAST" mempty
                    , Full.EnumValueDefinition mempty "SOUTH" mempty
                    , Full.EnumValueDefinition mempty "WEST" mempty
                    ]
                definition' = Full.TypeDefinition
                    $ Full.EnumTypeDefinition mempty "Direction" mempty values
                expected = "enum Direction {\n\
                    \  NORTH\n\
                    \  EAST\n\
                    \  SOUTH\n\
                    \  WEST\n\
                    \}"
                actual = typeSystemDefinition pretty definition'
             in actual `shouldBe` expected

        it "encodes an input type" $
            let intType = Full.TypeNonNull $ Full.NonNullTypeNamed "Int"
                stringType = Full.TypeNamed "String"
                fields =
                    [ Full.InputValueDefinition mempty "a" stringType Nothing mempty
                    , Full.InputValueDefinition mempty "b" intType Nothing mempty
                    ]
                definition' = Full.TypeDefinition
                    $ Full.InputObjectTypeDefinition mempty "ExampleInputObject" mempty fields
                expected = "input ExampleInputObject {\n\
                    \  a: String\n\
                    \  b: Int!\n\
                    \}"
                actual = typeSystemDefinition pretty definition'
             in actual `shouldBe` expected

        context "directive definition" $ do
            it "encodes a directive definition" $ do
                let definition' = Full.DirectiveDefinition mempty "example" mempty False
                        $ pure
                        $ DirectiveLocation.ExecutableDirectiveLocation DirectiveLocation.Field
                    expected = "@example() on\n\
                        \  | FIELD"
                    actual = typeSystemDefinition pretty definition'
                 in actual `shouldBe` expected

            it "encodes a repeatable directive definition" $ do
                let definition' = Full.DirectiveDefinition mempty "example" mempty True
                        $ pure
                        $ DirectiveLocation.ExecutableDirectiveLocation DirectiveLocation.Field
                    expected = "@example() repeatable on\n\
                        \  | FIELD"
                    actual = typeSystemDefinition pretty definition'
                 in actual `shouldBe` expected
