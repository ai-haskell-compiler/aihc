{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}

module Language.GraphQL.Validate.RulesSpec
    ( spec
    ) where

import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Language.GraphQL.AST as AST
import Language.GraphQL.Type
import qualified Language.GraphQL.Type.Schema as Schema
import qualified Language.GraphQL.AST.DirectiveLocation as DirectiveLocation
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Validate
import Test.Hspec (Spec, context, describe, it, shouldBe, shouldContain)
import Text.Megaparsec (parse, errorBundlePretty)

petSchema :: Schema IO
petSchema = schema queryType Nothing (Just subscriptionType)
    $ HashMap.singleton "repeat"
    $ Schema.Directive Nothing mempty True [DirectiveLocation.ExecutableDirectiveLocation DirectiveLocation.Field]

queryType :: ObjectType IO
queryType = ObjectType "Query" Nothing [] $ HashMap.fromList
    [ ("dog", dogResolver)
    , ("cat", catResolver)
    , ("findDog", findDogResolver)
    , ("findCats", findCatsResolver)
    ]
  where
    dogField = Field Nothing (Out.NamedObjectType dogType) mempty
    dogResolver = ValueResolver dogField $ pure Null
    findDogArguments = HashMap.singleton "complex"
        $ In.Argument Nothing (In.NonNullInputObjectType dogDataType) Nothing
    findDogField = Field Nothing (Out.NamedObjectType dogType) findDogArguments
    findDogResolver = ValueResolver findDogField $ pure Null
    catField = Field Nothing (Out.NamedObjectType catType) mempty
    catResolver = ValueResolver catField $ pure Null
    findCatsArguments = HashMap.singleton "commands"
        $ In.Argument Nothing (In.NonNullListType $ In.NonNullEnumType catCommandType)
        $ Just $ List []
    findCatsField = Field Nothing (Out.NonNullListType $ Out.NonNullObjectType catType) findCatsArguments
    findCatsResolver = ValueResolver findCatsField $ pure $ List []

catCommandType :: EnumType
catCommandType = EnumType "CatCommand" Nothing $ HashMap.fromList
    [ ("JUMP", EnumValue Nothing)
    ]

catType :: ObjectType IO
catType = ObjectType "Cat" Nothing [petType] $ HashMap.fromList
    [ ("name", nameResolver)
    , ("nickname", nicknameResolver)
    , ("doesKnowCommands", doesKnowCommandsResolver)
    , ("meowVolume", meowVolumeResolver)
    ]
  where
    meowVolumeField = Field Nothing (Out.NamedScalarType int) mempty
    meowVolumeResolver = ValueResolver meowVolumeField $ pure $ Int 3
    doesKnowCommandsType = In.NonNullListType
        $ In.NonNullEnumType catCommandType
    doesKnowCommandsField = Field Nothing (Out.NonNullScalarType boolean)
        $ HashMap.singleton "catCommands"
        $ In.Argument Nothing doesKnowCommandsType Nothing
    doesKnowCommandsResolver = ValueResolver doesKnowCommandsField
        $ pure $ Boolean True

nameResolver :: Resolver IO
nameResolver = ValueResolver nameField $ pure "Name"
  where
    nameField = Field Nothing (Out.NonNullScalarType string) mempty

nicknameResolver :: Resolver IO
nicknameResolver = ValueResolver nicknameField $ pure "Nickname"
  where
    nicknameField = Field Nothing (Out.NamedScalarType string) mempty

dogCommandType :: EnumType
dogCommandType = EnumType "DogCommand" Nothing $ HashMap.fromList
    [ ("SIT", EnumValue Nothing)
    , ("DOWN", EnumValue Nothing)
    , ("HEEL", EnumValue Nothing)
    ]

dogType :: ObjectType IO
dogType = ObjectType "Dog" Nothing [petType] $ HashMap.fromList
    [ ("name", nameResolver)
    , ("nickname", nicknameResolver)
    , ("barkVolume", barkVolumeResolver)
    , ("doesKnowCommand", doesKnowCommandResolver)
    , ("isHouseTrained", isHouseTrainedResolver)
    , ("owner", ownerResolver)
    ]
  where
    barkVolumeField = Field Nothing (Out.NamedScalarType int) mempty
    barkVolumeResolver = ValueResolver barkVolumeField $ pure $ Int 3
    doesKnowCommandField = Field Nothing (Out.NonNullScalarType boolean)
        $ HashMap.singleton "dogCommand"
        $ In.Argument Nothing (In.NonNullEnumType dogCommandType) Nothing
    doesKnowCommandResolver = ValueResolver doesKnowCommandField
        $ pure $ Boolean True
    isHouseTrainedField = Field Nothing (Out.NonNullScalarType boolean)
        $ HashMap.singleton "atOtherHomes"
        $ In.Argument Nothing (In.NamedScalarType boolean) Nothing
    isHouseTrainedResolver = ValueResolver isHouseTrainedField
        $ pure $ Boolean True
    ownerField = Field Nothing (Out.NamedObjectType humanType) mempty
    ownerResolver = ValueResolver ownerField $ pure Null

dogDataType :: InputObjectType
dogDataType = InputObjectType "DogData" Nothing
    $ HashMap.singleton "name" nameInputField
  where
    nameInputField = InputField Nothing (In.NonNullScalarType string) Nothing

sentientType :: InterfaceType IO
sentientType = InterfaceType "Sentient" Nothing []
    $ HashMap.singleton "name"
    $ Field Nothing (Out.NonNullScalarType string) mempty

petType :: InterfaceType IO
petType = InterfaceType "Pet" Nothing []
    $ HashMap.singleton "name"
    $ Field Nothing (Out.NonNullScalarType string) mempty

subscriptionType :: ObjectType IO
subscriptionType = ObjectType "Subscription" Nothing [] $ HashMap.fromList
    [ ("newMessage", newMessageResolver)
    , ("disallowedSecondRootField", newMessageResolver)
    ]
  where
    newMessageField = Field Nothing (Out.NonNullObjectType messageType) mempty
    newMessageResolver = ValueResolver newMessageField
        $ pure $ Object HashMap.empty

messageType :: ObjectType IO
messageType = ObjectType "Message" Nothing [] $ HashMap.fromList
    [ ("sender", senderResolver)
    , ("body", bodyResolver)
    ]
  where
    senderField = Field Nothing (Out.NonNullScalarType string) mempty
    senderResolver = ValueResolver senderField $ pure "Sender"
    bodyField = Field Nothing (Out.NonNullScalarType string) mempty
    bodyResolver = ValueResolver bodyField $ pure "Message body."

humanType :: ObjectType IO
humanType = ObjectType "Human" Nothing [sentientType] $ HashMap.fromList
    [ ("name", nameResolver)
    , ("pets", petsResolver)
    ]
  where
    petsField =
        Field Nothing (Out.ListType $ Out.NonNullInterfaceType petType) mempty
    petsResolver = ValueResolver petsField $ pure $ List []

validate :: Text -> [Error]
validate queryString =
    case parse AST.document "" queryString of
        Left parseErrors -> error $ errorBundlePretty parseErrors
        Right ast -> toList $ document petSchema specifiedRules ast

spec :: Spec
spec =
    describe "document" $ do
        context "executableDefinitionsRule" $
            it "rejects type definitions" $
                let queryString = "query getDogName {\n\
                        \  dog {\n\
                        \    name\n\
                        \    color\n\
                        \  }\n\
                        \}\n\
                        \\n\
                        \extend type Dog {\n\
                        \  color: String\n\
                        \}"
                    expected = Error
                        { message =
                            "Definition must be OperationDefinition or \
                            \FragmentDefinition."
                        , locations = [AST.Location 8 1]
                        }
                 in validate queryString `shouldContain` [expected]

        context "singleFieldSubscriptionsRule" $ do
            it "rejects multiple subscription root fields" $
                let queryString = "subscription sub {\n\
                        \  newMessage {\n\
                        \    body\n\
                        \    sender\n\
                        \  }\n\
                        \  disallowedSecondRootField\n\
                        \}"
                    expected = Error
                        { message =
                            "Subscription \"sub\" must select only one top \
                            \level field."
                        , locations = [AST.Location 1 1]
                        }
                 in validate queryString `shouldContain` [expected]

            it "rejects an introspection field as the subscription root" $
                let queryString = "subscription sub {\n\
                        \  __typename\n\
                        \}"
                    expected = Error
                        { message =
                            "Subscription \"sub\" must select exactly one top \
                            \level field, which must not be an introspection field."
                        , locations = [AST.Location 1 1]
                        }
                 in validate queryString `shouldContain` [expected]

            it "rejects multiple subscription root fields coming from a fragment" $
                let queryString = "subscription sub {\n\
                        \  ...multipleSubscriptions\n\
                        \}\n\
                        \\n\
                        \fragment multipleSubscriptions on Subscription {\n\
                        \  newMessage {\n\
                        \    body\n\
                        \    sender\n\
                        \  }\n\
                        \  disallowedSecondRootField\n\
                        \}"
                    expected = Error
                        { message =
                            "Subscription \"sub\" must select only one top \
                            \level field."
                        , locations = [AST.Location 1 1]
                        }
                 in validate queryString `shouldContain` [expected]

            it "finds corresponding subscription fragment" $
                let queryString = "subscription sub {\n\
                        \  ...anotherSubscription\n\
                        \  ...multipleSubscriptions\n\
                        \}\n\
                        \fragment multipleSubscriptions on Subscription {\n\
                        \  newMessage {\n\
                        \    body\n\
                        \  }\n\
                        \  disallowedSecondRootField {\n\
                        \    sender\n\
                        \  }\n\
                        \}\n\
                        \fragment anotherSubscription on Subscription {\n\
                        \  newMessage {\n\
                        \    body\n\
                        \    sender\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Subscription \"sub\" must select only one top \
                            \level field."
                        , locations = [AST.Location 1 1]
                        }
                 in validate queryString `shouldBe` [expected]

        context "loneAnonymousOperationRule" $
            it "rejects multiple anonymous operations" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    name\n\
                        \  }\n\
                        \}\n\
                        \\n\
                        \query getName {\n\
                        \  dog {\n\
                        \    owner {\n\
                        \      name\n\
                        \    }\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "This anonymous operation must be the only defined \
                            \operation."
                        , locations = [AST.Location 1 1]
                        }
                 in validate queryString `shouldBe` [expected]

        context "uniqueOperationNamesRule" $
            it "rejects operations with the same name" $
                let queryString = "query dogOperation {\n\
                        \  dog {\n\
                        \    name\n\
                        \  }\n\
                        \}\n\
                        \\n\
                        \mutation dogOperation {\n\
                        \  mutateDog {\n\
                        \    id\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "There can be only one operation named \
                            \\"dogOperation\"."
                        , locations = [AST.Location 1 1, AST.Location 7 1]
                        }
                 in validate queryString `shouldBe` [expected]

        context "uniqueFragmentNamesRule" $
            it "rejects fragments with the same name" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    ...fragmentOne\n\
                        \  }\n\
                        \}\n\
                        \\n\
                        \fragment fragmentOne on Dog {\n\
                        \  name\n\
                        \}\n\
                        \\n\
                        \fragment fragmentOne on Dog {\n\
                        \  owner {\n\
                        \    name\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "There can be only one fragment named \
                            \\"fragmentOne\"."
                        , locations = [AST.Location 7 1, AST.Location 11 1]
                        }
                 in validate queryString `shouldBe` [expected]

        context "fragmentSpreadTargetDefinedRule" $
            it "rejects the fragment spread without a target" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    ...undefinedFragment\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Fragment target \"undefinedFragment\" is \
                            \undefined."
                        , locations = [AST.Location 3 5]
                        }
                 in validate queryString `shouldBe` [expected]

        context "fragmentSpreadTypeExistenceRule" $ do
            it "rejects fragment spreads without an unknown target type" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    ...notOnExistingType\n\
                        \  }\n\
                        \}\n\
                        \fragment notOnExistingType on NotInSchema {\n\
                        \  name\n\
                        \}"
                    expected = Error
                        { message =
                            "Fragment \"notOnExistingType\" is specified on \
                            \type \"NotInSchema\" which doesn't exist in the \
                            \schema."
                        , locations = [AST.Location 3 5]
                        }
                 in validate queryString `shouldBe` [expected]

            it "rejects inline fragments without a target" $
                let queryString = "{\n\
                        \  ... on NotInSchema {\n\
                        \    name\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Inline fragment is specified on type \
                            \\"NotInSchema\" which doesn't exist in the schema."
                        , locations = [AST.Location 2 3]
                        }
                 in validate queryString `shouldBe` [expected]

        context "fragmentsOnCompositeTypesRule" $ do
            it "rejects fragments on scalar types" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    ...fragOnScalar\n\
                        \  }\n\
                        \}\n\
                        \fragment fragOnScalar on Int {\n\
                        \  name\n\
                        \}"
                    expected = Error
                        { message =
                            "Fragment cannot condition on non composite type \
                            \\"Int\"."
                        , locations = [AST.Location 6 1]
                        }
                 in validate queryString `shouldContain` [expected]

            it "rejects inline fragments on scalar types" $
                let queryString = "{\n\
                        \  ... on Boolean {\n\
                        \    name\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Fragment cannot condition on non composite type \
                            \\"Boolean\"."
                        , locations = [AST.Location 2 3]
                        }
                in validate queryString `shouldContain` [expected]

        context "noUnusedFragmentsRule" $
            it "rejects unused fragments" $
                let queryString = "fragment nameFragment on Dog { # unused\n\
                        \  name\n\
                        \}\n\
                        \\n\
                        \{\n\
                        \  dog {\n\
                        \    name\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Fragment \"nameFragment\" is never used."
                        , locations = [AST.Location 1 1]
                        }
                 in validate queryString `shouldBe` [expected]

        context "noFragmentCyclesRule" $
            it "rejects spreads that form cycles" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    ...nameFragment\n\
                        \  }\n\
                        \}\n\
                        \fragment nameFragment on Dog {\n\
                        \  name\n\
                        \  ...barkVolumeFragment\n\
                        \}\n\
                        \fragment barkVolumeFragment on Dog {\n\
                        \  barkVolume\n\
                        \  ...nameFragment\n\
                        \}"
                    error1 = Error
                        { message =
                            "Cannot spread fragment \"barkVolumeFragment\" \
                            \within itself (via barkVolumeFragment -> \
                            \nameFragment -> barkVolumeFragment)."
                        , locations = [AST.Location 10 1]
                        }
                    error2 = Error
                        { message =
                            "Cannot spread fragment \"nameFragment\" within \
                            \itself (via nameFragment -> barkVolumeFragment -> \
                            \nameFragment)."
                        , locations = [AST.Location 6 1]
                        }
                 in validate queryString `shouldBe` [error1, error2]

        context "uniqueArgumentNamesRule" $
            it "rejects duplicate field arguments" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    isHouseTrained(atOtherHomes: true, atOtherHomes: true)\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "There can be only one argument named \
                            \\"atOtherHomes\"."
                        , locations = [AST.Location 3 20, AST.Location 3 40]
                        }
                 in validate queryString `shouldBe` [expected]

        context "uniqueDirectiveNamesRule" $ do
            it "rejects more than one directive per location" $
                let queryString = "query ($foo: Boolean = true, $bar: Boolean = false) {\n\
                        \  dog @skip(if: $foo) @skip(if: $bar) {\n\
                        \    name\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "There can be only one directive named \"skip\"."
                        , locations = [AST.Location 2 7, AST.Location 2 23]
                        }
                 in validate queryString `shouldBe` [expected]

            it "allows repeating repeatable directives" $
                let queryString = "query {\n\
                        \  dog @repeat @repeat {\n\
                        \    name\n\
                        \  }\n\
                        \}"
                 in validate queryString `shouldBe` []

        context "uniqueVariableNamesRule" $
            it "rejects duplicate variables" $
                let queryString = "query houseTrainedQuery($atOtherHomes: Boolean, $atOtherHomes: Boolean) {\n\
                        \  dog {\n\
                        \    isHouseTrained(atOtherHomes: $atOtherHomes)\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "There can be only one variable named \
                            \\"atOtherHomes\"."
                        , locations = [AST.Location 1 25, AST.Location 1 49]
                        }
                 in validate queryString `shouldBe` [expected]

        context "variablesAreInputTypesRule" $
            it "rejects non-input types as variables" $
                let queryString = "query takesDogBang($dog: Dog!) {\n\
                        \  dog {\n\
                        \    isHouseTrained(atOtherHomes: $dog)\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Variable \"$dog\" cannot be non-input type \
                            \\"Dog\"."
                        , locations = [AST.Location 1 20]
                        }
                 in validate queryString `shouldContain` [expected]

        context "noUndefinedVariablesRule" $ do
            it "rejects undefined variables" $
                let queryString = "query variableIsNotDefinedUsedInSingleFragment {\n\
                        \  dog {\n\
                        \    ...isHouseTrainedFragment\n\
                        \  }\n\
                        \}\n\
                        \\n\
                        \fragment isHouseTrainedFragment on Dog {\n\
                        \  isHouseTrained(atOtherHomes: $atOtherHomes)\n\
                        \}"
                    expected = Error
                        { message =
                            "Variable \"$atOtherHomes\" is not defined by \
                            \operation \
                            \\"variableIsNotDefinedUsedInSingleFragment\"."
                        , locations = [AST.Location 8 32]
                        }
                 in validate queryString `shouldBe` [expected]

            it "gets variable location inside an input object" $
                let queryString = "query {\n\
                        \  findDog (complex: { name: $name }) {\n\
                        \    name\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message = "Variable \"$name\" is not defined."
                        , locations = [AST.Location 2 29]
                        }
                 in validate queryString `shouldBe` [expected]

            it "gets variable location inside an array" $
                let queryString = "query {\n\
                        \  findCats (commands: [JUMP, $command]) {\n\
                        \    name\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message = "Variable \"$command\" is not defined."
                        , locations = [AST.Location 2 30]
                        }
                 in validate queryString `shouldBe` [expected]

        context "noUnusedVariablesRule" $ do
            it "rejects unused variables" $
                let queryString = "query variableUnused($atOtherHomes: Boolean) {\n\
                        \  dog {\n\
                        \    isHouseTrained\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Variable \"$atOtherHomes\" is never used in \
                            \operation \"variableUnused\"."
                        , locations = [AST.Location 1 22]
                        }
                 in validate queryString `shouldBe` [expected]

            it "detects variables in properties of input objects" $
                let queryString = "query withVar ($name: String!) {\n\
                        \  findDog (complex: { name: $name }) {\n\
                        \    name\n\
                        \  }\n\
                        \}"
                 in validate queryString `shouldBe` []

        context "uniqueInputFieldNamesRule" $
            it "rejects duplicate fields in input objects" $
                let queryString = "{\n\
                        \  findDog(complex: { name: \"Fido\", name: \"Jack\" }) {\n\
                        \    name\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "There can be only one input field named \"name\"."
                        , locations = [AST.Location 2 22, AST.Location 2 36]
                        }
                 in validate queryString `shouldBe` [expected]

        context "fieldsOnCorrectTypeRule" $
            it "rejects undefined fields" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    meowVolume\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Cannot query field \"meowVolume\" on type \"Dog\"."
                        , locations = [AST.Location 3 5]
                        }
                 in validate queryString `shouldBe` [expected]

        context "scalarLeafsRule" $
            it "rejects scalar fields with not empty selection set" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    barkVolume {\n\
                        \      sinceWhen\n\
                        \    }\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Field \"barkVolume\" must not have a selection \
                            \since type \"Int\" has no subfields."
                        , locations = [AST.Location 3 5]
                        }
                 in validate queryString `shouldBe` [expected]

        context "knownArgumentNamesRule" $ do
            it "rejects field arguments missing in the type" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    doesKnowCommand(command: CLEAN_UP_HOUSE, dogCommand: SIT)\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Unknown argument \"command\" on field \
                            \\"Dog.doesKnowCommand\"."
                        , locations = [AST.Location 3 21]
                        }
                 in validate queryString `shouldBe` [expected]

            it "rejects directive arguments missing in the definition" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    isHouseTrained(atOtherHomes: true) @include(unless: false, if: true)\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Unknown argument \"unless\" on directive \
                            \\"@include\"."
                        , locations = [AST.Location 3 49]
                        }
                 in validate queryString `shouldBe` [expected]

        context "knownDirectiveNamesRule" $
            it "rejects undefined directives" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    isHouseTrained(atOtherHomes: true) @ignore(if: true)\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message = "Unknown directive \"@ignore\"."
                        , locations = [AST.Location 3 40]
                        }
                 in validate queryString `shouldBe` [expected]

        context "knownInputFieldNamesRule" $
            it "rejects undefined input object fields" $
                let queryString = "{\n\
                        \  findDog(complex: { favoriteCookieFlavor: \"Bacon\", name: \"Jack\" }) {\n\
                        \    name\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Field \"favoriteCookieFlavor\" is not defined \
                            \by type \"DogData\"."
                        , locations = [AST.Location 2 22]
                        }
                 in validate queryString `shouldBe` [expected]

        context "directivesInValidLocationsRule" $
            it "rejects directives in invalid locations" $
                let queryString = "query @skip(if: $foo) {\n\
                        \  dog {\n\
                        \    name\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Directive \"@skip\" may not be used on QUERY."
                        , locations = [AST.Location 1 7]
                        }
                 in validate queryString `shouldBe` [expected]

        context "overlappingFieldsCanBeMergedRule" $ do
            it "fails to merge fields of mismatching types" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    name: nickname\n\
                        \    name\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Fields \"name\" conflict because \"nickname\" and \
                            \\"name\" are different fields. Use different \
                            \aliases on the fields to fetch both if this was \
                            \intentional."
                        , locations = [AST.Location 3 5, AST.Location 4 5]
                        }
                 in validate queryString `shouldBe` [expected]

            it "fails if the arguments of the same field don't match" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    doesKnowCommand(dogCommand: SIT)\n\
                        \    doesKnowCommand(dogCommand: HEEL)\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Fields \"doesKnowCommand\" conflict because they \
                            \have different arguments. Use different aliases \
                            \on the fields to fetch both if this was \
                            \intentional."
                        , locations = [AST.Location 3 5, AST.Location 4 5]
                        }
                 in validate queryString `shouldBe` [expected]

            it "fails to merge same-named field and alias" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    doesKnowCommand(dogCommand: SIT)\n\
                        \    doesKnowCommand: isHouseTrained(atOtherHomes: true)\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Fields \"doesKnowCommand\" conflict because \
                            \\"doesKnowCommand\" and \"isHouseTrained\" are \
                            \different fields. Use different aliases on the \
                            \fields to fetch both if this was intentional."
                        , locations = [AST.Location 3 5, AST.Location 4 5]
                        }
                 in validate queryString `shouldBe` [expected]

            it "looks for fields after a successfully merged field pair" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    name\n\
                        \    doesKnowCommand(dogCommand: SIT)\n\
                        \  }\n\
                        \  dog {\n\
                        \    name\n\
                        \    doesKnowCommand: isHouseTrained(atOtherHomes: true)\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Fields \"doesKnowCommand\" conflict because \
                            \\"doesKnowCommand\" and \"isHouseTrained\" are \
                            \different fields. Use different aliases on the \
                            \fields to fetch both if this was intentional."
                        , locations = [AST.Location 4 5, AST.Location 8 5]
                        }
                 in validate queryString `shouldBe` [expected]

        context "possibleFragmentSpreadsRule" $ do
            it "rejects object inline spreads outside object scope" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    ... on Cat {\n\
                        \      meowVolume\n\
                        \    }\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Fragment cannot be spread here as objects of type \
                            \\"Dog\" can never be of type \"Cat\"."
                        , locations = [AST.Location 3 5]
                        }
                 in validate queryString `shouldBe` [expected]

            it "rejects object named spreads outside object scope" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    ... catInDogFragmentInvalid\n\
                        \  }\n\
                        \}\n\
                        \\n\
                        \fragment catInDogFragmentInvalid on Cat {\n\
                        \  meowVolume\n\
                        \}"
                    expected = Error
                        { message =
                            "Fragment \"catInDogFragmentInvalid\" cannot be \
                            \spread here as objects of type \"Dog\" can never \
                            \be of type \"Cat\"."
                        , locations = [AST.Location 3 5]
                        }
                 in validate queryString `shouldBe` [expected]

        context "providedRequiredInputFieldsRule" $
            it "rejects missing required input fields" $
                let queryString = "{\n\
                        \  findDog(complex: { name: null }) {\n\
                        \    name\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Input field \"name\" of type \"DogData\" is \
                            \required, but it was not provided."
                        , locations = [AST.Location 2 20]
                        }
                 in validate queryString `shouldBe` [expected]

        context "providedRequiredArgumentsRule" $ do
            it "checks for (non-)nullable arguments" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    doesKnowCommand(dogCommand: null)\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Field \"doesKnowCommand\" argument \"dogCommand\" \
                            \of type \"DogCommand\" is required, but it was \
                            \not provided."
                        , locations = [AST.Location 3 5]
                        }
                 in validate queryString `shouldBe` [expected]

        context "variablesInAllowedPositionRule" $ do
            it "rejects wrongly typed variable arguments" $
                let queryString = "query dogCommandArgQuery($dogCommandArg: DogCommand) {\n\
                        \  dog {\n\
                        \    doesKnowCommand(dogCommand: $dogCommandArg)\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Variable \"$dogCommandArg\" of type \
                            \\"DogCommand\" used in position expecting type \
                            \\"DogCommand!\"."
                        , locations = [AST.Location 1 26]
                        }
                 in validate queryString `shouldBe` [expected]

            it "rejects wrongly typed variable arguments" $
                let queryString = "query intCannotGoIntoBoolean($intArg: Int) {\n\
                        \  dog {\n\
                        \    isHouseTrained(atOtherHomes: $intArg)\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Variable \"$intArg\" of type \"Int\" used in \
                            \position expecting type \"Boolean\"."
                        , locations = [AST.Location 1 30]
                        }
                 in validate queryString `shouldBe` [expected]

        context "valuesOfCorrectTypeRule" $ do
            it "rejects values of incorrect types" $
                let queryString = "{\n\
                        \  dog {\n\
                        \    isHouseTrained(atOtherHomes: 3)\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Value 3 cannot be coerced to type \"Boolean\"."
                        , locations = [AST.Location 3 34]
                        }
                 in validate queryString `shouldBe` [expected]

            it "uses the location of a single list value" $
                let queryString = "{\n\
                        \  cat {\n\
                        \    doesKnowCommands(catCommands: [3])\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Value 3 cannot be coerced to type \"CatCommand!\"."
                        , locations = [AST.Location 3 36]
                        }
                 in validate queryString `shouldBe` [expected]

            it "validates input object properties once" $
                let queryString = "{\n\
                        \  findDog(complex: { name: 3 }) {\n\
                        \    name\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "Value 3 cannot be coerced to type \"String!\"."
                        , locations = [AST.Location 2 28]
                        }
                 in validate queryString `shouldBe` [expected]

            it "checks for required list members" $
                let queryString = "{\n\
                        \  cat {\n\
                        \    doesKnowCommands(catCommands: [null])\n\
                        \  }\n\
                        \}"
                    expected = Error
                        { message =
                            "List of non-null values of type \"CatCommand\" \
                            \cannot contain null values."
                        , locations = [AST.Location 3 36]
                        }
                 in validate queryString `shouldBe` [expected]
