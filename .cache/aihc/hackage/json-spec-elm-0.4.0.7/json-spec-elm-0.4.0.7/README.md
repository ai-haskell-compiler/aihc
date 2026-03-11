# json-spec-elm

Produce elm types, encoders, and decoders from a
[`json-spec`](https://hackage.haskell.org/package/json-spec) `Specification`.

See `/test/test.hs` for an example.

# Example

First let's define an example spec.

A couple of things to note:

1. Only things that are named using `JsonLet` will get elm types. (Also
   note, `Named` is just an alias for `JsonLet`.) So you will probably
   want to name the top-level of your spec at least.
2. `JsonEither` spec types _must_ be named. Elm can support anonymous
   record types but not anonymous sum types, so there is no way to embed
   an anonymous `JsonEither`. You have to give it a name.
3. Naming the constructors for sum types is a little tricky. If a branch
   of `JsonEither` is given a name (using `Named`), then we interpret
   the name as the name of the data constructor, not the name of the
   type contained within the branch.  To name both the data constructor
   and the type, you must use nested `Named`s.

```
type ExampleSpec =
  Named "ExampleType"
    ( JsonObject
        '[ '("stringField", JsonString)
         , '( "anonymousObject"
            , JsonObject
                '[ '("floatField", JsonNum)
                 , '("dateField", JsonDateTime)
                 , '( "sumType1"
                    , Named "SumTypeWithCustomConstructorNames"
                        ( JsonEither
                            ( JsonEither
                                (Named "IntConstructor" JsonInt)
                                (Named "StringConstructor" JsonString)
                            )
                            (Named "FloatConstructor" JsonNum)
                        )
                    )
                 , '( "sumType2"
                    , Named "SumTypeWithAutomaticConstructorNames"
                        ( JsonEither
                            ( JsonEither
                                JsonInt
                                JsonString
                            )
                            JsonNum
                        )
                    )
                 ]
            )
         , '( "namedObject"
            , Named "NamedElmRecord"
                ( JsonObject
                    '[ '("stringField", JsonString)
                     , '( "listOfStrings"
                        , JsonArray JsonString
                        )
                     ]
                )
            )
         ]
    )
```

This spec will produce the following code (after running it through
`elm-format`):

```
module Api.Data exposing
  ( ExampleType
  , NamedElmRecord
  , SumTypeWithAutomaticConstructorNames(..)
  , SumTypeWithCustomConstructorNames(..)
  , exampleTypeDecoder
  , exampleTypeEncoder
  , namedElmRecordDecoder
  , namedElmRecordEncoder
  , sumTypeWithAutomaticConstructorNamesDecoder
  , sumTypeWithAutomaticConstructorNamesEncoder
  , sumTypeWithCustomConstructorNamesDecoder
  , sumTypeWithCustomConstructorNamesEncoder
  )

import Iso8601
import Json.Decode
import Json.Encode
import Time


exampleTypeDecoder : Json.Decode.Decoder ExampleType
exampleTypeDecoder =
  Json.Decode.succeed
    (\a b c ->
      { stringField = a
      , anonymousObject = b
      , namedObject = c
      }
    )
    |> Json.Decode.andThen (\a -> Json.Decode.map a Json.Decode.string)
    |> Json.Decode.andThen
        (\a ->
          Json.Decode.map a
            (Json.Decode.succeed
              (\b c d e ->
                { floatField = b
                , dateField = c
                , sumType1 = d
                , sumType2 = e
                }
              )
              |> Json.Decode.andThen (\b -> Json.Decode.map b Json.Decode.float)
              |> Json.Decode.andThen (\b -> Json.Decode.map b Iso8601.decoder)
              |> Json.Decode.andThen (\b -> Json.Decode.map b sumTypeWithCustomConstructorNamesDecoder)
              |> Json.Decode.andThen (\b -> Json.Decode.map b sumTypeWithAutomaticConstructorNamesDecoder)
            )
        )
    |> Json.Decode.andThen (\a -> Json.Decode.map a namedElmRecordDecoder)


exampleTypeEncoder : ExampleType -> Json.Encode.Value
exampleTypeEncoder a =
  Json.Encode.object
    [ ( "stringField", Json.Encode.string a.stringField )
    , ( "anonymousObject"
      , (\b ->
          Json.Encode.object
            [ ( "floatField", Json.Encode.float b.floatField )
            , ( "dateField", Iso8601.encode b.dateField )
            , ( "sumType1", sumTypeWithCustomConstructorNamesEncoder b.sumType1 )
            , ( "sumType2", sumTypeWithAutomaticConstructorNamesEncoder b.sumType2 )
            ]
        )
          a.anonymousObject
      )
    , ( "namedObject", namedElmRecordEncoder a.namedObject )
    ]


namedElmRecordDecoder : Json.Decode.Decoder NamedElmRecord
namedElmRecordDecoder =
  Json.Decode.succeed (\a b -> { stringField = a, listOfStrings = b })
    |> Json.Decode.andThen (\a -> Json.Decode.map a Json.Decode.string)
    |> Json.Decode.andThen (\a -> Json.Decode.map a (Json.Decode.list Json.Decode.string))


namedElmRecordEncoder : NamedElmRecord -> Json.Encode.Value
namedElmRecordEncoder a =
  Json.Encode.object
    [ ( "stringField", Json.Encode.string a.stringField )
    , ( "listOfStrings", Json.Encode.list Json.Encode.string a.listOfStrings )
    ]


sumTypeWithAutomaticConstructorNamesDecoder : Json.Decode.Decoder SumTypeWithAutomaticConstructorNames
sumTypeWithAutomaticConstructorNamesDecoder =
  Json.Decode.oneOf
    [ Json.Decode.map SumTypeWithAutomaticConstructorNames_1 Json.Decode.int
    , Json.Decode.map SumTypeWithAutomaticConstructorNames_2 Json.Decode.string
    , Json.Decode.map SumTypeWithAutomaticConstructorNames_3 Json.Decode.float
    ]


sumTypeWithAutomaticConstructorNamesEncoder : SumTypeWithAutomaticConstructorNames -> Json.Encode.Value
sumTypeWithAutomaticConstructorNamesEncoder a =
  case a of
    SumTypeWithAutomaticConstructorNames_1 b ->
      Json.Encode.int b

    SumTypeWithAutomaticConstructorNames_2 b ->
      Json.Encode.string b

    SumTypeWithAutomaticConstructorNames_3 b ->
      Json.Encode.float b


sumTypeWithCustomConstructorNamesDecoder : Json.Decode.Decoder SumTypeWithCustomConstructorNames
sumTypeWithCustomConstructorNamesDecoder =
  Json.Decode.oneOf
    [ Json.Decode.map IntConstructor Json.Decode.int
    , Json.Decode.map StringConstructor Json.Decode.string
    , Json.Decode.map FloatConstructor Json.Decode.float
    ]


sumTypeWithCustomConstructorNamesEncoder : SumTypeWithCustomConstructorNames -> Json.Encode.Value
sumTypeWithCustomConstructorNamesEncoder a =
  case a of
    IntConstructor b ->
      Json.Encode.int b

    StringConstructor b ->
      Json.Encode.string b

    FloatConstructor b ->
      Json.Encode.float b


type SumTypeWithAutomaticConstructorNames
  = SumTypeWithAutomaticConstructorNames_1 Int
  | SumTypeWithAutomaticConstructorNames_2 String
  | SumTypeWithAutomaticConstructorNames_3 Float


type SumTypeWithCustomConstructorNames
  = IntConstructor Int
  | StringConstructor String
  | FloatConstructor Float


type alias ExampleType =
  { stringField : String
  , anonymousObject :
      { floatField : Float
      , dateField : Time.Posix
      , sumType1 : SumTypeWithCustomConstructorNames
      , sumType2 : SumTypeWithAutomaticConstructorNames
      }
  , namedObject : NamedElmRecord
  }


type alias NamedElmRecord =
  { stringField : String, listOfStrings : List String }
```

## Generating code

The main function exposed by this module is `elmDefs`, which returns
a `Set Definition` (where `Definition` is from the `elm-syntax`
package). For examples on how to transform a `Definition` into some
files on disk, see `/test/test.hs`.


