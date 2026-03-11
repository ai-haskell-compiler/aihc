# `registry-aeson`

##### *It's functions all the way down* <img src="doc/images/unboxed-bottomup.jpg" border="0"/>

### Presentation

This library is an add-on to [`registry`](https://github.com/etorreborre/registry), providing customizable encoders / decoders for [Aeson](https://hackage.haskell.org/package/aeson).

The approach taken is to add to a registry a list of functions taking encoders / decoders as parameters and producing encoders / decoders.
Then `registry` is able to assemble all the functions required to make an `Encoder` or a `Decoder` of a given type if the encoders or decoders for its dependencies can
be made out of the registry.

By doing so we get all the advantages from using `registry`:

 - we can override the `aeson` `Options` for either a whole graph of data types or just one data type
 - we can easily provide a different encodings / decodings for one data type in a specific context (a `Date` can be formatted differently if it is a birth date or an acquisition date for example)
 - we can define incremental evolutions of an API, all mapping to the same underlying data model

### Encoders

#### Example

Here is an example of creating encoders for a set of related data types:
```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import Data.Aeson
import Data.Registry
import Data.Registry.Aeson.Encoder
import Data.Time
import Protolude

newtype Identifier = Identifier Int
newtype Email = Email { _email :: Text }
newtype DateTime = DateTime { _datetime :: UTCTime }
data Person = Person { identifier :: Identifier, email :: Email }

data Delivery =
    NoDelivery
  | ByEmail Email
  | InPerson Person DateTime

encoders :: Registry _ _
encoders =
  $(makeEncoder ''Delivery)
  <: $(makeEncoder ''Person)
  <: $(makeEncoder ''Email)
  <: $(makeEncoder ''Identifier)
  <: fun datetimeEncoder
  <: jsonEncoder @Text
  <: jsonEncoder @Int
  <: defaultEncoderOptions

datetimeEncoder :: Encoder DateTime
datetimeEncoder = fromValue $ \(DateTime dt) -> do
  let formatted = toS $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" dt
  Object [("_datetime", String formatted)]
```

In the code above most encoders are created with `TemplateHaskell` and the `makeEncoder` function. The other encoders are either:

 - created manually: `dateTimeEncoder` (note that this encoder needs to be added to the registry with `fun`)
 - retrieved from a `Aeson` instance: `jsonEncoder @Text`, `jsonEncoder @Int`

Given the list of `encoders` an `Encoder Person` can be retrieved with:
```haskell
let encoderPerson = make @(Encoder Person) encoders
let encoded = encodeValue encoderPerson (Person (Identifier 123) (Email "me@here.com")) :: Value
```

#### Generated encoders

The `makeEncoder` function uses the `defaultOptions` added to the registry to produce the same values that a `Generic` `ToJSON` instance would produce.

__NOTE__ this function does not support recursive data types (and much less mutually recursive data types)

### Decoders

#### Example

Here is an example of creating decoders for a set of related data types:
```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import Data.Aeson
import Data.Registry
import Data.Registry.Aeson.Decoder
import Data.Time
import Protolude

newtype Identifier = Identifier Int
newtype Email = Email { _email :: Text }
newtype DateTime = DateTime { _datetime :: UTCTime }
data Person = Person { identifier :: Identifier, email :: Email }

data Delivery =
    NoDelivery
  | ByEmail Email
  | InPerson Person DateTime

decoders :: Registry _ _
decoders =
  $(makeDecoder ''Delivery)
  <: $(makeDecoder ''Person)
  <: $(makeDecoder ''Email)
  <: $(makeDecoder ''Identifier)
  <: fun dateTimeDecoder
  <: jsonDecoder @Text
  <: jsonDecoder @Int
  <: defaultDecoderOptions

datetimeDecoder :: Decoder DateTime
datetimeDecoder = Decoder $ \case
  String s ->
    case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" $ toS s of
      Just t -> pure (DateTime t)
      Nothing -> Left ("cannot read a DateTime: " <> s)
  other -> Left $ "not a valid DateTime: " <> show other
```

In the code above most decoders are created with `TemplateHaskell` and the `makeDecoder` function. The other decoders are either:

 - created manually: `dateTimeDecoder` (note that this decoder needs to be added to the registry with `fun`)
 - retrieved from a `Aeson` instance: `jsonDecoder @Text`, `jsonDecoder @Int`

Given the list of `Decoders` an `Decoder Person` can be retrieved with:
```haskell
let decoderPerson = make @(Decoder Person) decoders
let decoded = decode decoderPerson $ ObjectArray [Number 123, ObjectStr "me@here.com"]
```

##### Overriding the generated encoders

There is a bit of flexibility in the way encoders are created with TemplateHaskell.

A custom `ConstructorsEncoder` can be added to the registry to tweak the generation:
```haskell
newtype ConstructorEncoder = ConstructorEncoder
  { encodeConstructor :: Options -> FromConstructor -> (Value, Encoding)
  }
```
A `ConstructorEncoder` uses configuration options and type information extracted from
given data type (with TemplateHaskell) in order to produce a `Value` and an `Encoding`.

If necessary you can provide your own options and reuse the default function to produce different encoders.

#### Generated decoders

The `makeDecoder` function makes the following functions:
```haskell
-- makeDecoder ''Identifier
\(d::Decoder Int) -> Decoder $ \o -> Identifier <$> decode d o

-- makeDecoder ''Email
\(d::Decoder Text) -> Decoder $ \o -> Email <$> decode d o

-- makeDecoder ''Person
\(d1::Decoder Identifier) (d2::Decoder Email) -> Decoder $ \case
  ObjectArray [o1, o2] -> Person <$> decode d1 o1 <*> decode d2 o2
  other -> Error ("not a valid Person: " <> show other)

-- makeDecoder ''Delivery
\(d1::Decoder Email) (d2::Decoder Person) (d3::Decoder DateTime) -> Decoder $ \case
  ObjectArray [Number 0] -> pure NoDelivery
  ObjectArray [Number 1, o1] -> ByEmail <$> decode d1 o1
  ObjectArray [Number 2, o1, o2] -> InPerson <$> decode d1 o1 <*> decode d2 o2
  other -> Error ("not a valid Delivery: " <> show other)
```

__NOTE__ this function does not support recursive data types (and much less mutually recursive data types)

##### Overriding the generated decoders

There is a bit of flexibility in the way decoders are created with TemplateHaskell.

A custom `ConstructorsDecoder` can be added to the registry to tweak the generation:
```haskell
newtype ConstructorsDecoder = ConstructorsDecoder
  { decodeConstructors :: Options -> [ConstructorDef] -> Value -> Either Text [ToConstructor]
  }
```
This function extracts values for a set of constructor definitions and returns `ToConstructor` values
containing a JSON `Value` to be decoded for each field of a given constructor (along with its name).

If necessary you can provide your own options and reuse the default function to produce different decoders.
