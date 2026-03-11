# json-spec

## Motivation

The primary motivation is to allow you to avoid Aeson Generic instances
while still getting the possibility of auto-generated (and therefore
/correct/) documentation and code in your servant APIs.

Historically, the trade-off has been:

1. Use Generic instances, and therefore your API is brittle. Changes
   to a deeply nested object might unexpectedly change (and break) your
   API. You must structure your Haskell types exactly as they are
   rendered into JSON, which may not always be "natural" and easy to
   work with. In exchange, you get the ability to auto-derive matching
   ToSchema instances along with various code generation tools that
   all understand Aeson Generic instances.

2. Hand-write your ToJSON and FromJSON instances, which means you
   get to structure your Haskell types in the way that works best
   for Haskell, while structuring your JSON in the way that works
   best for your API. It also means you can more easily support "old"
   decoding versions and more easily maintain backwards compatibility,
   etc. In exchange, you have to to hand-write your ToSchema instances,
   and code generation is basically out.

The goal of this library is to provide a way to hand-write the encoding
and decoding of your JSON using type-level 'Specification's, while
still allowing the use of tools that can interpret the specification
and auto-generate ToSchema instances and code.

The tooling ecosystem that knows how to interpret 'Specification's
is still pretty new, but it at least includes OpenApi compatibility
(i.e. ToSchema instances) and Elm code generation.

## Example

```haskell
data User = User
  { name :: Text
  , lastLogin :: UTCTime
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON User)
instance HasJsonEncodingSpec User where
  type EncodingSpec User =
    JsonObject '[
      Required "name" JsonString,
      Required "last-login" JsonDateTime
    ]
  toJSONStructure user =
    (Field @"name" (name user),
    (Field @"last-login" (lastLogin user),
    ()))
instance HasJsonDecodingSpec User where
  type DecodingSpec User = EncodingSpec User
  fromJSONStructure
      (Field @"name" name,
      (Field @"last-login" lastLogin,
      ()))
    =
      pure User { name , lastLogin }
```

For more examples, take a look at the test suite.
