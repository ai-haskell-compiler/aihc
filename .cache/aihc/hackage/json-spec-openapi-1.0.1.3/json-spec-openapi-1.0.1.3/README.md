# json-spec-openapi

This package provides a way to produce
[`openapi3`](https://hackage.haskell.org/package/openapi3) documentation from a
[`json-spec`](https://hackage.haskell.org/package/json-spec-0.1.0.0)
specification.

## Example

Given this data type:

```haskell
data User = User
  {      name :: Text
  , lastLogin :: Maybe UTCTime
  }
  deriving ToSchema via (EncodingSchema User) -- <-- ToSchema instance defined here
instance HasJsonEncodingSpec User where
  type EncodingSpec User =
    JsonObject
      '[ Required "name" JsonString
       , Optional "last-login" JsonDateTime
       ]
  toJSONStructure user =
    (Field @"name" (name user),
    (fmap (Field @"last-login") (lastLogin user),
    ()))
```

Calling `Data.Aeson.encode (Data.OpenApi3.toSchema (Proxy :: Proxy User))`
will produce the following Schema:

```json
{
  "additionalProperties": false,
  "properties": {
    "last-login": {
      "format": "date-time",
      "type": "string"
    },
    "name": {
      "type": "string"
    }
  },
  "required": [
    "name"
  ],
  "type": "object"
}
```
