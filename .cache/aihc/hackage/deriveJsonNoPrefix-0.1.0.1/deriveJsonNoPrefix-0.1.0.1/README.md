# deriveJsonNoPrefix

Template Haskell macros to derive ToJSON/FromJSON instances in a more prefix-friendly manner.

## Example

Suppose you want to create a JSON like this:

```json
{
    "id": "ID STRING",
    "max": 0.789,
    "min": 0.123
}
```

You'd want to define a record type to derive the instance of [ToJSON](http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:ToJSON) (and possibly [FromJSON](http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:FromJSON)) automatically:

```hs
{-# LANGUAGE TemplateHaskell #-}

import Data.Aeson.TH

data SomeRecord = SomeRecord
  { id :: String
  , max :: Double
  , min :: Double
  } deriving (Eq, Show)

$(deriveToJSON ''SomeRecord)
```

But you shouldn't define such a record because both `id`, `max`, and `min` are predefined functions of `Prelude`!!

As a workaround, we frequently prefix the record labels with their type name:

```hs
data SomeRecord = SomeRecord
  { someRecordId :: String
  , someRecordMax :: Double
  , someRecordMin :: Double
  } deriving (Eq, Show)
```

Then `deriveToJSON` with a modified option:

```hs
deriveToJSON Json.defaultOptions { fieldLabelModifier = firstLower . drop (length "SomeRecord") } ''SomeRecord
```

That's almost exactly what `deriveToJsonNoTypeNamePrefix` does!!  
`deriveToJsonNoTypeNamePrefix` is essentially defined as:

```hs
deriveToJsonNoTypeNamePrefix :: Name -> Q [Dec]
deriveToJsonNoTypeNamePrefix deriver name =
  deriveToJSON Json.defaultOptions { fieldLabelModifier = dropPrefix name } name
```

So now, you don't have reimplement the `fieldLabelModifier` anymore!

```hs
import Data.Aeson.DeriveNoPrefix

$(deriveJsonNoTypeNamePrefix ''SomeRecord)
```

## Other libraries which would solve the same problem

- [extensible](https://hackage.haskell.org/package/extensible).
- And other libraries providing extensible records with `ToJSON` / `FromJSON` instances.

So use this package all of them are too heavy in learning cost / dependency footprint / etc.
