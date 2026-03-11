# generic-aeson

[![Build Status](https://travis-ci.org/silkapp/generic-aeson.svg?branch=master)](https://travis-ci.org/silkapp/generic-aeson)

The structure of the generated JSON is meant to be close to
idiomatic JSON. This means:

* Enumerations (data types containing constructors without fields) are converted to JSON strings.

* Record fields become JSON keys.

* Data types with one unlabeled field convert to just that field.

* Data types with multiple unlabeled fields become arrays.

* Multiple constructors are represented by keys.

* `Maybe` values are either an absent key, or the value.

* Leading and trailing underscores are removed from constructor names and record fields

See `tests/Main.hs` in [json-schema](http://hackage.haskell.org/package/json-schema) for more examples.


## How does generic-aeson compare to the TH/Generics already present in aeson?

generic-aeson contains more special cases for creating more concise
and idiomatic json. If you're working with the JSON representation
directly generic-aeson should feel more natural.

## Will the generated format ever change?

Changing the format would incur a breaking change to every API that
uses generic-aeson so we must keep it intact.

If we find a bug where the fix changes the format we need to create a
new package or version the generation code.

## Schemas

[json-schema](http://hackage.haskell.org/package/json-schema) has
generic derivation of schemas that match the generic-aeson format.
