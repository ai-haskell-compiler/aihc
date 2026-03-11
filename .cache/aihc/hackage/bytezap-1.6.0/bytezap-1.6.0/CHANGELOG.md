## 1.6.0 (2024-09-28)
* disable another WIP module

## 1.5.0 (2024-09-28)
* pass metadata in generic struct parser (for pretty errors)
* rewrite type-level bytestring parser (better errors)
* clean up source tree

## 1.4.0 (2024-09-25)
* `Write` now takes a type-level `LengthType` to indicate how it should be used
* rewrite type-level bytestring parsing: now with error handling!
* re-fix GHC 9.8 build
* add some weird wip code for failable serializers

## 1.3.1 (2024-07-15)
* fix building on GHC 9.8 and probably 9.10

## 1.3.0 (2024-04-13)
* move type-level byte stuff to another package

## 1.2.0 (2024-04-11)
* fix type-level generic blen type family (needed defunctionalization)
  * split out into its own even _more_ abstract library, generic-type-functions
* add missing instance `Integral (ByteOrdered end a)`
* add struct parser! design taken from flatparse. barebones combinators
  * add type-level byte parsing. hahahaha
* add `Bytezap.Poke.toStructPoke :: Poke s -> Struct.Poke s`

## 1.1.0 (2024-04-05)
* add struct serializer
* add type-level bytestring utilities (generalized from binrep)

## 1.0.0 (2024-03-17)
* initial release, finally its own package
