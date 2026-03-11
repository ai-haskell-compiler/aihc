# enum-subset-generate

[![Build Status](https://travis-ci.org/0xd34df00d/enum-subset-generate.svg?branch=master)](https://travis-ci.org/0xd34df00d/enum-subset-generate)

Generates an ADT having a subset of constructors of some other ADT along with a
pair of functions to map between the two.

## Motivation

Consider implementing FFI bindings for a library. The lower level (directly
mapping the C API onto Haskell) might expose an enumeration as an ADT, not all
values of which might make sense for higher-level well-typed code. In this case
the higher-level bindings might instead expose the generated ADT.

As an example, consider a parser library for a language like C++ or Java. A
cursor pointing to a node in an AST might have a property like
```c
enum AccessSpecifier {
    AS_Invalid,
    AS_Public,
    AS_Protected,
    AS_Private
};

AccessSpecifier getAccessSpecifier(Cursor*);
```

Access specifier doesn't make much sense for a node representing a `for`-loop,
hence the `AS_Invalid` member.

A low-level Haskell bindings library might translate this enum into
```haskell
module Library.Bindings.FFI where

-- ...

data AccessSpecifier = Invalid
                     | Public
                     | Protected
                     | Private
                     deriving (Eq, Ord, Show)

getAccessSpecifier :: Cursor -> BindingsMonad AccessSpecifier
```

A more type-safe wrapper around this might introduce typed cursors and add a
constraint to the function:
```haskell
module Library.Bindings.Pure where

import qualified Library.Bindings.FFI as FFI

accessSpecifier :: HasAccessSpecifier t => Cursor t -> FFI.AccessSpecifier
```
so this `accessSpecifier` is guaranteed to always produce a non-`Invalid`
result. But since it's not explicitly stated in the types, the calling code will
not be able to know about this, so the compiler's case analyzer might still
require handling the case of `Invalid`.

Using this library, one might instead just do
```haskell
-- Generate an AccessSpecifier in this module using FFI.AccessSpecifier
-- but without the FFI.Invalid constructor.
mkEnum ''FFI.AccessSpecifier ['FFI.Invalid]

accessSpecifier :: HasAccessSpecifier t => Cursor t -> AccessSpecifier
```
