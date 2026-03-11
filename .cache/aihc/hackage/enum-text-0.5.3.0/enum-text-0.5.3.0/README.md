# enum-text

A simple toolkit for rendering enumerated types into `Text` `Builder` (used by
the [`fmt`](http://hackage.haskell.org/package/fmt) package) and parsing them
back again into Text with the provided `TextParsable` type class.

To get the `Buildable` and `TextParsable` instances for an enumerated data type
the following pattern can be used without any language extensions:

```
import Fmt
import Text.Enum.Text

data Foo = FOO_bar | FOO_bar_baz
  deriving (Bounded,Enum,Eq,Ord,Show)

instance EnumText     Foo
instance Buildable    Foo where build     = buildEnumText
instance TextParsable Foo where parseText = parseEnumText
```

With the `DeriveAnyClass` language extension you can list `EnumText` in the
`deriving` clause, and with `DerivingVia` (available from GHC 8.6.1) you can
derive `via` `UsingEnumText` as follows:

```
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}

import Fmt
import Text.Enum.Text

data Foo = FOO_bar | FOO_bar_baz
  deriving (Bounded,Enum,EnumText,Eq,Ord,Show)
  deriving (Buildable,TextParsable) via UsingEnumText Foo
```

This will use the default configuration for generating the text of each
enumeration from the derived `show` text, namely:

  * removing the prefix upto and including the first underscore (`_`);
  * converting each subsequent underscore (`_`) into a dash (`-`).

See the Haddocks for details on how to override this default configuration for
any given enumeration type.

Functions for rendering text, generating and parsing UTF-8 encoded ByteStrings
(suitable for cassava) and `Hashable` functions are also provided `EnumText`.
