module TextBuilder
  ( TextBuilder,

    -- * Accessors
    toText,
    toString,
    isEmpty,

    -- * Constructors

    -- ** Transformations
    force,
    intercalate,
    intercalateMap,

    -- ** Textual
    text,
    lazyText,
    string,
    unsafeUtf8ByteString,

    -- ** Character
    char,
    unicodeCodepoint,

    -- ** Integers

    -- *** Decimal
    decimal,
    fixedLengthDecimal,
    thousandSeparatedDecimal,

    -- *** Binary
    binary,
    prefixedBinary,

    -- *** Octal
    octal,
    prefixedOctal,

    -- *** Hexadecimal
    hexadecimal,
    prefixedHexadecimal,
  )
where

import TextBuilder.Domains.ByteString
import TextBuilder.Domains.Combinators
import TextBuilder.Domains.Digits
import TextBuilder.Domains.Other
import TextBuilderCore
