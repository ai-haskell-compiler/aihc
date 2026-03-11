module TextBuilderDev
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
    padFromLeft,
    padFromRight,

    -- ** Textual
    text,
    lazyText,
    string,
    unsafeUtf8ByteString,

    -- ** Character
    char,
    unicodeCodepoint,

    -- ** Data
    byteStringHexEncoding,

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

    -- ** Real
    doubleFixedPoint,
    doubleFixedPointPercent,

    -- ** Time
    utcTimeIso8601Timestamp,
    realFracDdHhMmSsInterval,
    diffTimeSeconds,
    picoseconds,

    -- ** Other
    approximateDataSize,

    -- * Classes
    Isomorphic (..),
  )
where

import TextBuilder
import TextBuilderDev.Domains.Digits
import TextBuilderDev.Domains.Other
import TextBuilderDev.Domains.Padding
import TextBuilderDev.Domains.StrictBuilder ()
import TextBuilderDev.Domains.StrictTextBuilder ()
import TextBuilderDev.Domains.Time
import TextBuilderDev.Isomorphic
