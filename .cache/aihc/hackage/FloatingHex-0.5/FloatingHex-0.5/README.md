## FloatingHex: Read/Write Hexadecimal floats

[![Hackage version](http://img.shields.io/hackage/v/FloatingHex.svg?label=Hackage)](http://hackage.haskell.org/package/FloatingHex)
[![Build Status](http://img.shields.io/travis/LeventErkok/FloatingHex.svg?label=Build)](http://travis-ci.org/LeventErkok/FloatingHex)

### Hexadecimal Floats

For syntax reference, see: <http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf>, pages 57-58.
We slightly diverge from the standard and do not allow for the "floating-suffix,"
as the type inference of Haskell makes this unnecessary. Some examples are:

```
  [hf|0x1p+1|]
  [hf|0x1p+8|]
  [hf|0x1.b7p-1|]
  [hf|0x1.fffffffffffffp+1023|]
  [hf|0X1.921FB4D12D84AP-1|]
```

This format allows for concise and precise string representation for floating point numbers. Note that you need the `QuasiQuotes` extension of GHC to be able to write these literals.

## Example

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Data.Numbers.FloatingHex

-- expressions
f :: Double
f = [hf|0x1.f44abd5aa7ca4p+25|]

-- patterns
g :: Float -> String
g [hf|0x1p1|]  = "two!"
g [hf|0x1p-1|] = "half!"
g d            = "something else: " ++ show d

-- showing hexadecimal floats
test = showHFloat [hf|0x1.f44abd5aa7ca4p+25|] ""
```

(Note that while the quasiquoter allows for floating-point patterns, it is usually not a good idea to use floating-point literals in pattern matching.)

### Thanks
The following people reported bugs, provided comments/feedback, or contributed to the development of
FloatingHex in various ways: Herbert Valerio Riedel.
