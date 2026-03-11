# Mantissa

Reals in the interval [0,1), implemented as machine words. Boxed and
unboxed variants are available.

Arithmetic is available; addition and subtraction is taken
modulo 1. On the whole, the user is responsible for ensuring that,
when these numbers are produced by division (in any of several
possible ways), the result will be in the interval [0,1).

## Nomenclature

As of 2024, most people would call this concept a
[fractional part](https://en.wikipedia.org/wiki/Fractional_part),
but the name
[Fractional](https://hackage.haskell.org/package/base-4.20.0.1/docs/Prelude.html#t:Fractional)
is already in use in Haskell base for something different. Thus we use the slightly obscure name
[mantissa](https://en.wikipedia.org/wiki/Common_logarithm#Mantissa_and_characteristic).



