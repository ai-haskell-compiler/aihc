<!--
SPDX-FileCopyrightText: Copyright Preetham Gujjula
SPDX-License-Identifier: BSD-3-Clause
-->

We benchmark the performance of the `applyMerge` implementations on different
"shapes" of generated elements.

* Linear: `applyMerge const [1..] [1..]`
  <details>
    <summary>Shape</summary>

      . . . . . . . . . . . . . . .
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
  </details>

* Double linear: `applyMerge min [1..] [1..]`
  <details>
    <summary>Shape</summary>

      . . . . . . . . . . . . . . .
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
  </details>

* Box: `applyMerge max [1..] [1..]`
  <details>
    <summary>Shape</summary>

      . . . . . . . * * * * * * * *
      . . . . . . . * * * * * * * *
      . . . . . . . * * * * * * * *
      . . . . . . . * * * * * * * *
      . . . . . . . * * * * * * * *
      . . . . . . . * * * * * * * *
      . . . . . . . * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
  </details>

* Triangular `applyMerge (+) [1..] [1..]`
  <details>
      <summary>Shape</summary>

      . . . . . . . . . . . . . . *
      . . . . . . . . . . . . . * *
      . . . . . . . . . . . . * * *
      . . . . . . . . . . . * * * *
      . . . . . . . . . . * * * * *
      . . . . . . . . . * * * * * *
      . . . . . . . . * * * * * * *
      . . . . . . . * * * * * * * *
      . . . . . . . * * * * * * * *
      . . . . . . * * * * * * * * *
      . . . . . * * * * * * * * * *
      . . . . * * * * * * * * * * *
      . . . * * * * * * * * * * * *
      . . * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
  </details>

* Skewed triangular `applyMerge (\x y -> 4 * x + y) [1..] [1..]`
  <details>
    <summary>Shape</summary>

      . . . . . . . . . . . . . . *
      . . . . . . . . . . * * * * *
      . . . . . . * * * * * * * * *
      . . * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
  </details>

* Hyperbolic: `applyMerge (*) [1..] [1..]`
  <details>
    <summary>Shape</summary>

      . . . . . . . . . . . . . . .
      . . . . . . . * * * * * * * *
      . . . . . * * * * * * * * * *
      . . . * * * * * * * * * * * *
      . . . * * * * * * * * * * * *
      . . * * * * * * * * * * * * *
      . . * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
    </summary>
  </details>

* Skewed hyperbolic `applyMerge (\x y -> x^3 * y) [1..]`
  <details>
    <summary>Shape</summary>

      . . . . . . . . . . . . . . .
      . . . . . . . . . . . . . . .
      . . . . . . . . . * * * * * *
      . . . * * * * * * * * * * * *
      . . * * * * * * * * * * * * *
      . * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
  </details>

* Circular: `applyMerge (\x y -> x*x + y*y) [1..]`
  <details>
    <summary>Shape</summary>

      . . . . . . . . . . . . . * *
      . . . . . . . . . . . . . * *
      . . . . . . . . . . . . . * *
      . . . . . . . . . . . . . * *
      . . . . . . . . . . . . * * *
      . . . . . . . . . . . . * * *
      . . . . . . . . . . . * * * *
      . . . . . . . . . . . * * * *
      . . . . . . . . . . * * * * *
      . . . . . . . . . * * * * * *
      . . . . . . . . * * * * * * *
      . . . . . . * * * * * * * * *
      . . . . * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
  </details>

* Elliptical: `applyMerge (\x y -> 4*x*x + y*y) [1..]`
  <details>
    <summary>Shape</summary>

      . . . . . . . . . . . . . . *
      . . . . . . . . . . . . . * *
      . . . . . . . . . . . . . * *
      . . . . . . . . . . . . * * *
      . . . . . . . . . . * * * * *
      . . . . . . . * * * * * * * *
      . . . * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
  </details>

* Composites:

  We can generate prime numbers using the Sieve of Erastosthenes:
  ````haskell
  primes :: [Int]
  primes = 2 : ([3..] `minus` composites)    -- `minus` from data-ordlist

  composites :: [Int]
  composites = applyMerge (\p i -> p * (p + i)) primes [0..]
  ````

  The shape of `composites` then looks like:
  <details>
    <summary>Shape</summary>

      . . . . . . . . . . . . . . .
      . . . . . . . . . . . . . . .
      . . . . . . . . . . . * * * *
      . . . . * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * *
  </details>

  Since the *n*th prime number is approximately *n* log *n*, this is a
  quasi-hyperbolic shape, roughly equivalent to

  ```haskell
  f :: Int -> Int -> Double
  f x y =
    let x' :: Double
        x' = fromIntegral x

        y' :: Double
        y' = fromIntegral y

        xlogx :: Double
        xlogx = x' * log x'
     in xlogx * (xlogx + y')

  compositesApprox :: [Double]
  compositesApprox = applyMerge f [1..] [1..]
  ```
