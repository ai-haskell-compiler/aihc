# oeis2

[![GitHub Actions](https://github.com/23prime/oeis2/actions/workflows/test.yml/badge.svg)](https://github.com/23prime/oeis2/actions/workflows/test.yml)
[![Haskell](https://img.shields.io/badge/Language-Haskell-yellowgreen.svg)](https://www.haskell.org)
[![Hackage version](https://img.shields.io/hackage/v/oeis2.svg?label=Hackage&color=4cc41c)](https://hackage.haskell.org/package/oeis2)
[![Stackage version](https://www.stackage.org/package/oeis2/badge/lts?label=Stackage)](https://www.stackage.org/package/oeis2)
[![BSD3](https://img.shields.io/badge/License-BSD-blue.svg)](https://en.wikipedia.org/wiki/BSD_License)

Haskell interface for [Online Encyclopedia of Integer Sequences](https://oeis.org/); homage to [oeis](http://hackage.haskell.org/package/oeis2).

## Difference from  [oeis](http://hackage.haskell.org/package/oeis)

- Source data of OEIS.  
  : [oeis](http://hackage.haskell.org/package/oeis) use `fmt=text`, but this library use `fmt=json`.
- Possible to get all search results.
- Search functions from ID or sub-sequence are merged.
- Possibele to search from **other** than ID or sub-sequence.
- Support for HTTPS.

## Usage

Add import statement.

```haskell
import Math.OEIS
```

- Get all search results from sub-sequence

  - If `n == 0`, you get all search results.

    ```haskell
    ghci>searchSeq (SubSeq [1,2,3,4]) 0
    [OEIS {number = "A000027", ids = ["M0472","N0173"], seqData = [1,2,3,4,5,6,7,...
    ghci>length it
    53
    ```

  - Otherwise, you get first `n` search results.

    ```haskell
    ghci>searchSeq (SubSeq [1,2,3,4]) 17
    [OEIS {number = "A000027", ids = ["M0472","N0173"], seqData = [1,2,3,4,5,6,7,8,9,
    ghci>length it
    17
    ```

- Get first few terms from sub-sequence

  ```haskell
  ghci>getSeqData (SubSeq [1,2,2,3,3,3,4,4,4,4])
  Just [1,2,2,3,3,3,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,7,7,7,...
  ```

- Get Mathematica function from sub-sequence

  ```haskell
  ghci>mathematica <$> lookupSeq (SubSeq [1,2,2,3,3,3,4,4,4,4])
  Just ["a[1] = 1; a[n_] := a[n] = a[n - a[n - 1]] + 1 (* _Branko Curgus_, May 12 2009 *)","Table[n, {n, 13}, {n}] // Flatten (* _Robert G. Wilson v_, May 11 2010 *)"]
  ```

- If no search result

  ```haskell
  ghci>lookupSeq (ID "1145141919893")
  Nothing
  ```
