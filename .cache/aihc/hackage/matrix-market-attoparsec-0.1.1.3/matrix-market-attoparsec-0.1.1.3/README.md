# matrix-market-attoparsec

[![Build Status](https://travis-ci.org/ocramz/matrix-market-attoparsec.png)](https://travis-ci.org/ocramz/matrix-market-attoparsec)

Attoparsec parser for the NIST Matrix Market format [0].

The library also contains functions for serializing matrix data to text file.

## User guide

The module `Data.Matrix.MatrixMarket` exports the user interface:

    readMatrix :: FilePath -> IO (Matrix S.Scientific)

    readArray :: FilePath -> IO (Array S.Scientific)

    writeMatrix :: Show a => FilePath -> Matrix a -> IO ()
 
    writeArray :: Show a => FilePath -> Array a -> IO ()  

The first two functions contain the parsing logic, and make use of `scientific` for parsing numerical data in scientific notation.

As of version 0.1.1 there are also intermediate functions `readMatrix'`, `readArray'`, `writeMatrix'` and `writeArray'` that do not touch the filesystem but (de-)serialize from/to lazy ByteString.


### Naming convention

We follow the MatrixMarket format definitions, by which a "Matrix" is _sparse_ and stored in coordinate format (row, column, entry), whereas an "Array" is a _dense_ grid of numbers, stored in column-oriented form.
Algebraic vectors, such as the right-hand sides of equation systems, are stored as n-by-1 Arrays.

## Testing

`test/LibSpec.hs` contains a simple read/write/read sanity test for the included Matrix Marked data files (`fidapm05.mtx` and `fidapm05_rhs1.mtx`):

    m0 <- readMatrix fname   -- load original
    writeMatrix ftemp m0     -- save as temp
    m1 <- readMatrix ftemp   -- load temp
    m0 `shouldBe` m1         -- compare temp with original


## References

[0] [http://math.nist.gov/MatrixMarket/](http://math.nist.gov/MatrixMarket/)
