# matrix-market-attoparsec

[![Build Status](https://travis-ci.org/ocramz/matrix-market-attoparsec.png)](https://travis-ci.org/ocramz/matrix-market-attoparsec)

Attoparsec parser for the NIST Matrix Market format [1].

The library also contains functions for serializing to text file, and the read/write/read roundtrip works as expected.

## User guide

The module `Data.Matrix.MatrixMarket` exports the user interface:


    readMatrix :: FilePath -> IO (Matrix S.Scientific)

    readArray :: FilePath -> IO (Array S.Scientific)



    writeMatrix :: Show a => FilePath -> Matrix a -> IO ()
 
    writeArray :: Show a => FilePath -> Array a -> IO ()  

The first two functions contain the parsing logic, and make use of `scientific` for parsing numerical data in scientific notation.

`test/LibSpec.hs` contains a simple read/write/read sanity test:

    m0 <- readMatrix fname   -- load original
    writeMatrix ftemp m0     -- save as temp
    m1 <- readMatrix ftemp   -- load temp
    m0 `shouldBe` m1         -- compare temp with original


## References

[1] [http://math.nist.gov/MatrixMarket/](http://math.nist.gov/MatrixMarket/)
