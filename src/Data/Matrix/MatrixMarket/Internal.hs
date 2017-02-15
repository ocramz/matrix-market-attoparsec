{-# LANGUAGE GADTs, OverloadedStrings, DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Matrix.MatrixMarket.Internal
-- Copyright   :  (c) Marco Zocca 2017
-- License     :  GPL-style (see the file LICENSE)
--
-- Maintainer  :  zocca marco gmail
-- Stability   :  experimental
-- Portability :  portable
--
-- Attoparsec parser and serializer for the NIST MatrixMarket format. The parser logic originally appeared in `accelerate-examples` and it is reused here (courtesy of T.McDonell and the `accelerate` developers) with some amendments.
-- 
-- In this version:
-- *) Numbers are represented with Scientific notation instead of floating point
-- *) Parsing rules are a bit relaxed to accommodate various whitespace corner cases
--
-----------------------------------------------------------------------------
module Data.Matrix.MatrixMarket.Internal
       (readMatrix, readArray,
        writeMatrix, writeArray,
        Matrix(..), Array(..),
        Format (Coordinate, Array),
        nnz, dim, numDat,
        dimArr, numDatArr) where



import Control.Applicative                      hiding ( many )

import Data.Int
import qualified Data.Char as C
import Data.Complex
import qualified Data.Scientific as S
import Data.Attoparsec.ByteString.Char8 hiding (I)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Attoparsec.Lazy           as L
import qualified Data.ByteString.Lazy           as L

import Control.Monad.Catch
import Control.Exception.Common



-- | Specifies either sparse or dense storage.  In sparse (\"coordinate\")
-- storage, elements are given in (i,j,x) triplets for matrices (or (i,x) for
-- vectors).  Indices are 1-based, so that A(1,1) is the first element of a
-- matrix, and x(1) is the first element of a vector.
--
-- In dense (\"array\") storage, elements are given in column-major order.
--
-- In both cases, each element is given on a separate line.
--
data Format = Coordinate | Array
    deriving (Eq, Show)

-- | Specifies the element type.  Pattern matrices do not have any elements,
-- only indices, and only make sense for coordinate matrices and vectors.
--
data Field  = R | C | I | P
    deriving (Eq, Show)
             

-- | Specifies any special structure in the matrix.  For symmetric and hermitian
-- matrices, only the lower-triangular part of the matrix is given. For skew
-- matrices, only the entries below the diagonal are stored.
--
data Structure = General | Symmetric | Hermitian | Skew
    deriving (Eq, Show)


-- | NB: indices are 1-based i.e. A(1,1) is the top-left entry of matrix A
data Matrix a = RMatrix (Int, Int) Int Structure [(Int, Int, a)]
              | CMatrix (Int, Int) Int Structure [(Int, Int, Complex a)]
              | PatternMatrix (Int,Int) Int  Structure [(Int32,Int32)]
              | IntMatrix (Int,Int) Int Structure [(Int32,Int32,Int)] deriving (Eq, Show)




data Array a = RArray (Int, Int) Structure [a]
             | CArray (Int, Int) Structure [Complex a] deriving (Eq, Show)






--------------------------------------------------------------------------------
-- Attoparsec combinators
--------------------------------------------------------------------------------

comment :: Parser ()
comment = char '%' *> skipWhile (not . eol) *> endOfLine
  where
    eol w = w `elem` ("\n\r" :: String)

floating :: Parser S.Scientific
floating = skipSpace' *> scientific

integral :: Integral a => Parser a
integral = skipSpace' *> decimal

format :: Parser Format
format =  string "coordinate" *> pure Coordinate
      <|> string "array"      *> pure Array
      <?> "matrix format"

field :: Parser Field
field =  string "real"    *> pure R
     <|> string "complex" *> pure C
     <|> string "integer" *> pure I
     <|> string "pattern" *> pure P
     <?> "matrix field"

structure :: Parser Structure
structure =  string "general"        *> pure General
         <|> string "symmetric"      *> pure Symmetric
         <|> string "hermitian"      *> pure Hermitian
         <|> string "skew-symmetric" *> pure Skew
         <?> "matrix structure"

header :: Parser (Format,Field,Structure)
header =  string "%%MatrixMarket matrix"
       >> (,,) <$> (skipSpace' *> format)
               <*> (skipSpace' *> field)
               <*> (skipSpace' *> structure)
               <*  endOfLine
               <?> "MatrixMarket header"

extentMatrix :: Parser (Int,Int,Int)
extentMatrix = do
  [m,n,l] <- skipSpace' *> count 3 integral <* endOfLine
  return (m,n,l)

extentArray :: Parser (Int,Int)
extentArray = do
  [m,n] <- skipSpace' *> count 2 integral <* endOfLine
  return (m,n)

line3 :: Integral i => Parser a -> Parser (i,i,a)
line3 f = (,,) <$> integral
               <*> integral
               <*> f
               <*  endOfLine

skipSpace' :: Parser String
skipSpace' = many' space


--------------------------------------------------------------------------------
-- Load and parse
--------------------------------------------------------------------------------

matrix :: Parser (Matrix S.Scientific)
matrix = do
  (f, t, s) <- header
  (m, n, l) <- skipMany comment *> extentMatrix
  if f /= Coordinate
    then fail "matrix is not in Coordinate format"
    else
    case t of 
     R -> RMatrix (m,n) l s <$> many1 (line3 floating)
     C -> CMatrix (m,n) l s <$> many1 (line3 ((:+) <$> floating <*> floating))
     I -> IntMatrix     (m,n) l s <$> many1 (line3 integral)
     P -> PatternMatrix (m,n) l s <$> many1 ((,) <$> integral <*> integral)

array :: Parser (Array S.Scientific)
array = do
  (f, t, s) <- header
  (m, n) <- skipMany comment *> extentArray
  if f /= Array
    then fail "array is not in Array format"
    else
     case t of
       R -> RArray (m,n) s <$> many1 floating
       C -> CArray (m,n) s <$> many1 ((:+) <$> floating <*> floating)
       _ -> fail "integer and pattern cases not relevant for the dense case"


-- | Load a matrix (sparse, i.e. in Coordinate format) from file
readMatrix :: FilePath -> IO (Matrix S.Scientific)
readMatrix file = do
  chunks <- L.readFile file
  case L.parse matrix chunks of
    L.Fail _ _ msg      -> throwM (FileParseError "readMatrix" msg)
    L.Done _ mtx        -> return mtx

-- | Load a dense matrix (i.e. a matrix or vector in Array format) from file
readArray :: FilePath -> IO (Array S.Scientific)
readArray file = do
  chunks <- L.readFile file
  case L.parse array chunks of
    L.Fail _ _ msg      -> throwM (FileParseError "readArray" msg)
    L.Done _ mtx        -> return mtx


--------------------------------------------------------------------------------
-- Write to file
--------------------------------------------------------------------------------

showFormat :: Format -> String
showFormat = map C.toLower <$> show

showField :: Field -> String
showField f = case f of R -> "real"
                        C -> "complex"
                        I -> "integer"
                        P -> "pattern"

showStruct :: Structure -> String
showStruct = map C.toLower <$> show

-- %%MatrixMarket matrix coordinate real general

headerStr :: Format -> Field -> Structure -> L.ByteString
headerStr f t s =
    B.pack $ unwords ["%%MatrixMarket matrix",
                      showFormat f, showField t, showStruct s]


nl :: L.ByteString
nl = toLBS "\n" 

showLines :: (a -> String) -> [a] -> L.ByteString
showLines showf d = L.concat (L.pack . withNewline . showf <$> d) where
  withNewline x = toEnum . C.ord <$> x ++ "\n"



-- | Serialize a sparse matrix in Coordinate format
writeMatrix :: Show b => FilePath -> Matrix b -> IO ()
writeMatrix file mat = 
  case mat of (RMatrix d nz s dat) -> 
                L.writeFile file (matrixByteString d nz R s dat)
              (CMatrix d nz s dat) -> 
                L.writeFile file (matrixByteString d nz C s dat)
              (IntMatrix d nz s dat) -> 
                L.writeFile file (matrixByteString d nz I s dat)
              _ -> error "writeMatrix : PatternMatrix not implemented yet"
     where
       matrixByteString di nz t s d =
         L.concat [headerStr Coordinate t s,
                   nl,
                   headerSzMatrix di nz,
                   nl,
                   showLines sf3 d]
         where
         sf3 (i,j,x) = unwords [show i, show j, show x]
         headerSzMatrix (m,n) numz = B.pack $ unwords [show m, show n, show numz] 

-- | Serialize a dense matrix in Array format
writeArray :: Show a => FilePath -> Array a -> IO ()  
writeArray file arr = 
  case arr of (RArray d s dat) -> L.writeFile file (arrayByteString d R s dat)
              (CArray d s dat) -> L.writeFile file (arrayByteString d C s dat)
    where
      arrayByteString di t s d =
        L.concat [headerStr Array t s,
            nl,
            headerSzArray di,
            nl,
            showLines show d] where
        headerSzArray (m,n) = B.pack $ unwords [show m, show n]
  
  






-- helpers

nnz :: Matrix t -> Int
nnz m = case m of (RMatrix _ nz _ _) -> nz
                  (CMatrix _ nz _ _) -> nz
                  (PatternMatrix _ nz _ _) -> nz
                  (IntMatrix _ nz _ _) -> nz

dim :: Matrix t -> (Int, Int)
dim m = case m of (RMatrix d _ _ _) -> d
                  (CMatrix d _ _ _) -> d
                  (PatternMatrix d _ _ _) -> d
                  (IntMatrix d _ _ _) -> d

numDat :: Matrix t -> Int
numDat m = case m of (RMatrix _ _ _ d) -> length d
                     (CMatrix _ _ _ d) -> length d
                     (PatternMatrix _ _ _ d) -> length d
                     (IntMatrix _ _ _ d) -> length d


dimArr :: Array t -> (Int, Int)
dimArr a = case a of (RArray d _ _) -> d
                     (CArray d _ _) -> d
 
numDatArr :: Array a -> Int
numDatArr a = case a of (RArray _ _ ll) -> length ll 
                        (CArray _ _ ll) -> length ll            




-- | String -> ByteString
-- NB: do not abuse
toBS :: String -> BS.ByteString
toBS x = BS.pack $ (toEnum . C.ord) <$> x

-- | String -> lazy ByteString
toLBS :: String -> L.ByteString
toLBS x = L.pack $ (toEnum . C.ord) <$> x




-- t1 = toBS "%%MatrixMarket matrix coordinate real general\n5  5  8\n1     1   1.000e+00\n2     2   1.050e+01\n3     3   1.500e-02\n1     4   6.000e+00\n4     2   2.505e+02\n4     4  -2.800e+02\n4     5   3.332e+01\n5     5   1.200e+01\n"

-- t2 = toBS "%%MatrixMarket matrix coordinate real general\n5  5  8\n1     1   1.000e+00\n"
