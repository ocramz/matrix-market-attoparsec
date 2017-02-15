{-# LANGUAGE GADTs, OverloadedStrings, DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Matrix.MatrixMarket2
-- Copyright   :  (c) Marco Zocca 2017
-- License     :  GPL-style (see the file LICENSE)
--
-- Maintainer  :  zocca marco gmail
-- Stability   :  experimental
-- Portability :  portable
--
-- Attoparsec parser and serializer for the NIST MatrixMarket format. The parser logic originally appeared in `accelerate-examples` and it is reused here (courtesy of T.McDonell and the `accelerate` developers) with some amendments.
-- For example, in this version we use Scientific notation instead of Float
--
-----------------------------------------------------------------------------
module Data.Matrix.MatrixMarket.Internal where

import Control.Applicative                      hiding ( many )

import Data.Int
import Data.List
import qualified Data.Char as C
import Data.Complex
import qualified Data.Scientific as S
import Data.Attoparsec.ByteString.Char8 hiding (I)
import GHC.Word
import Data.ByteString.Lex.Fractional
-- import qualified Data.ByteString.Internal as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Attoparsec.Lazy           as L
import qualified Data.ByteString.Lazy           as L

-- import qualified Data.Vector as V

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

-- floating :: Fractional a => Parser a
-- floating = do
--   -- mv <- readSigned readDecimal <$> (skipSpace *> takeTill isSpace)
--   mv <- scientific <$> (skipSpace *> takeTill isSpace)
--   case mv of
--        Just (v,_) -> return v
--        Nothing    -> fail "floating-point number"

floating = skipSpace' *> scientific -- <$> (skipSpace *> takeTill isSpace)

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

-- parseLines f = many1 f -- <* endOfInput


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
    L.Fail _ _ msg      -> throwM (FileParseError "readMatrix" msg)
    L.Done _ mtx        -> return mtx


--------------------------------------------------------------------------------
-- Write to file
--------------------------------------------------------------------------------

showFormat :: Format -> String
showFormat = map C.toLower <$> show

showField :: Field -> String
showField f = case f of R -> "real"
                        C -> "complex"

showStruct :: Structure -> String
showStruct = map C.toLower <$> show

-- %%MatrixMarket matrix coordinate real general

headerStr :: Format -> Field -> Structure -> L.ByteString
headerStr f t s =
    B.pack $ unwords ["%%MatrixMarket matrix",
                      showFormat f, showField t, showStruct s]


headerSzMatrix :: (Show a2, Show a1, Show a) => (a, a1) -> a2 -> L.ByteString
headerSzMatrix (m,n) nz = B.pack $ unwords [show m, show n, show nz] 
headerSzArray :: (Show a1, Show a) => (a, a1) -> L.ByteString
headerSzArray (m,n) = B.pack $ unwords [show m, show n]


-- | From a sparse matrix in Coordinate format to its MatrixMarket serialized form
matrixByteString :: Show b => 
 (Int, Int) -> Int -> Field -> Structure -> [(Int, Int, b)] -> L.ByteString
matrixByteString di nz t s d =
  L.concat [headerStr Coordinate t s,
            nl,
            headerSzMatrix di nz,
            nl,
            showLines sf3 d] where
  sf3 (i,j,x) = unwords [show i, show j, show x]

-- | From a dense matrix in Array format to its MatrixMarket serialized form
arrayByteString :: Show a =>
     (Int, Int) -> Field -> Structure -> [a] -> L.ByteString
arrayByteString di t s d =
  L.concat [headerStr Array t s,
            nl,
            headerSzArray di,
            nl,
            showLines show d]
  

nl :: L.ByteString
nl = L.pack $ withNewline " "

showLines :: (a -> String) -> [a] -> L.ByteString
showLines showf d = L.concat (L.pack . withNewline . showf <$> d)

withNewline :: Enum b => String -> [b]
withNewline x = toEnum . C.ord <$> x ++ "\n"





writeMatrix :: Show b => FilePath -> Matrix b -> IO ()
writeMatrix file mat = do
  case mat of (RMatrix d nz s dat) -> 
                L.writeFile file (matrixByteString d nz R s dat)
              (CMatrix d nz s dat) -> 
                L.writeFile file (matrixByteString d nz C s dat) 

writeArray :: Show a => FilePath -> Array a -> IO ()  
writeArray file arr = do
  case arr of (RArray d s dat) -> L.writeFile file (arrayByteString d R s dat)
              (CArray d s dat) -> L.writeFile file (arrayByteString d C s dat)
  






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
dimArr a = case a of (RArray d s _) -> d
                     (CArray d s _) -> d
 
numDatArr :: Array a -> Int
numDatArr a = case a of (RArray _ s ll) -> length ll 
                        (CArray _ s ll) -> length ll            




-- | String -> ByteString
-- NB: do not abuse
toBS :: String -> BS.ByteString
toBS x = BS.pack $ (toEnum . C.ord) <$> x


t1 = toBS "%%MatrixMarket matrix coordinate real general\n5  5  8\n1     1   1.000e+00\n2     2   1.050e+01\n3     3   1.500e-02\n1     4   6.000e+00\n4     2   2.505e+02\n4     4  -2.800e+02\n4     5   3.332e+01\n5     5   1.200e+01\n"

t2 = toBS "%%MatrixMarket matrix coordinate real general\n5  5  8\n1     1   1.000e+00\n"
