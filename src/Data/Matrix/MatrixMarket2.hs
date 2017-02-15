{-# LANGUAGE GADTs, OverloadedStrings, DeriveFunctor #-}

module Data.Matrix.MatrixMarket2 where

import Control.Applicative                      hiding ( many )

import Data.Int
import Data.Complex
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Lex.Fractional
import qualified Data.Attoparsec.Lazy           as L
import qualified Data.ByteString.Lazy           as L

import qualified Data.Vector as V

import Control.Monad.Catch
import Control.Exception.Common

-- | Specifies the element type.  Pattern matrices do not have any elements,
-- only indices, and only make sense for coordinate matrices and vectors.
--
data Field  = Real | Complex | Integer | Pattern
    deriving (Eq, Show)

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

-- | Specifies any special structure in the matrix.  For symmetric and hermitian
-- matrices, only the lower-triangular part of the matrix is given. For skew
-- matrices, only the entries below the diagonal are stored.
--
data Structure = General | Symmetric | Hermitian | Skew
    deriving (Eq, Show)


-- | NB: indices are 1-based i.e. A(1,1) is the top-left entry of matrix A
data Matrix a = RMatrix (Int, Int) Int Format Structure [(Int, Int, a)]
              | CMatrix (Int, Int) Int Format Structure [(Int, Int, Complex a)]
              | PatternMatrix (Int,Int) Int Format Structure [(Int32,Int32)]
              | IntMatrix (Int,Int) Int Format Structure [(Int32,Int32,Int)] deriving (Eq, Show)

nnz :: Matrix t -> Int
nnz m = case m of (RMatrix _ nz _ _ _) -> nz
                  (CMatrix _ nz _ _ _) -> nz
                  (PatternMatrix _ nz _ _ _) -> nz
                  (IntMatrix _ nz _ _ _) -> nz

dim :: Matrix t -> (Int, Int)
dim m = case m of (RMatrix d _ _ _ _) -> d
                  (CMatrix d _ _ _ _) -> d
                  (PatternMatrix d _ _ _ _) -> d
                  (IntMatrix d _ _ _ _) -> d

numDat :: Matrix t -> Int
numDat m = case m of (RMatrix _ _ _ _ d) -> length d
                     (CMatrix _ _ _ _ d) -> length d
                     (PatternMatrix _ _ _ _ d) -> length d
                     (IntMatrix _ _ _ _ d) -> length d


--

-- headerStr :: Data.ByteString
headerStr = "%%MatrixMarket matrix"


--------------------------------------------------------------------------------
-- Attoparsec combinators
--------------------------------------------------------------------------------

comment :: Parser ()
comment = char '%' *> skipWhile (not . eol) *> endOfLine
  where
    eol w = w `elem` ("\n\r" :: String)

floating :: Fractional a => Parser a
floating = do
  mv <- readSigned readDecimal <$> (skipSpace *> takeTill isSpace)
  case mv of
       Just (v,_) -> return v
       Nothing    -> fail "floating-point number"

integral :: Integral a => Parser a
integral = skipSpace *> decimal

format :: Parser Format
format =  string "coordinate" *> pure Coordinate
      <|> string "array"      *> pure Array
      <?> "matrix format"

field :: Parser Field
field =  string "real"    *> pure Real
     <|> string "complex" *> pure Complex
     <|> string "integer" *> pure Integer
     <|> string "pattern" *> pure Pattern
     <?> "matrix field"

structure :: Parser Structure
structure =  string "general"        *> pure General
         <|> string "symmetric"      *> pure Symmetric
         <|> string "hermitian"      *> pure Hermitian
         <|> string "skew-symmetric" *> pure Skew
         <?> "matrix structure"

header :: Parser (Format,Field,Structure)
header =  string headerStr -- "%%MatrixMarket matrix"
       >> (,,) <$> (skipSpace *> format)
               <*> (skipSpace *> field)
               <*> (skipSpace *> structure)
               <*  endOfLine
               <?> "MatrixMarket header"

extent :: Parser (Int,Int,Int)
extent = do
  [m,n,l] <- skipWhile isSpace *> count 3 integral <* endOfLine
  return (m,n,l)

line :: Integral i => Parser a -> Parser (i,i,a)
line f = (,,) <$> integral
              <*> integral
              <*> f
              <*  endOfLine

--------------------------------------------------------------------------------
-- Load and parse
--------------------------------------------------------------------------------

matrix :: Parser (Matrix Double)
matrix = do
  (f, t, s) <- header
  (m, n, l) <- skipMany comment *> extent
  case t of 
    Real -> RMatrix (m,n) l f s <$> many1 (line floating)
    Complex -> CMatrix (m,n) l f s <$> many1 (line ((:+) <$> floating <*> floating))
    Integer -> IntMatrix     (m,n) l f s <$> many1 (line integral)
    Pattern -> PatternMatrix (m,n) l f s <$> many1 ((,) <$> integral <*> integral)


-- parseLines f = many1 f -- <* endOfInput


-- | Load a matrix from file
readMatrix :: FilePath -> IO (Matrix Double)
readMatrix file = do
  chunks <- L.readFile file
  case L.parse matrix chunks of
    L.Fail _ _ msg      -> throwM (FileParseError "readMatrix" msg)
    L.Done _ mtx        -> return mtx



--------------------------------------------------------------------------------
-- Write to file
--------------------------------------------------------------------------------


-- writeMatrix file mat = do
  
  
