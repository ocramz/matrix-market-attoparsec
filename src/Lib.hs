{-# language OverloadedStrings #-}
module Lib where


import Control.Applicative
import qualified Data.Vector as V

-- import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

import Data.Attoparsec.Internal.Types (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P (decimal, double, scientific, letter_ascii, char, endOfLine, endOfInput, parseOnly, space, count)
import qualified Data.Attoparsec.ByteString.Lazy as P

import Data.Complex




-- loadData fname = do
--   c <- B.readFile fname
--   let header = P.parseOnly parseHeader c
--   -- (SparseInfo nr nc nnz) <- P.parseOnly parseInfo c
--   case header of
--     Left e -> error e
--     Right (MMFormat repr numFormat symm) ->
--       undefined
--       -- case repr of Coordinate -> do




data MRepr = Coordinate | Array deriving (Eq, Show)

-- | Numerical format : Real / Integer / Complex 
data MNumFormat = R | I | C deriving (Eq, Show)

data MSymm = General | Symmetric | SkewSymmetric | Hermitian deriving (Eq, Show)

data MMFormat = MMFormat MRepr MNumFormat MSymm deriving (Eq, Show)





repr =
  (P.string "coordinate" >> pure Coordinate) <|>
  (P.string "array" >> pure Array)

numFormat =
  (P.string "real" >> pure R) <|>
  (P.string "complex" >> pure C) <|>
  (P.string "integer" >> pure I)

symm =
  (P.string "general" >> pure General) <|>
  (P.string "symmetric" >> pure Symmetric) <|>
  (P.string "skew-symmetric" >> pure SkewSymmetric) <|>
  (P.string "hermitian" >> pure Hermitian)

parseHeader :: Parser B.ByteString MMFormat
parseHeader = do
  P.string "%%MatrixMarket matrix"
  spaces
  r <- repr
  spaces
  f <- numFormat
  spaces
  s <- symm
  return $ MMFormat r f s




-- * sparse row types

data SparseInfo =
  SparseInfo {-# UNBOX #-} !Int
             {-# UNBOX #-} !Int
             {-# UNBOX #-} !Int deriving (Eq, Show)

parseInfo = do
  spaces
  nr <- P.decimal
  spaces
  nc <- P.decimal
  spaces
  nnz <- P.decimal
  return $ SparseInfo nr nc nnz

-- | Real-valued, Coordinate 
data RCoordRow =
  RCRow {-# UNBOX #-} !Int
        {-# UNBOX #-} !Int
        {-# UNBOX #-} !Double deriving (Eq, Show)

parseRCoordRow :: Parser B.ByteString RCoordRow
parseRCoordRow = do
  spaces
  i <- P.decimal
  spaces
  j <- P.decimal
  spaces
  x <- P.double
  return $ RCRow i j x

-- | Complex-valued, Coordinate
data CCoordRow =
    CCRow {-# UNBOX #-} !Int
          {-# UNBOX #-} !Int
          {-# UNBOX #-} !(Complex Double) deriving (Eq, Show)

parseCCoordRow :: Parser B.ByteString CCoordRow
parseCCoordRow = do
  spaces
  i <- P.decimal
  spaces
  j <- P.decimal
  spaces
  re <- P.double
  spaces
  im <- P.double  
  return $ CCRow i j (re :+ im)  







parseComments :: Parser B.ByteString [String]
parseComments = P.sepBy parseCommentLine P.endOfLine

parseCommentLine :: Parser B.ByteString String
parseCommentLine = do
  commentS
  P.many' (P.letter_ascii <|> P.space <|> commentS)


commentS :: P.Parser Char
commentS = P.char '%'

spaces = P.many' P.space




-- data Row a = Row { artistName :: String,
--                    dailyStreams1 :: V.Vector a,  -- ^ 1st obs.period
--                    dailyStreams2 :: V.Vector a   -- ^ 2nd obs.period
--                  } deriving (Eq, Show)


-- -- | Parser for a single row
-- parseRow :: Parser B.ByteString (Row Int)
-- parseRow = do
--   n <- parseName <* delim
--   d1 <- P.count obsLen (P.decimal <* delim)  -- first obsLen entries
--   d2 <- P.sepBy P.decimal delim              -- remaining entries
--   return $ Row n (V.fromList d1) (V.fromList d2)
--   where
--     delim = P.char ';'
--     parseName = P.many' (P.letter_ascii <|>
--                          P.space <|>
--                          P.char '\'' <|>
--                          P.char '-')


-- -- | Parser for the whole file
-- parseRows :: Parser B.ByteString (V.Vector (Row Int))
-- parseRows = V.fromList <$> P.sepBy parseRow P.endOfLine <* P.endOfInput
