{-# language OverloadedStrings #-}
module Lib where


import Control.Applicative
import qualified Data.Vector as V

-- import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

import Data.Attoparsec.Internal.Types (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P (decimal, scientific, letter_ascii, char, endOfLine, endOfInput, parseOnly, space, count)
import qualified Data.Attoparsec.ByteString.Lazy as P





data MRepr = Coordinate | Array deriving (Eq, Show)

-- | Numerical format : Real / Integer / Complex 
data MNumFormat = R | I | C deriving (Eq, Show)

data MSymm = General | Symmetric | SkewSymmetric | Hermitian deriving (Eq, Show)



commentS :: P.Parser Char
commentS = P.char '%'

parseHeader :: P.Parser B.ByteString
parseHeader = P.string "%%MatrixMarket matrix"



parseComment :: Parser B.ByteString [Char]
parseComment = do
  commentS
  P.many' (P.letter_ascii <|> P.space <|> commentS)
  



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
