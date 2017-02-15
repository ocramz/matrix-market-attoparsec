-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Matrix.MatrixMarket
-- Copyright   :  (c) Marco Zocca 2017
-- License     :  GPL-3 (see the file LICENSE)
--
-- Maintainer  :  zocca marco gmail
-- Stability   :  experimental
-- Portability :  portable
--
-- Attoparsec parser and serializer for the NIST MatrixMarket format. The parser logic originally appeared in `accelerate-examples` and it is reused here (courtesy of T.McDonell and the `accelerate` developers) with some amendments.
-- 
-- In this version:
--
-- *) Numbers are represented with Scientific notation instead of floating point
--
-- *) Parsing rules are a bit relaxed to accommodate various whitespace corner cases
--
-----------------------------------------------------------------------------
module Data.Matrix.MatrixMarket
       (-- * Load
        readMatrix, readArray,
        -- * Save
        writeMatrix, writeArray,
        -- 
        Matrix(..), Array(..),
        Format (Coordinate, Array), Structure (General, Symmetric, Hermitian, Skew),
        -- * Helpers
        -- ** Matrix-related
        nnz, dim, numDat,
        -- ** Array-related
        dimArr, numDatArr) where

import Data.Matrix.MatrixMarket.Internal as M
