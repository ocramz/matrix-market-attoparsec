module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Matrix.MatrixMarket

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Data.Matrix.MatrixMarket" $ do
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x
    it "fidapm05 : imports all matrix entries" $ do 
      x <- readMatrix "data/fidapm05.mtx"
      consistentDims x `shouldBe` True
    it "fidapm05_rhs1 : imports all array entries" $ do 
      x <- readArray "data/fidapm05_rhs1.mtx"
      consistentDimsArr x `shouldBe` True  
    -- it "memplus : imports all matrix entries" $ do 
    --   x <- readMatrix "data/memplus.mtx"
    --   consistentDims x `shouldBe` True
    -- it "memplus_rhs1 : imports all array entries" $ do 
    --   x <- readArray "data/memplus_rhs1.mtx"
    --   consistentDimsArr x `shouldBe` True    


-- | Helpers

-- check if matrix dimensions (read from header vs counted from memory) coincide
consistentDims :: Matrix t -> Bool
consistentDims m = nnz m == numDat m

-- check if array dimensions (read from header vs counted from memory) coincide
consistentDimsArr :: Array a -> Bool
consistentDimsArr mm = d == numDatArr mm where
  (m,n) = dimArr mm
  d = m*n



roundTrip fname ftemp = do
  let fname2 = fname ++ ftemp
  m0 <- readMatrix fname
  writeMatrix fname2 m0
  m1 <- readMatrix fname2
  m0 `shouldBe` m1
