module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Lib 

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    -- it "works" $ do
    --   True `shouldBe` True
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x
    it "fidapm05 : imports all matrix entries" $ do 
      x <- readMatrix "data/fidapm05.mtx"
      consistentDims x `shouldBe` True
    it "memplus : imports all matrix entries" $ do 
      x <- readMatrix "data/memplus.mtx"
      consistentDims x `shouldBe` True
    it "memplus_rhs1 : imports all array entries" $ do 
      x <- readArray "data/memplus_rhs1.mtx"
      consistentDimsArr x `shouldBe` True    


-- | Helpers

-- check if matrix dimensions (read from header vs counted from memory) coincide
consistentDims :: Matrix t -> Bool
consistentDims m = nnz m == numDat m

-- check if array dimensions (read from header vs counted from memory) coincide
consistentDimsArr :: Array a -> Bool
consistentDimsArr mm = d == numDatArr mm where
  (m,n) = dimArr mm
  d = m*n
