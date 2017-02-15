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
      isConsistent x `shouldBe` True
    it "memplus : imports all matrix entries" $ do 
      x <- readMatrix "data/memplus.mtx"
      isConsistent x `shouldBe` True
    it "memplus_rhs1 : imports all array entries" $ do 
      x <- readArray "data/memplus_rhs1.mtx"
      isConsistentArr x `shouldBe` True    


isConsistent :: Matrix t -> Bool
isConsistent m = nnz m == numDat m

isConsistentArr mm = d == numDatArr mm where
  (m,n) = dimArr mm
  d = m*n
