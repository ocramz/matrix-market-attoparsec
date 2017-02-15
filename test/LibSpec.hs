module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Lib 

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x
    it "imports all matrix entries" $ do 
      x <- readMatrix "data/fidapm05.mtx"
      isConsistent x `shouldBe` True


isConsistent :: Matrix t -> Bool
isConsistent m = nnz m == numDat m
