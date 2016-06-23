module Main where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Test stub" $ do
    it "Test stub" $ do
      2 + 2 `shouldBe` 4
