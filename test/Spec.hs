module Main where

import Test.Hspec (hspec, Spec, describe, it, shouldBe, shouldNotBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "sequence-search" $ do
    it "works" $ do
        "A" `shouldNotBe` "A"
