module Main where

import Test.Hspec (hspec, Spec, describe, it, shouldBe, shouldNotBe)
import SequenceSearch (writeLn)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "sequence-search" $ do
    it "match one" $ do
      writeLn ["A"] "A" `shouldBe` "{A}"

    it "match different" $ do
      writeLn ["A B"] "A A" `shouldBe` "A A"

    it "second match" $ do
      writeLn ["A B", "C"] "A C" `shouldBe` "A {C}"
