module Main where

import Test.Hspec (hspec, Spec, describe, it, shouldBe, shouldNotBe)
import SequenceSearch (writeLn)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "sequence-search" $ do
    it "basic 1" $ do
      writeLn ["A"] "A" `shouldBe` "{A}"
