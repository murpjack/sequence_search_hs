module Main where

import Test.Hspec (hspec, Spec, describe, it, shouldBe, shouldNotBe)
import qualified SequenceSearch
import qualified SequenceSearchText
import qualified Data.Text as Text


main :: IO ()
main = hspec $ do
  describe "SequenceSearch" $ spec SequenceSearch.writeLn
  describe "SequenceSearchText" $ spec $ \needles haystack ->
    Text.unpack $ SequenceSearchText.writeLn (map Text.pack needles) (Text.pack haystack)

spec writeLn = do
  it "match one" $ do
    writeLn ["A"] "A" `shouldBe` "{A}"

  it "match different" $ do
    writeLn ["A B"] "A A" `shouldBe` "A A"

  it "second match" $ do
    writeLn ["A B", "C"] "A C" `shouldBe` "A {C}"

  it "second match" $ do
    writeLn ["A", "A B", "A B C"] "A A B A B C" `shouldBe` "{A} {{A} B} {{{A} B} C}"
