{-# LANGUAGE OverloadedStrings #-}
module SequenceSearchText (writeLn) where

import Data.Text (Text)
import qualified Data.Text as Text


writeLn :: [Text] -> Text -> Text
writeLn needles haystack = foldr replace haystack needles
  where
  replace needle hay = Text.replace needle (wrap needle) hay
  wrap needle = "{" <> needle <> "}"


