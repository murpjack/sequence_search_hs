module SequenceSearchTrie (writeLn) where

import qualified Data.Map as Map
import Control.Applicative ((<|>))
import Data.Map (Map)
import Data.Foldable (fold)

writeLn :: [String] -> String -> String
writeLn needles haystack = go haystack
  where
  trie = toTrie needles

  go "" = ""
  go hay@(x:xs) =
    case longestMatch hay trie of
      Nothing -> x : go xs
      Just m -> wrap m ++ go (drop (length m) hay)

wrap a = "{" <> a <> "}"

toTrie :: [String] -> Trie
toTrie = fold . map new

data Trie = Trie Bool (Map Char Trie)

new :: String -> Trie
new "" = Trie True mempty
new (x:xs) = Trie False (Map.singleton x (new xs))

instance Semigroup Trie where
  Trie ta a <> Trie tb b = Trie (ta || tb) (Map.unionWith (<>) a b)

instance Monoid Trie where
  mempty = Trie False mempty
  mappend a b = a <> b

longestMatch :: String -> Trie -> Maybe String
longestMatch str (Trie isTerminal next) = following <|> this
  where
  this = if isTerminal then Just "" else Nothing
  following =
    case str of
      "" -> Nothing
      x:xs -> do
        m <- Map.lookup x next
        w <- longestMatch xs m
        return (x : w)
