module Main (main) where

import Control.Exception (evaluate)
import System.TimeIt (timeItNamed)
import qualified SequenceSearch
import qualified SequenceSearchText
import qualified Data.Text as Text
import Data.Text (Text)
import System.Timeout (timeout)

main = do
  putStrLn "Benchmarks about to start!"
  () <- timeItNamed "SequenceSearch" $ withTimeout $ evaluate $
      sum $
      map (length . SequenceSearch.writeLn needles) $
      replicate 100 haystack

  () <- timeItNamed "SequenceSearchText" $ withTimeout $ evaluate $
      sum $
      map (Text.length . SequenceSearchText.writeLn needlesTxt) $
      replicate 100 haystackTxt

  putStrLn "Benchmark complete."
  where

  withTimeout :: Show a => IO a -> IO ()
  withTimeout act = do
    r <- timeout (10 * 1000000) act
    case r of
      Nothing -> putStrLn "Timed out!"
      Just x -> print x

needlesTxt :: [Text]
needlesTxt = map Text.pack needles

needles :: [String]
needles = map toNeedle [1..100]
  where
  toNeedle n = take n $ drop n haystack

haystackTxt :: Text
haystackTxt = Text.pack haystack

haystack :: String
haystack = unwords $ concat $ replicate 1000
  [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit,"
  , "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. "
  , "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris "
  , "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in "
  , "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. "
  , "Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt "
  , "mollit anim id est laborum."
  ]

