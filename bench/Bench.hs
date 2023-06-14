module Main (main) where

import Control.Exception (evaluate)
import System.TimeIt (timeIt)
import SequenceSearch (writeLn)
import System.Timeout (timeout)

main = do
  putStrLn "Benchmarks about to start!"
  v <- timeIt $ withTimeout $ evaluate $
      sum $
      map (length . writeLn needles) $
      replicate 100 haystack

  print v
  putStrLn "Benchmark complete."
  where

  withTimeout :: Show a => IO a -> IO ()
  withTimeout act = do
    r <- timeout (10 * 1000000) act
    case r of
      Nothing -> putStrLn "Timed out!"
      Just x -> print x

needles :: [String]
needles = map toNeedle [1..100]
  where
  toNeedle n = take n $ drop n haystack

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

