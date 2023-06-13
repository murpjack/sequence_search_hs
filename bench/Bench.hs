module Main (main) where

import Control.Exception (evaluate)
import System.TimeIt (timeIt)
import SequenceSearch (writeLn)

main = do
  putStrLn "Benchmarks about to start!"
  v <- timeIt $ evaluate $
      sum $
      replicate 10000000000 $
      length $
      writeLn
        [ "Lorem ipsum dolor sit amet"
        , "nostrud exercitation ullamco laboris"
        , "Ut enim ad minim veniam"] $
      unlines
        [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit,"
        , "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. "
        , "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris "
        , "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in "
        , "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. "
        , "Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt "
        , "mollit anim id est laborum."
        ]

  print v
  putStrLn "Benchmark complete."


