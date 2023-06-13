module Main where

import qualified System.Environment as SysEnv
import qualified System.IO as SysIO
import qualified SequenceSearch

main :: IO ()
main = do
  args <- SysEnv.getArgs
  block <- toLines (List.head args)
  exprs <- toLines "./words_en_2000.txt"
  putStrLn $ List.unlines $ List.map (SequenceSearch.writeLn exprs) block

toLines :: String -> IO [String]
toLines filename = List.lines <$> SysIO.readFile filename


