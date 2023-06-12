module Main where

import Data.List as List
import qualified Data.Tuple as Tuple
import qualified System.Environment as SysEnv
import qualified System.IO as SysIO

main :: IO ()
main = do
  args <- SysEnv.getArgs
  block <- toLines (List.head args)
  exprs <- toLines "./words_en_2000.txt"
  putStrLn $ List.unlines $ List.map (writeLn exprs) block

toLines :: String -> IO [String]
toLines filename = List.lines <$> SysIO.readFile filename

wrapExpr :: String -> String
wrapExpr e = "{" ++ e ++ "}"

-- Can find matches. Can not handle where multiple patterns begin the same
writeLn :: [String] -> String -> String
writeLn ptns =
  Tuple.fst
    . List.foldl
      ( \(ln, match) t ->
          if List.null match
            then case List.find (\ptn -> List.head ptn == t) ptnLs of
              Just ptn ->
                if List.length ptn > 1
                  then (ln, List.head ptn : match)
                  else (ln ++ " " ++ wrapExpr t, [])
              Nothing ->
                (ln ++ " " ++ t, [])
            else case List.find (\ptn -> (match ++ [t]) `isPrefixOf` ptn) ptnLs of
              Just ptn ->
                if List.length ptn > List.length (match ++ [t])
                  then (ln, match ++ [t])
                  else (ln ++ " " ++ wrapExpr (List.unwords ptn), [])
              Nothing -> (ln ++ " " ++ wrapExpr (List.unwords match) ++ " " ++ t, [])
      )
      ("", [])
    . List.words
  where
    ptnLs :: [[String]]
    ptnLs = List.map List.words ptns
