module SequenceSearch (writeLn) where

import Data.List as List
import qualified Data.Tuple.Extra as Tuple

wrapExpr :: String -> String
wrapExpr e = "{" ++ e ++ "}"

-- Finds matches; and when many patterns begin the same.
-- TODO: Can not match where uppercase chars. Should it?
-- TODO: Can not match where punctuation exists
writeLn :: [String] -> String -> String
writeLn ptns =
  Tuple.fst3
    . List.foldl
      ( \(ln, cache, partMatches) t ->
          if List.null cache
            then case List.findIndices (\ptn -> List.head ptn == t) ptnLs of
              [] -> (ln ++ " " ++ t, [], [])
              [n] ->
                let ptn = ptnLs !! n
                 in if List.length ptn > 1
                      then (ln, [List.head ptn], [ptnLs !! n])
                      else (ln ++ " " ++ wrapExpr t, [], [])
              ns ->
                let ptn = ptnLs !! List.head ns
                 in (ln, [List.head ptn], findMatches ptnLs ns)
            else case List.findIndices (\ptn -> (cache ++ [t]) `isPrefixOf` ptn) partMatches of
              [] -> case List.findIndices (\ptn -> List.head ptn == t) ptnLs of
                [] -> (ln ++ " " ++ wrapExpr (List.unwords cache) ++ " " ++ t, [], [])
                [n] ->
                  let ptn = ptnLs !! n
                   in if List.length ptn > 1
                        then (ln, [List.head ptn], [ptnLs !! n])
                        else (ln ++ " " ++ wrapExpr (List.unwords cache) ++ " " ++ wrapExpr t, [], [])
                ns ->
                  let ptn = ptnLs !! List.head ns
                   in (ln, [List.head ptn], findMatches ptnLs ns)
              [n] ->
                let ptn = partMatches !! n
                 in if List.length ptn > List.length (cache ++ [t])
                      then (ln, cache ++ [t], partMatches)
                      else (ln ++ " " ++ wrapExpr (List.unwords ptn), [], [])
              ns ->
                let fstMatch = partMatches !! List.head ns

                    ptnPrfx = List.take (List.length cache) fstMatch
                 in (ln, ptnPrfx ++ [t], findMatches partMatches ns)
      )
      ("", [], [])
    . List.words
  where
    ptnLs :: [[String]]
    ptnLs = List.map List.words ptns

findMatches :: [[String]] -> [Int] -> [[String]]
findMatches ptns = List.map (ptns !!)
