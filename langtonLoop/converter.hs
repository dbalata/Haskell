import Data.List.Split

main = do
  let file' = readFile "table.txt"
  file <- file'
  let list = splitOn "\n" file
  let result = concatMap (\word -> ("transition \"" ++ (take 5 word) ++ "\" = "
                            ++ "'" ++ (drop 5 word) ++ "'\n")) list
  writeFile "Transition.hs" result
  return ()
