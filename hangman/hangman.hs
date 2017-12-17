import Data.List.Split
import System.Random
import Data.Char
import Data.List
import Text.Printf
import System.IO

totalGuesses = 6

main = do
  dict' <- readFile "dict.txt"
  let dict = splitOn "|" dict'
  mapM (\s -> putStr (s ++ " ")) dict
  putStrLn ""
  idx <- randomRIO(0, length dict)
  let answer = dict !! idx
  gameLoop totalGuesses "" (trimSpaces answer)

gameLoop numTries guesses answer =
  if numTries == 0 then print $ "You Lose. Answer was: " ++ answer
  else if not (elem False (map (\c -> elem c guesses) answer))
  then print $ "You win. Answer: " ++ answer
  else
    do
      printMan numTries
      putStrLn $ map (\c -> if elem c guesses then c else '_') answer
      putStrLn $ "Letters guessed: " ++ guesses
      putStrLn $ "Tries remaining: " ++ show numTries ++ "\nGuess a letter"
      guess' <- getLine
      let guess = trimGuess guess'
      if elem guess guesses || not (isValid guess) then
        gameLoop numTries guesses answer
      else if elem guess answer then gameLoop numTries (guesses ++ [guess]) answer
      else gameLoop (numTries - 1) (guesses ++ [guess]) answer


isValid c = (ord c <= ord 'z') && (ord c >= ord 'a')

trimGuess s = if (s == "") then ' ' else head s

trimSpaces s = takeWhile isValid s

printMan 6 = do 
  putStrLn "____"
  putStrLn "|   "
  putStrLn "|   "
  putStrLn "|   "
  
printMan 5 = do 
  putStrLn "____"
  putStrLn "| o "
  putStrLn "|   "
  putStrLn "|   "
  
printMan 4 = do 
  putStrLn "____"
  putStrLn "| o "
  putStrLn "| | "
  putStrLn "|   "
  
printMan 3 = do 
  putStrLn "____"
  putStrLn "| o "
  putStrLn "|-| "
  putStrLn "|   "

printMan 2 = do 
  putStrLn "____"
  putStrLn "| o "
  putStrLn "|-|-"
  putStrLn "|   "

printMan 1 = do 
  putStrLn "____"
  putStrLn "| o "
  putStrLn "|-|-"
  putStrLn "|/  "
  
printMan 0 = do 
  putStrLn "____"
  putStrLn "| o "
  putStrLn "|-|-"
  putStrLn "|/ \\"
  
