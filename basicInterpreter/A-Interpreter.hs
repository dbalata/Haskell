import Control.Monad.State
import Data.Maybe
import Data.List.Split
import Parselib
import Data.List
import Numeric
import Data.Char
import System.Random

data Interp = Interp { table :: [(String, String)],
                       source :: [String],
                       lineNum :: Int } deriving Show

main = do
  f <- readFile "ham.txt"
  let src = (tail $ lines f)
  runStateT interpLine (Interp [] src 0)
  return ()

interpLine :: StateT Interp IO ()
interpLine = do
  interp <- get
  let line = source interp !! lineNum interp
  --liftIO $ print line
  --liftIO $ print (table interp)
  case (splitOn " " line) !! 1 of
    "input" -> interpInput
    "let" -> interpLet
    "print" -> interpPrint True
    "end" -> interpEnd
    "if" -> interpIf
    "goto" -> interpGoto
    "gosub" -> interpGosub
    "return" -> interpReturn
    "for" -> interpFor
    "next" -> interpNext
    "print!" -> interpPrint False


interpNext :: StateT Interp IO ()
interpNext = do
  interp <- get
  let s = source interp !! lineNum interp
  let var = fst $ head (apply parseNext s)
  let look = fromJust $ lookup var (table interp)
  let inc = (read look :: Int) + 1
  let newTable = addToTable var (show inc) (table interp)
  let forLine = fromJust $ lookup ("for" ++ var) newTable
  let bound = fromJust $ lookup "bound" newTable
  if inc > (read bound :: Int)
    then put $ Interp newTable (source interp) ((lineNum interp) + 1)
    else put $ Interp newTable (source interp) (read forLine :: Int)
  --return ()
  interpLine

parseNext = do
  symb "("
  nat
  symb "next"
  id <- token identifier
  return id

data ForData = ForData {var :: String, initial :: Int, bound :: Int, line :: Int}
  deriving Show

interpFor :: StateT Interp IO ()
interpFor = do
  interp <- get
  let s = source interp !! lineNum interp
  let forParse = fst $ head (apply parseFor s)
  let table0 = addToTable (var forParse) (show (initial forParse)) (table interp)
  let table1 = addToTable "bound" (show (bound forParse)) (table0)
  let finalTable = addToTable ("for" ++ (var forParse)) (show $ (lineNum interp) + 1) table1
  put $ Interp finalTable (source interp) ((lineNum interp) + 1)
  interpLine
  

parseFor = do
  symb "("
  line <- nat
  symb "for"
  var <- token identifier
  symb "="
  initial <- int
  symb "to"
  bound <- nat
  return $ ForData var initial bound line

  
interpReturn :: StateT Interp IO ()
interpReturn = do
  interp <- get
  let newLine = fromJust $ lookup "return" (table interp)
  put $ Interp (table interp) (source interp) ((read newLine :: Int) + 1)
  interpLine

interpGosub :: StateT Interp IO ()
interpGosub = do
  interp <- get
  let s = source interp !! lineNum interp
  let return = fst $ head (apply parseCurrentLineNum s)
  let next = fst $ head (apply parseGosubLineNum s)
  let convertedNext = convertLineNum (show next) (source interp) 0
  let convertedReturn = convertLineNum (show return) (source interp) 0
  let newTable = addToTable "return" (show convertedReturn) (table interp)
  put $ Interp newTable (source interp) (convertedNext)
  interpLine

parseCurrentLineNum = do{symb "("; n <- natural; return n;}

parseGosubLineNum = do{symb "("; natural; space; symb "gosub"; n <- natural; return n;}

interpGoto :: StateT Interp IO ()
interpGoto = do
  interp <- get
  let s = source interp !! lineNum interp
  let lineNum = fst $ head (apply parseGoto s)
  let newLine = convertLineNum (show lineNum) (source interp) 0
  put $ Interp (table interp) (source interp) (newLine)
  interpLine

parseGoto = do
  symb "("
  natural
  space
  string "goto"
  space
  n <- natural
  return n

interpIf :: StateT Interp IO ()
interpIf = do
  interp <- get
  let s = source interp !! lineNum interp
  let replaced =(initReplaceSymbols s (table interp))
  --liftIO $ print replaced
  let evaled = getBool replaced
  let action = getAction replaced  
  let jumpLine = convertLineNum action (source interp) 0
  if evaled then put $ Interp (table interp) (source interp) (jumpLine)
  else put $ Interp (table interp) (source interp) (lineNum interp + 1)
  if evaled && action == "end" then return () else interpLine

convertLineNum oldNum src newNum = do
  let s = src !! newNum
  let check = head $ splitOn " " s
  if check == '(' : oldNum then newNum
    else convertLineNum oldNum src (newNum + 1)

getBool s =
  fst $ head (apply parseBool s)

parseBool = do
  symb "("
  nat
  symb "if"
  symb "("
  expr0 <- parseExpr
  space
  op <- many (sat (/= ' '))
  space
  expr1 <- parseExpr
  space
  symb ")"
  return $ getBoolOp op (read expr0 :: Float) (read expr1 :: Float)

getBoolOp "=" = (==)
getBoolOp ">" = (>)
getBoolOp "<" = (<)
getBoolOp ">=" = (>=)
getBoolOp "<=" = (<=)
getBoolOp "<>" = (/=)

getAction s =
  fst $ head (apply parseAction s)

parseAction = do
  parseBool
  space
  symb "then"
  space
  action <- many (sat (/= ' '))
  return $ action

interpEnd :: StateT Interp IO ()
interpEnd = do
  return ()

interpPrint :: Bool -> StateT Interp IO ()
interpPrint newLine = do
  interp <- get
  let s = source interp !! lineNum interp
  let replaced = initReplaceSymbols s (table interp)
  let out = (fst $ head (apply parsePrint replaced))
  if newLine then liftIO $ putStrLn ( out)
    else liftIO $ putStr (out)
  put $ Interp (table interp) (source interp) (lineNum interp + 1)
  interpLine

initReplaceSymbols s table =
  concatMap (++ " ") $ replaceSymbols (splitOnValues s) table False []

replaceSymbols [] _ _ acc = reverse acc
replaceSymbols (c:cs) table inQ acc =
  if c == "\"" then replaceSymbols cs table (inQ) (c : acc)
  else if inQ then replaceSymbols cs table inQ (c : acc)
  else
    case lookup c table of
      Just k -> replaceSymbols cs table inQ (k : acc)
      Nothing -> replaceSymbols cs table inQ (c : acc)

splitOnValues s = splitOn " " (insertSpaces s [])

insertSpaces [] acc = acc
insertSpaces (c:cs) acc =
  if (c == ')' || c == '(') then insertSpaces cs (acc ++ " " ++ [c] ++ " ")
  else insertSpaces cs (acc ++ [c])

parsePrint = do
  space
  symb "("
  natural
  space
  many $ sat (\c -> isAlpha c || c == '!')
  space
  p <- many parsePrintable
  return $ concat p

parsePrintable = do
  parseQuote +++ parseExpr +++ parseTab

parseTab = do
  symb "tab("
  n <- nat
  symb ")"
  return $ replicate n ' '

parseQuote = do
  space
  char '"'
  quote <- many (sat (/= '"'))
  symb "\""
  return quote

arithmetic =
  (char '+') +++
  (char '-') +++
  (char '*') +++
  (char '/') 

getOp '+' = (+)
getOp '-' = (-)
getOp '*' = (*)
getOp '/' = (/)


parseExpr =
  --do{space; i <- digit; space; return $ show i;}
  -- +++
  do{x <- parseExpr0; return $ showFFloat Nothing x "";}
  -- +++
  --do{space; symb "int"; e <- parseExpr0; return $ show (round e);}
    

parseExpr0 = do{
  space;
  symb "(";
  expr1 <- parseExpr0;
  space;
  op <- arithmetic;
  space;
  expr2 <- parseExpr0;
  space;
  symb ")";
  return $ ((getOp op) expr1 expr2);}
  +++
  do{space; symb "("; e <- parseExpr0; symb ")"; return e}
  +++
  do{space; a <- float; return a}
  +++
  do{space; f <- parseFunction; return f}
  +++
  do{space; symb "int"; e <- parseExpr0; return $ fromIntegral (round e);}
  -- +++
  --do{space; fName <- many1 alphanum; space; e <- parseExpr0; return $ getFun fName e;}

parseFunction = do
  f <- parseFun
  space
  symb "("
  n <- float
  symb ")"
  return $ getFun f n

parseFun = do
  s <- string "log" +++
    string "rnd" +++
    string "abs" +++
    string "sqrt" +++
    string "sin" +++
    string "cos" +++
    string "tan" +++
    string "asin" +++
    string "acos" +++
    string "atan"
  return s

getFun :: String -> Float -> Float
getFun "log" = log
getFun "rnd" = \x -> fromIntegral $ mod getRand (round (x))
getFun "abs" = abs
getFun "sqrt" = sqrt
getFun "sin" = sin
getFun "cos" = cos
getFun "tan" = tan
getFun "asin" = asin
getFun "acos" = acos
getFun "atan" = atan
getFun "int" = fromIntegral . floor

getRand = fst $ next (mkStdGen 123)

interpInput :: StateT Interp IO ()
interpInput = do
  interp <- get
  let s = source interp !! lineNum interp
  let prompt = takeWhile (/= '"') $ tail (dropWhile (/= '"') s)
  let symbol = reverse $ takeWhile (/= ' ') (tail $ tail (reverse s))
  liftIO (print prompt)
  response <- liftIO getLine
  put $ Interp ((symbol, response) : (table interp)) (source interp) (lineNum interp + 1)
  interpLine

interpLet :: StateT Interp IO ()
interpLet = do
  interp <- get
  let s = source interp !! lineNum interp
  let symbol = splitOn " " s !! 2
  let expr = (tail $ tail (dropWhile (/= '=') s))
  let result = fst $ head (apply parseExpr (initReplaceSymbols expr (table interp)))
  put $ Interp (addToTable symbol (result) (table interp))(source interp) (lineNum interp + 1)
  interpLine

addToTable key val [] = [(key, val)]
addToTable key val (p@(k, _):rest) =
  if k == key then (k, val) : rest
  else p : addToTable key val rest 


float = do
  first <- int
  p <- char '.'
  second <- many1 alphanum
  let f = (read ((show first) ++ [p] ++ (second)) :: Float)
  return f 
  +++
    do
      i <- integer
      return (fromIntegral i)
