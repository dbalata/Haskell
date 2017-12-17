-- Dylan Balata
-- Phuong Tran

module SchemeParser where

import Parselib
import Data.Char

data Sexpr = Symbol String | Number Int | Nil | Cons Sexpr Sexpr

instance Show Sexpr where
  show (Symbol x) = x
  show (Number x) = show x
  show Nil = "()"
  show (Cons x y) = "(" ++ show x ++ showCdr y ++ ")"
  
showCdr :: Sexpr -> String
showCdr Nil = ""
showCdr (Cons x Nil) = " " ++ show x
showCdr (Cons x v@(Cons y z)) = " " ++ show x ++ showCdr v
showCdr (Cons x y) = " " ++ show x ++ " . " ++ show y
showCdr x = " . " ++ show x

parl = symb "("
parr = symb ")"

foo' = "(define fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1))))))"

putParens s = Cons s Nil
  
first = misc +++ lower

misc =
  char '<' +++
  char '>' +++
  char '^' +++
  char '+' +++
  char '-' +++
  char '*' +++
  char '=' +++
  char '/'

symbolic = do{f <- first; return f;} +++
           do{d <- (sat isDigit); return d;}

symbol = do{f <- first; m <- (many symbolic); return ([f] ++ m);}

number = many1 digit

e =
  do{parl; e1 <- e; parr; e2 <- e; return (Cons e1 (e2));} +++
  do{s1 <- s; space; e1 <- e; return (Cons s1 (e1));} +++
  do{s1 <- s; return (putParens s1);}

a = do{s <- symbol; return (Symbol s);} +++
    do{n <- number; return (Number (read $ concatMap show n));}

s =
  do{parl; parr; return (Nil);} +++
  do{parl; e1 <- e; parr; return (e1)} +++
  do{a1 <- a; return (a1)} +++
  do{parl; s1 <- s; char '.'; s2 <- s; parr; return (Cons s1 (s2));}

parseSexpr x = fst $ head (parse (s +++ a +++ e) x)
