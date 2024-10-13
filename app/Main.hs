module Main (main) where

import Data.Char
import Data.List

-- refs: https://stackoverflow.com/questions/16648208/expression-evaluation-tree-in-haskell
-- refs: https://media.ed.ac.uk/playlist/dedicated/179956591/0_wp5oihez/1_3rbyyctu

-- Pretty neat, huh?!

main :: IO ()   -- outputs: "(42+(42*(10-12))) = -42" 
main = print ( (showExpr(Lit 42 :+: (Lit 42 :*: (Lit 10 :-: Lit 12))))  ++ " = " ++
			   (toString (evalExpr(Lit 42 :+: (Lit 42 :*: (Lit 10 :-: Lit 12))))) )

-- is this an algebraic data type, or a recursive data type?

data Expr = Lit Int
		  | Expr :+: Expr
		  | Expr :-: Expr
		  | Expr :*: Expr
		  deriving(Eq, Show)

evalExpr :: Expr -> Int
evalExpr (Lit x) = x
evalExpr (a :+: b) = evalExpr a + evalExpr b
evalExpr (a :-: b) = evalExpr a - evalExpr b
evalExpr (a :*: b) = evalExpr a * evalExpr b

-- Remember: ++ is concat
addParrs :: String -> String
addParrs x = "(" ++ x ++ ")"

showExpr :: Expr -> String
showExpr (Lit x) = show x
showExpr (a :+: b) = addParrs (showExpr a ++ "+" ++ showExpr b)
showExpr (a :-: b) = addParrs (showExpr a ++ "-" ++ showExpr b)
showExpr (a :*: b) = addParrs (showExpr a ++ "*" ++ showExpr b)

toString :: Int -> String
toString x = show x