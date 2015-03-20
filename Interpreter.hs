{- Assignment 2 - A Racket Interpreter

This module is the main program for the interpreter.
All of your work should go into this file.

We have provided a skeleton interpreter which can be run
successfully on sample.rkt; it is your job to extend this
program to handle the full range of Paddle.

In the space below, please list your group member(s):
<Full name>, <CDF Account>
<Full name>, <CDF Account>
-}

module Interpreter (main) where

import BaseParser (BaseExpr(LiteralInt, LiteralBool, Atom, Compound), parseFile)
import Data.List
import System.Environment (getArgs)


-- |Run interpreter on an input file,
--  either from commandline or user input.
--  You should not need to change this function.
main :: IO ()
main =
    getArgs >>= \args ->
    if length args > 0
    then
        parseFile (head args) >>= \baseTree ->
        putStr (interpretPaddle baseTree)
    else
        putStrLn "Enter the name of a file: " >>
        getLine >>= \file ->
        parseFile file >>= \baseTree ->
        putStr (interpretPaddle baseTree)


-- |Take the output of the base parser and interpret it,
--  first constructing the AST, then evaluating it,
--  and finally returning string representations of the results.
--  You will need to make this function more robust against errors.
interpretPaddle :: Maybe [BaseExpr] -> String
interpretPaddle (Just exprs) =
    let ast = map parseExpr exprs
        vals = map evaluate ast
    in
        -- String representations of each value, joined with newlines
        unlines (map show vals)


-- An expression data type
data Expr = Number Integer |
            Boolean Bool |
            If Expr Expr Expr |
-- + , -, *, >= , ==
		AddOp Expr Expr |
	SubOp Expr Expr |
	MultOp Expr Expr |
	GreaterThan Expr Expr |
	Equal Expr Expr |
--  and, or, if , cond
	And Expr Expr |
	Or Expr Expr
	

	

instance Show Expr where
    show (Number x) = show x
    show (Boolean True) = "#t"
    show (Boolean False) = "#f"
    -- Note: the following definition is not necessary for this assignment,
    -- but you may find it helpful to define string representations of all
    -- expression forms.
    show (If e1 e2 e3) =
        "(if " ++ show e1 ++ " " ++ show e2 ++ " " ++ show e3 ++ ")"
    show (AddOp x y) = 
	"(+ " ++ show x ++ " " ++ show y ++ " " ++ ")"
    show (SubOp x y) = 
	"(- " ++ show x ++ " " ++ show y ++ " " ++ ")"
    show (MultOp x y) = 
	"(* " ++ show x ++ " " ++ show y ++ " " ++ ")"
    show (GreaterThan x y) = 
	"(< " ++ show x ++ " " ++ show y ++ " " ++ ")"
    show (Equal x y) = 
	"(equal? " ++ show x ++ " " ++ show y ++ " " ++ ")"
	-- and,or,if ,cond 
--	show (And x y) = 
--	"(and " ++ show x ++ " " ++ show y ++ " " ++ ")"

-- |Take a base tree produced by the starter code,
--  and transform it into a proper AST.
parseExpr :: BaseExpr -> Expr
parseExpr (LiteralInt n) = Number n
parseExpr (LiteralBool b) = Boolean b
parseExpr (Compound [Atom "if", b, x, y]) =
    If (parseExpr b) (parseExpr x) (parseExpr y)
-- +,-,*,<,==
parseExpr (Compound [Atom "+", LiteralInt x, LiteralInt y]) =
	AddOp (Number x) (Number y)
parseExpr (Compound [Atom "-", LiteralInt x, LiteralInt y]) =
	SubOp (Number x) (Number y)
parseExpr (Compound [Atom "*", LiteralInt x, LiteralInt y]) =
	MultOp (Number x) (Number y)
parseExpr (Compound [Atom "<", LiteralInt x, LiteralInt y]) =
	GreaterThan (Number x) (Number y)
parseExpr (Compound [Atom "equal?", LiteralInt x, LiteralInt y]) =
	Equal (Number x) (Number y)
parseExpr (Compound [Atom "equal?", LiteralBool x, LiteralBool y]) =
	Equal (Boolean x) (Boolean y)
-- and,or,if,cond
parseExpr (Compound [Atom "and", x, y]) =
	And (parseExpr x) (parseExpr y)
parseExpr (Compound [Atom "or", x, y]) =
	Or (parseExpr x) (parseExpr y)
-- |Evaluate an AST by simplifying it into
--  a number, boolean, list, or function value.
evaluate :: Expr -> Expr
evaluate (Number n) = Number n
evaluate (Boolean b) = Boolean b
evaluate (If cond x y) =
    case cond of
        Boolean True -> x
        Boolean False -> y
evaluate (AddOp (Number x) (Number y)) = 
	Number (x + y)
evaluate (SubOp (Number x) (Number y)) = 
	Number (x - y)
evaluate (MultOp (Number x) (Number y)) = 
	Number (x * y)
evaluate (GreaterThan (Number x) (Number y)) = 
	Boolean (x < y)
evaluate (Equal (Boolean x) (Boolean y)) = 
	Boolean (x == y)
evaluate (Equal (Number x) (Number y)) = 
	Boolean (x == y)
-- and,or,cond
evaluate (And (Boolean x) (Boolean y)) = 
	Boolean (x&&y)
evaluate (Or (Boolean x) (Boolean y)) = 
	Boolean (x||y) 
