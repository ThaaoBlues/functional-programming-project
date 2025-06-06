-- Add your names and student numbers to the following lines. Do not change anything else on these lines, since it they are parsed.
-- Student 1: Théo Mougnibas (s3581497)
-- Student 2: Iris Borgonjen (s3192393)
-- Student 3: Mirte Verheijen (s3207803)

{-# LANGUAGE TemplateHaskell #-}

module MicroFP where

import Control.Applicative
import PComb
import BasicParsers
import Test.QuickCheck
import Test.QuickCheck.All



-- FP3.1 
-- by Théo Mougnibas


-- ⟨program⟩::=( ⟨function⟩ )+
-- ⟨function⟩::=identifier (identifier | integer) ∗ ’:=’ ⟨expr⟩ ’;’
-- ⟨expr⟩::=⟨term⟩ | ⟨term⟩ ( ’+’ | ’-’ ) ⟨expr⟩
-- ⟨term⟩::=⟨factor⟩ | ⟨factor⟩ ’*’ ⟨term⟩
-- ⟨factor⟩::=integer
-- ⟨ordering⟩
-- |identifier ( ’(’ ⟨expr⟩ (’,’⟨expr⟩) ∗ ’)’)?
-- |’if’ ’(’ ⟨expr⟩ ⟨ordering⟩ ⟨expr⟩ ’)’ ’then’ ’{’ ⟨expr⟩ ’}’ ’else’ ’{’ ⟨expr⟩ ’}’
-- |’(’ ⟨expr⟩ ’)’
-- ::=
-- ’<’ | ’==’ | ’>’

newtype Prog = Program [Procedure]
            deriving Show

data Procedure = Procedure String [Param] Expr
            deriving Show

-- | Represents a parameter which can be an identifier or an integer
data Param = IdParam Expr | IntParam Expr
  deriving Show

data Expr = IntConst Integer
          | Var String
          | Mult Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | If Condition Expr Expr
          | Call String [Param]
          deriving Show

data Condition = Cond Comparator Expr Expr
                deriving Show

data Comparator = Eq | Lt | Gt deriving Show

-- FP3.2
-- by Théo Mougnibas


-- TO CHECK, ONE IMPLEMENTATION OF FIBONACCI GIVES F(0) = 1 AND THE OTHER F(0) = 0 
fibonacci = Program [Procedure "fibonacci" [IntParam (Var "n")] (If (Cond Eq (Var "n") (IntConst 0)) (IntConst 0)
 (If (Cond Eq (Var "n") (IntConst 1)) (IntConst 1) ( Add 
    (Call "fibonacci" [IntParam (Sub (Var "n") (IntConst 1))])
    (Call "fibonacci" [IntParam (Sub (Var "n") (IntConst 2))] )
    )

  ) 
  )]


fib = Program [Procedure "fib" [IntParam (Var "n")] (If (Cond Eq (Var "n") (IntConst 0)) (IntConst 0)
 (If (Cond Eq (Var "n") (IntConst 1)) (IntConst 1) ( Add 
    (Call "fib" [IntParam (Sub (Var "n") (IntConst 1))])
    (Call "fib" [IntParam (Sub (Var "n") (IntConst 2))] )
    )

  ) 
  )]

sumProg = Program [Procedure "sum" [IntParam (Var "a")] (If (Cond Eq (Var "a") (IntConst 0)) (IntConst 0) (Add (Var "a") (Call "sum" [IntParam (Sub (Var "a") (IntConst 1))] )))]

divProg = Program [Procedure "div" [IntParam (Var "x"), IntParam (Var "y")] (
    If (Cond Lt (Var "x") (Var "y"))
       (IntConst 0)
       (Add (IntConst 1) (Call "div" [IntParam (Sub (Var "x") (Var "y")), IntParam (Var "y")]))
  )]

twice =   Program [Procedure "twice" [IdParam (Var "f"), IntParam (Var "x")] (
    Call "f" [IntParam (Call "f" [IntParam (Var "x")])]
  )
  ]



struct = Program [
  Procedure "add" [IntParam (Var "x"), IntParam (Var "y")] (Add (Var "x") (Var "y")),
  Procedure "inc" [IntParam (Var "x")] (Call "add" [IntParam (IntConst 1)]),
  Procedure "eleven" [] (Call "inc" [IntParam (IntConst 10)])
  ]

-- UTILISATION EXAMPLE ???

-- TESTS ???



-- FP3.3
-- by Théo Mougnibas

-- pretty-printing


-- MAIN FUNCTION
-- the one to be called to print a whole program
pretty :: Prog -> String
pretty (Program procedures) = foldr (\x s-> x ++ "\n"++ s) "" $ map prettyProcedure procedures
-- the foldr is to build a global string with all precedures separated by newlines


-- HELPER FUNCTIONS

-- build string representing a procedure data type
prettyProcedure :: Procedure -> String
prettyProcedure (Procedure name params expr) =
  name ++ " " ++ foldr (\x s-> x ++ " "++ s) "" (map prettyParam params) ++ " := " ++ prettyExpr expr ++ ";"
-- the foldr is to build a global string with all parameters separated by spaces


-- build string representing a Param data type
prettyParam :: Param -> String
prettyParam (IdParam expr) = prettyExpr expr
prettyParam (IntParam expr) = prettyExpr expr

-- build string representing a Expr data type
prettyExpr :: Expr -> String
prettyExpr (IntConst i) = show i -- int
prettyExpr (Var s) = s -- simple String identifier
prettyExpr (Mult e1 e2) = "(" ++ prettyExpr e1 ++ " * " ++ prettyExpr e2 ++ ")"
prettyExpr (Add e1 e2) = "(" ++ prettyExpr e1 ++ " + " ++ prettyExpr e2 ++ ")"
prettyExpr (Sub e1 e2) = "(" ++ prettyExpr e1 ++ " - " ++ prettyExpr e2 ++ ")"
prettyExpr (If cond e1 e2) = "if (" ++ prettyCondition cond ++ ")"
    ++ " then {\n\t" ++ prettyExpr e1 ++ "\n}" -- then
    ++ " else {\n\t" ++ prettyExpr e2 ++ "\n}\n" -- else

-- there, we need to fold as we might have multiple parameters
prettyExpr (Call func params) = func ++ "(" ++ foldr (\x s -> prettyParam x ++ s) "" params ++ ")"

-- build string representing a Condition data type
prettyCondition :: Condition -> String
prettyCondition (Cond cmp e1 e2) =
  prettyExpr e1 ++ " " ++ prettyComparator cmp ++ " " ++ prettyExpr e2

-- build string representing a the comparator used in a Condition
prettyComparator :: Comparator -> String
prettyComparator Eq = "=="
prettyComparator Lt = "<"
prettyComparator Gt = ">"

-- UTILISATION EXAMPLE
printFibo = putStrLn $ pretty fibonacci

-- TESTS
testPrettyFibonacci :: Bool
testPrettyFibonacci = pretty fibonacci == expected
  where expected = "fibonacci n := if (n == 0) then {\n\t0\n} else {\n\tif (n == 1) then {\n\t1\n} else {\n\t(fibonacci((n - 1)) + fibonacci((n - 2)))\n}\n}"




-- FP 3.4 
-- by Théo Mougnibas


-- MAIN FUNCTION
eval :: Prog->String->[Integer]->Integer
eval p@(Program ((Procedure fname params fbody):fs)) function_to_call args
    | fname == function_to_call = evalExpr fbody associated_args p
    | otherwise = eval (Program fs) function_to_call args
    -- associate every parameter's value with its identifier String
    where associated_args = zip params args 

-- HELPER FUNCTIONS


-- find an argument value from its variable identifier
findArgValue::[(Param,Integer)]->String->Integer
findArgValue ((IntParam (Var argname),value):args) varname 
    | varname == argname = value
    | otherwise = findArgValue args varname


-- Biggest helper function, will evaluate an Expr
-- The parameters are used here
evalExpr :: Expr->[(Param,Integer)]->Prog->Integer
evalExpr (IntConst x) _ _ = x
evalExpr (Var x) args _ = findArgValue args x -- assume local variable declaration is not possible as this is FP
-- basic operations
evalExpr (Mult e1 e2)args p= evalExpr e1 args p * evalExpr e2 args p
evalExpr (Add e1 e2) args p = evalExpr e1 args p + evalExpr e2 args p
evalExpr (Sub e1 e2) args p = evalExpr e1 args p - evalExpr e2 args p

-- If/Else
evalExpr (If cond e1 e2) args p
  | evalCond cond args p = evalExpr e1 args p
  | otherwise = evalExpr e2 args p

-- function call
evalExpr (Call fname params) args p = eval p fname $ evalParams params args p



-- evaluate parameters to call a function from another function context
-- will return the actuak values of the parameters
evalParams::[Param]->[(Param,Integer)]->Prog->[Integer]
-- only IntParam as we don't do higher order functions (so IdParam is not matched)
evalParams [IntParam e] args p = [evalExpr e args p]
evalParams ((IntParam e):xs) args p = evalExpr e args p : evalParams xs args p


-- Do the comparison inside a Condition
-- and then contextually evaluate the "then" or the "else" Expr of said Condition 
evalCond :: Condition->[(Param,Integer)]->Prog->Bool
evalCond (Cond Eq e1 e2) args p = evalExpr e1 args p == evalExpr e2 args p
evalCond (Cond Lt e1 e2) args p = evalExpr e1 args p < evalExpr e2 args p
evalCond (Cond Gt e1 e2) args p = evalExpr e1 args p > evalExpr e2 args p


-- UTILISATION EXAMPLE
fibofive = eval fibonacci "fibonacci" [5]


-- TESTS
testEvalSum :: Bool
testEvalSum = eval sumProg "sum" [5] == 15

testEvalFibonacci :: Bool
testEvalFibonacci = eval fibonacci "fibonacci" [5] == 5


-- factor :: Parser Expr
-- factor =
--       whitespace intConst
--   <|> whitespace ifExpr
--   <|> whitespace functionCall
--   <|> whitespace <*> parens expr
--   <|> whitespace variable

-- intConst :: Parser Expr
-- intConst = IntConst <$> integer

-- variable :: Parser Expr
-- variable = Var <$> many letter


-- expr :: Parser Expr
-- expr = 

-- -- term :: Parser Expr
-- -- term = factor <|> factor <* char '*' *> term

-- ifExpr :: Parser Expr
-- ifExpr =
--   string "if" *>
--   ( If <$> parens condition <*
  
--   string "then" *>
  
--   (Then <$> braces expr) <*
  
--   string "else" *>
  
--   (Else <$> braces expr ) 
  
--   )

-- functionCall :: Parser Expr
-- functionCall =
--   many letter >>= \name ->
--   option [] (parens (sep param (char ','))) >>= \args ->
--   pure (Call name args)






-- condition :: Parser Condition
-- condition =
--   Cond <$> 
--   expr <*> 
--   comparator <*>
--   expr

--   -- pure (Cond cmp e1 e2)

-- comparator :: Parser Comparator
-- comparator =
--       pure Eq <$ string "=="
--   <|> pure Lt <$ char '<'
--   <|> pure Gt <$ char '>'




-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll

