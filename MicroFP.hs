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

data Procedure = Procedure Expr [Param] Expr
            deriving Show

newtype Identifier = Identifier Expr
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
          | Then Expr
          | Else Expr
          | Call String [Param]
          deriving Show

data Condition = Cond Comparator Expr Expr
                deriving Show

data Comparator = Eq | Lt | Gt deriving Show

-- FP3.2


-- TO CHECK, ONE IMPLEMENTATION OF FIBONACCI GIVES F(0) = 1 AND THE OTHER F(0) = 0 
fibonacci = Program [Procedure (Var "fibonacci") [IntParam (Var "n")] (If (Cond Eq (Var "n") (IntConst 0)) (Then (IntConst 0))
 (Else (If (Cond Eq (Var "n") (IntConst 1)) (Then (IntConst 1)) (Else ( Add 
    (Call "fibonacci" [IntParam (Sub (Var "n") (IntConst 1))])
    (Call "fibonacci" [IntParam (Sub (Var "n") (IntConst 2))] )
    
    )        
    ) ) ) )]


fib = Program [Procedure (Var "fibonacci") [IntParam (Var "n")] (If (Cond Eq (Var "n") (IntConst 0)) (Then (IntConst 0))
 (Else (If (Cond Eq (Var "n") (IntConst 1)) (Then (IntConst 1)) (Else ( Add 
    (Call "fibonacci" [IntParam (Sub (Var "n") (IntConst 1))])
    (Call "fibonacci" [IntParam (Sub (Var "n") (IntConst 2))] )
    
    )        
    ) ) ) )]

sum = Program [Procedure (Var "sum") [IntParam (Var "a")] (If (Cond Eq (Var "a") (IntConst 0)) (Then (IntConst 0)) (Else (Add (Var "a") (Call "sum" [IntParam (Sub (Var "a") (IntConst 1))] ))))]

div = Program [Procedure (Var "div") [IntParam (Var "x"), IntParam (Var "y")] (
    If (Cond Lt (Var "x") (Var "y"))
       (IntConst 0)
       (Add (IntConst 1) (Call "div" [IntParam (Sub (Var "x") (Var "y")), IntParam (Var "y")]))
  )]

twice =   Program [Procedure (Var "twice") [IdParam (Var "f"), IntParam (Var "x")] (
    Call "f" [IntParam (Call "f" [IntParam (Var "x")])]
  )
  ]



struct = Program [
  Procedure (Var "add") [IntParam (Var "x"), IntParam (Var "y")] (Add (Var "x") (Var "y")),
  Procedure (Var "inc") [IntParam (Var "x")] (Call "add" [IntParam (IntConst 1)]),
  Procedure (Var "eleven") [] (Call "inc" [IntParam (IntConst 10)])
  ]


-- FP3.3

-- pretty-printing

pretty :: Prog -> String
pretty (Program procedures) = foldr (\x s-> x ++ "\n"++ s) "" $ map prettyProcedure procedures
-- the foldr is to build a global string with all precedures separated by newlines

prettyProcedure :: Procedure -> String
prettyProcedure (Procedure name params expr) =
  prettyExpr name ++ " " ++ foldr (\x s-> x ++ " "++ s) "" (map prettyParam params) ++ " := " ++ prettyExpr expr ++ ";"
-- the foldr is to build a global string with all parameters separated by spaces

prettyParam :: Param -> String
prettyParam (IdParam expr) = prettyExpr expr
prettyParam (IntParam expr) = prettyExpr expr

prettyExpr :: Expr -> String
prettyExpr (IntConst i) = show i
prettyExpr (Var s) = s
prettyExpr (Mult e1 e2) = "(" ++ prettyExpr e1 ++ " * " ++ prettyExpr e2 ++ ")"
prettyExpr (Add e1 e2) = "(" ++ prettyExpr e1 ++ " + " ++ prettyExpr e2 ++ ")"
prettyExpr (Sub e1 e2) = "(" ++ prettyExpr e1 ++ " - " ++ prettyExpr e2 ++ ")"
prettyExpr (If cond e1 e2) = "if (" ++ prettyCondition cond ++ ")"
    ++ prettyExpr e1 -- then
    ++ prettyExpr e2 -- else

-- there, we need to fold as we might have multiple parameters
prettyExpr (Call func params) = func ++ "(" ++ foldr (\x s -> prettyParam x ++ s) "" params ++ ")"
prettyExpr (Then e) = " then {\n\t" ++ prettyExpr e ++ "\n}"
prettyExpr (Else e) = " else {\n\t" ++ prettyExpr e ++ "\n}\n"

prettyCondition :: Condition -> String
prettyCondition (Cond cmp e1 e2) =
  prettyExpr e1 ++ " " ++ prettyComparator cmp ++ " " ++ prettyExpr e2

prettyComparator :: Comparator -> String
prettyComparator Eq = "=="
prettyComparator Lt = "<"
prettyComparator Gt = ">"


testProg = Program [
  Procedure (Var "foo") [IntParam (Var "x"), IntParam (IntConst 5)] (Add (Var "x") (IntConst 1)),
  Procedure (Var "bar") [] (If (Cond Lt (Then (Var "a")) (Else (IntConst 10))) (IntConst 1) (IntConst 0))
  ]


-- FP 3.4
eval :: Prog->String->[Integer]->Integer
eval p@(Program ((Procedure (Var fname) params fbody):fs)) function_to_call args
    | fname == function_to_call = evalExpr fbody associated_args p
    | otherwise = eval (Program fs) function_to_call args
    where associated_args = zip params args


findArgValue::[(Param,Integer)]->String->Integer
findArgValue ((IntParam (Var argname),value):args) varname 
    | varname == argname = value
    | otherwise = findArgValue args varname


evalExpr :: Expr->[(Param,Integer)]->Prog->Integer
evalExpr (IntConst x) _ _ = x
evalExpr (Var x) args _ = findArgValue args x -- assume local variable declaration is not possible as this is FP
evalExpr (Mult e1 e2)args p= evalExpr e1 args p * evalExpr e2 args p
evalExpr (Add e1 e2) args p = evalExpr e1 args p + evalExpr e2 args p
evalExpr (Sub e1 e2) args p = evalExpr e1 args p - evalExpr e2 args p

evalExpr (If cond e1 e2) args p
  | evalCond cond args p = evalExpr e1 args p
  | otherwise = evalExpr e2 args p

evalExpr (Then e) args p = evalExpr e args p
evalExpr (Else e) args p = evalExpr e args p
evalExpr (Call fname params) args p = eval p fname $ evalParams params args p

-- evaluate parameters to call a function from another function context
evalParams::[Param]->[(Param,Integer)]->Prog->[Integer]
-- only IntParam as we don't do higher order functions
evalParams [IntParam e] args p = [evalExpr e args p]
evalParams ((IntParam e):xs) args p = evalExpr e args p : evalParams xs args p


evalCond :: Condition->[(Param,Integer)]->Prog->Bool
evalCond (Cond Eq e1 e2) args p = evalExpr e1 args p == evalExpr e2 args p
evalCond (Cond Lt e1 e2) args p = evalExpr e1 args p < evalExpr e2 args p
evalCond (Cond Gt e1 e2) args p = evalExpr e1 args p > evalExpr e2 args p


-- eval fibonacci "fibonacci" [5]

-- putStrLn $ pretty testProg

-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll
