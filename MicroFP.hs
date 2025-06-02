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

data Prog = Program [Procedure]
            deriving Show

data Procedure = Procedure Expr [Param] Expr
            deriving Show

data Identifier = Identifier Expr
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

fibonacci = Program [Procedure (Var "fibonacci") [IdParam (Var "n")] (If (Cond Eq (Var "n") (IntConst 0)) (Then (IntConst 1))
 (Else (If (Cond Eq (Var "n") (IntConst 1)) (Then (IntConst 1)) (Else ( Add 
    (Call "fibonacci" [IntParam (Sub (Var "n") (IntConst 1))])
    (Call "fibonacci" [IntParam (Sub (Var "n") (IntConst 2))] )
    
    )        
    ) ) ) )]


fib = Program [Procedure (Var "fibonacci") [IdParam (Var "n")] (If (Cond Eq (Var "n") (IntConst 0)) (Then (IntConst 1))
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



-- TODO : implement the thing with functions from functions.txt

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
    ++ prettyExpr e2 -- els
prettyExpr (Call func expr) = func ++ "(" ++ prettyExpr expr ++ ")"
prettyExpr (Then e) = " then {\n\t" ++ prettyExpr e ++ "\n}\n"
prettyExpr (Else e) = " else {\n\t" ++ prettyExpr e ++ "\n}\n"

prettyCondition :: Condition -> String
prettyCondition (Cond cmp e1 e2) =
  prettyExpr e1 ++ " " ++ prettyComparator cmp ++ " " ++ prettyExpr e2

prettyComparator :: Comparator -> String
prettyComparator Eq = "=="
prettyComparator Lt = "<"
prettyComparator Gt = ">"


testProg = Program [
  Procedure (Var "foo") [IdParam (Var "x"), IntParam (IntConst 5)] (Add (Var "x") (IntConst 1)),
  Procedure (Var "bar") [] (If (Cond Lt (Then (Var "a")) (Else (IntConst 10))) (IntConst 1) (IntConst 0))
  ]

-- putStrLn $ pretty testProg

-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll
