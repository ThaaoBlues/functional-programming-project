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
          | Dec Expr
          | Call String Expr
          deriving Show

data Condition = Cond Comparator Expr Expr
                deriving Show

data Comparator = Eq | Lt | Gt deriving Show



-- Pretty-printing



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
prettyExpr (If cond e1 e2) =
  "if (" ++ prettyCondition cond ++ ") then {" ++ prettyExpr e1 ++ "} else {" ++ prettyExpr e2 ++ "}"
prettyExpr (Call func expr) = func ++ "(" ++ prettyExpr expr ++ ")"
prettyExpr (Dec expr) = prettyExpr expr

prettyCondition :: Condition -> String
prettyCondition (Cond comp e1 e2) =
  prettyExpr e1 ++ " " ++ prettyComparator comp ++ " " ++ prettyExpr e2

prettyComparator :: Comparator -> String
prettyComparator Eq = "=="
prettyComparator Lt = "<"
prettyComparator Gt = ">"


testProg = Program [
  Procedure (Var "foo") [IdParam (Var "x"), IntParam (IntConst 5)] (Add (Var "x") (IntConst 1)),
  Procedure (Var "bar") [] (If (Cond Lt (Var "a") (IntConst 10)) (IntConst 1) (IntConst 0))
  ]

-- putStrLn $ pretty testProg

-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll
