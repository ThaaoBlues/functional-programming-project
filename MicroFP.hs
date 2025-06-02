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

data Expr = Const Integer
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


-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll
