-- Add your names and student numbers to the following lines. Do not change anything else on these lines, since it they are parsed.
-- Student 1: Théo Mougnibas (s3581497)
-- Student 2: Iris Borgonjen (s3192393)
-- Student 3: Mirte Verheijen (s3207803)

{-# LANGUAGE TemplateHaskell #-}

module MicroFP where

import Control.Applicative
import PComb
import BasicParsers



check :: (Eq a, Show a) => String -> a -> a -> IO ()
check label expected actual =
  putStrLn (
    if expected == actual
      then "[PASS] " ++ label
      else "[FAIL] " ++ label 
      ++ "\n  Expected: " ++ show expected 
      ++ "\n  Actual:   " ++ show actual
  )



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
            deriving (Show, Eq)

data Procedure = Procedure String [Param] Expr
            deriving (Show, Eq)

-- EDSL
data Param = Param Expr
  deriving (Show, Eq)

data Expr = IntConst Integer
          | Var String
          | Mult Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | If Condition Expr Expr
          | Call String [Param]
          deriving (Show, Eq)

data Condition = Cond Comparator Expr Expr
                deriving (Show, Eq)

data Comparator = Eq | Lt | Gt deriving (Show, Eq)

-- FP3.2
-- by Théo Mougnibas

fibonacci = Program [
  (Procedure "fibonacci" [Param (IntConst 0)] (IntConst 0)),
  (Procedure "fibonacci" [Param (IntConst 1)] (IntConst 1)),
  (Procedure "fibonacci" [Param (Var "n")]    
    (Add (Call "fibonacci" [Param (Sub (Var "n") (IntConst 1))])
    (Call "fibonacci" [Param (Sub (Var "n") (IntConst 2))])
    
    )
    
  )



    
  ]


fib = Program [Procedure "fib" [Param (Var "n")] (If (Cond Eq (Var "n") (IntConst 0)) (IntConst 0)
 (If (Cond Eq (Var "n") (IntConst 1)) (IntConst 1) ( Add 
    (Call "fib" [Param (Sub (Var "n") (IntConst 1))])
    (Call "fib" [Param (Sub (Var "n") (IntConst 2))] )
    )

  ) 
  )]

sumProg = Program [Procedure "sum" [Param (Var "a")] (If (Cond Eq (Var "a") (IntConst 0)) (IntConst 0) (Add (Var "a") (Call "sum" [Param (Sub (Var "a") (IntConst 1))] )))]

divProg = Program [Procedure "div" [Param (Var "x"), Param (Var "y")] (
    If (Cond Lt (Var "x") (Var "y"))
       (IntConst 0)
       (Add (IntConst 1) (Call "div" [Param (Sub (Var "x") (Var "y")), Param (Var "y")]))
  )]

twice =   Program [Procedure "twice" [Param (Var "f"), Param (Var "x")] (
    Call "f" [Param (Call "f" [Param (Var "x")])]
  )
  ]



struct = Program [
  Procedure "add" [Param (Var "x"), Param (Var "y")] (Add (Var "x") (Var "y")),
  Procedure "inc" [Param (Var "x")] (Call "add" [Param (IntConst 1)]),
  Procedure "eleven" [] (Call "inc" [Param (IntConst 10)])
  ]

-- UTILISATION EXAMPLE

-- tested and used by testing other features that actually do something

-- TESTS 



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
prettyParam  (Param expr) = prettyExpr expr

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
prettyExpr (Call func (p1:ps)) = func ++ "(" ++ foldr (\x s -> s++"," ++prettyParam x ) (prettyParam p1) ps ++ ")"

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
printFib = putStrLn $ pretty fib

-- TESTS
testPrettyFibonacci = check "pretty fib" (pretty fib) "fib n := if (n == 0) then {\n\t0\n} else {\n\tif (n == 1) then {\n\t1\n} else {\n\t(fib((n - 1)) + fib((n - 2)))\n}\n}"




-- FP 3.4
-- FP 5.2
-- by Théo Mougnibas


-- MAIN FUNCTION
eval :: Prog->String->[Integer]->Integer
eval (Program []) _ _ = error "Procedure not found or arguments didn't match"
eval p@(Program f_list) function_to_call args

    -- associate every parameter's value with its identifier String
    | null matching_candidates = error "No procedure to pattern match the arguments or procedure not found by name."
    | otherwise = 
        -- get first candidate procedure by pattern matching
        let (Procedure fname params fbody) = head matching_candidates
        -- associate procedure parameters to actual values
        in let associated_arguments = zip params args
        -- evaluate procedure body
        in evalExpr fbody associated_arguments p
    where 
        matching_candidates = filter 
          (\(Procedure fname params fbody) -> fname == function_to_call && matchArgs args params) 
          f_list


-- HELPER FUNCTIONS

-- return true if we can match all parameters to the pattern of a procedure definiton
matchArgs :: [Integer] -> [Param] -> Bool
matchArgs [] [] = True
matchArgs (i:is) (Param (IntConst p) : ps) = i == p && matchArgs is ps
matchArgs (i:is) (Param (Var _) : ps)      = matchArgs is ps
matchArgs _ _ = False  -- fallback: mismatched lengths or unknown pattern




-- find an argument value from its variable identifier
findArgValue::[(Param,Integer)]->String->Integer
findArgValue ((Param (Var argname),value):args) varname 
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
-- only Param as we don't do higher order functions (so Param is not matched)
evalParams [Param e] args p = [evalExpr e args p]
evalParams ((Param e):xs) args p = evalExpr e args p : evalParams xs args p


-- Do the comparison inside a Condition
-- and then contextually evaluate the "then" or the "else" Expr of said Condition 
evalCond :: Condition->[(Param,Integer)]->Prog->Bool
evalCond (Cond Eq e1 e2) args p = evalExpr e1 args p == evalExpr e2 args p
evalCond (Cond Lt e1 e2) args p = evalExpr e1 args p < evalExpr e2 args p
evalCond (Cond Gt e1 e2) args p = evalExpr e1 args p > evalExpr e2 args p


-- UTILISATION EXAMPLE
fibfive = eval fib "fib" [5]


-- TESTS
testEvalSum = check "sum" (show (eval sumProg "sum" [5]) ) "15"

testEvalFib = check "fib" (show (eval fib "fib" [5])) "5"

testEvalFibonacci = check "fibonacci" (show (eval fibonacci "fibonacci" [5])) "5"


-- FP4.1
-- Théo Mougnibas


-- takes an initial value and a list of functions,
-- and applies them from left to right to the accumulated result
-- used for left-associative application of operators
fold :: a -> [a -> a] -> a
fold = foldl (\x f -> f x)

-- parses one or more occurrences of 'p' separated by operator 'op'
-- left-associative
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = parse
  where
    -- parses a single value with parser p
    -- then parses z/more of operator followed by another value
    -- and folds them left-associatively.
    parse = fold <$> p <*> many rest

    -- parses an operator and a value, then returns a function
    -- that takes the current accumulated value 
    -- and applies the operator to it and the incoming value
    -- ( built objects would be Add, Sub or Mul, f in the lambda function )
    rest = (\f y -> \x -> f x y) <$> op <*> p

-- use left associative chain to avoid recursion
-- recursion was extremely slow
expr :: Parser Expr
expr = term `chainl1` addOp

term :: Parser Expr
term = factor `chainl1` mulOp

-- parses left side of Add/sub operations
addOp :: Parser (Expr -> Expr -> Expr)
addOp =  symbol "+" *> pure Add
     <|> symbol "-" *> pure Sub

-- parses left side of Mult operations
mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = symbol "*" *> pure Mult


factor :: Parser Expr
factor =
  functionCall
  <|> ifExpr
  <|> parens expr
  <|> intConst
  <|> variable -- don't move variable from here as it could eat identifier of function calls

-- parses an integer and put it as an expression
intConst :: Parser Expr
intConst = IntConst <$> integer

-- parses a variable name
variable :: Parser Expr
variable = Var <$> identifier



-- parses an if statement
ifExpr :: Parser Expr
ifExpr =
  symbol "if" *>
  ( If <$> parens condition <*>
  
  (symbol "then" *>
  
  braces expr )<*> -- Then
  
  (symbol "else" *>
  
  braces expr) -- Else
  
  )

-- parses a function call parameter
callParam :: Parser Param
callParam = Param <$> whitespace expr

-- parses a function formal parameters
formalParam :: Parser Param
formalParam = Param <$> ((Var <$> identifier) <|> (IntConst <$> integer) )

-- parses a function declaration and its body
functionParser :: Parser Procedure 
functionParser = Procedure <$>
  identifier <*> 
  many formalParam <*>
  ( symbol ":=" *> 
  expr <*
  symbol ";")


-- parses a function call and its parameters
functionCall :: Parser Expr
functionCall = whitespace(
  Call <$> identifier <*> parens (sep1 callParam (symbol ",")))


-- parses the condition of an if statement
condition :: Parser Condition
-- funky function to re-order arguments to the Cond data constructor
condition = (\e1 cmp e2-> Cond cmp e1 e2) <$> expr <*>comparator <*> expr  

-- parses the comparator of an if statement condition
comparator :: Parser Comparator
comparator =
      Eq <$ symbol "=="
  <|> Lt <$ symbol "<"
  <|> Gt <$ symbol ">"


-- parses all function declared in a program
program :: Parser Prog
program = Program <$> whitespace (some functionParser)

-- USAGE EXAMPLES
example3 = runParser program (Stream "f x := x;")

-- TESTS

testIntConst = runParser intConst (Stream "42")

testVariable = check "variable"
  [(Var "x", Stream " y")]
  (runParser variable (Stream "x y"))

testAddExpr = check "addition"
  [(Add (IntConst 1) (IntConst 2), Stream "")]
  (runParser expr (Stream "1+2"))

testSubExpr = check "subtraction"
  [(Sub (IntConst 3) (IntConst 1), Stream "")]
  (runParser expr (Stream "3-1"))

testMultExpr = check "multiplication"
  [(Mult (IntConst 2) (IntConst 4), Stream "")]
  (runParser term (Stream "2*4"))

testIfExpr = check "ifExpr"
  [(If (Cond Gt (Var "x") (IntConst 0)) (IntConst 1) (IntConst 2), Stream "")]
  (runParser ifExpr (Stream "if (x > 0) then {1} else {2} "))

testFunctionCall = check "functionCall"
  [(Call "f" [Param (Var "x")], Stream "")]
  (runParser functionCall (Stream "f (x)"))

testComparator1 = check "comparator ==" Eq (fst . head $ runParser comparator (Stream "=="))
testComparator2 =  check "comparator <" Lt (fst . head $ runParser comparator (Stream "<"))
testComparator3 =  check "comparator >" Gt (fst . head $ runParser comparator (Stream ">"))

testCondition = check "condition"
  [(Cond Lt (Var "x") (IntConst 2), Stream "")]
  (runParser condition (Stream "x < 2"))

testFunctionParser = check "functionParser"
  [(Procedure "f" [Param (Var "x")] (Add (Var "x") (IntConst 1)), Stream "")]
  (runParser functionParser (Stream "f x := x + 1;"))

testProgramParser =
  let input = "f x := x + 1; g y := y * 2;"
      result = runParser program (Stream input)
      Program fs = fst (head result)
      names = map (\(Procedure name _ _) -> name) fs
  in check "programParser (function names)" ["f", "g"] names


-- FP5.3
-- Théo Mougnibas

-- rewrites function definitions with pattern
-- matching to a single definition with if-expressions.
patmatch :: Prog->Prog
patmatch (Program p@((Procedure fname params fbody):ps)) = Program [(Procedure fname new_params new_body)]
  where new_params = Param (getVarOfFirstArgumentOfLastProcedure p) : tail params
        new_body = patmatch' p


patmatch' :: [Procedure] -> Expr
patmatch' [(Procedure name params fbody)] = fbody
patmatch' p@((Procedure name params fbody):ps) = If (Cond (Eq) (var) (const)) (fbody) (sbody)
  where var = getVarOfFirstArgumentOfLastProcedure p
        (Param const) = head params
        sbody = patmatch' ps

getVarOfFirstArgumentOfLastProcedure :: [Procedure]->Expr
getVarOfFirstArgumentOfLastProcedure ps = var
  where (Param var) = head params
        (Procedure _ params _ ) = last ps

-- USAGE EXAMPLE
patmatchUsageExample :: String
patmatchUsageExample = (pretty.patmatch) fibonacci


-- TESTS
patmatchTest1 = check "patmatch fibonacci" ((pretty.patmatch) fibonacci) "fibonacci n  := if (n == 0) then {\n\t0\n} else {\n\tif (n == 1) then {\n\t1\n} else {\n\t(fibonacci((n - 1)) + fibonacci((n - 2)))\n}\n\n}\n;\n"
patmatchTest2 = check "patmatch sum" ((pretty.patmatch) sumProg) "sum a  := if (a == 0) then {\n\t0\n} else {\n\t(a + sum((a - 1)))\n}\n;\n"


-- FP4.2
-- Iris Borgonjen
compile :: String -> Prog
compile s = case runParser program (Stream s) of
    [] -> error "Impossible to parse program. (Missing semicolon ?)"
    result -> fst. head $ result
testcompile = compile "fib n := if (n < 3) then {1} else {fib (n-1) + fib (n-2)};"

runFile :: FilePath -> [Integer] -> IO Integer
runFile file xs = eval <$> p <*> (f <$> p) <*> pure xs
  where p = compile <$> readFile file
        f (Program ps) = name (last ps)
        name (Procedure n _ _) = n
