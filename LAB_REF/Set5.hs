module Set5 where

import Data.List
import Data.Functor
import Data.Either
import Test.QuickCheck.All
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Test.QuickCheck
import Data.Maybe (Maybe(Nothing))

fromLeft' :: Either l r -> l
fromLeft' (Left x) = x -- Newer GHC versions contain a fromLeft :: l -> Either l r -> l

fromRight' :: Either l r -> r
fromRight' (Right x) = x -- Newer GHC versions contain a fromRight :: r -> Either l r -> r

parser :: Parser a -> String -> a
parser p xs | isLeft res = error $ show $ fromLeft' res
          | otherwise  = fromRight' res
  where res = parse p "" xs


languageDef = emptyDef{
  Token.identLetter = alphaNum,
  Token.identStart = letter,
  Token.reservedOpNames = ["+", "*","=="],
  Token.reservedNames = ["(",")", "if","then","else","dec","function"]
}


-- Create lexer (=tokenizer) for your language
lexer = Token.makeTokenParser languageDef
-- Create functions for all types of tokens
identifier = Token.identifier lexer
integer = Token.integer lexer
parens = Token.parens lexer
symbol = Token.symbol lexer
reserved = Token.reserved lexer

data Expr = Const Integer
          | Var String
          | Mult Expr Expr
          | Add Expr Expr
          | If Condition Expr Expr
          | Then Expr
          | Else Expr
          | Dec Expr
          | Call String Expr
          deriving Show

data Condition = Eq Expr Expr
                 deriving Show


parseFactor :: Parser Expr
parseFactor = try(Const <$> integer) 
              <|> try (Call <$> identifier <*> parens parseExpr)
              <|> try(Var <$> identifier) 
              <|> try(parens parseExpr)

parseTerm :: Parser Expr
parseTerm = try(  parseFactor `chainl1` ((\_->Mult) <$> symbol "*") ) <|> parseFactor

parseExpr :: Parser Expr
parseExpr = try parseDec <|> try( parseTerm  `chainl1` ((\_->Add) <$> symbol "+") ) <|> try parseTerm <|> try parseIf


parseCondition :: Parser Condition
parseCondition = Eq <$> (parseExpr <* symbol "==") <*> parseExpr

parseIf :: Parser Expr
parseIf = If <$> (reserved "if" *> parseCondition ) <*>(reserved "then" *> parseExpr) <*>(reserved "else" *> parseExpr)

parseDec :: Parser Expr
parseDec = Dec <$> (reserved "dec" *> parseExpr)



data FunDef  = Func String String Expr
               deriving Show

parseFunDef :: Parser FunDef 
parseFunDef = Func <$> (reserved "function" *> identifier) <*> (identifier <* symbol "=") <*> parseExpr

parserFun :: String -> FunDef
parserFun = parser parseFunDef


--fib :: FunDef
--fib = parserFun "function fib x = if x == 0 then 1 else ( if x == 1 then 1 else fib ( dec x )+ fib ( dec dec x )) "



evalfun :: FunDef -> Integer -> Integer
evalfun f@(Func id1 id2 e) n = evalExpr e n f

evalExpr :: Expr->Integer->FunDef->Integer
evalExpr (Const x) _ _ = x
evalExpr (Var x) n _ = n
evalExpr (Mult e1 e2) n f= evalExpr e1 n f * evalExpr e2 n f
evalExpr (Add e1 e2) n f= evalExpr e1 n f + evalExpr e2 n f
evalExpr (If (Eq ec1 ec2) e1 e2) n f
  | evalExpr ec1 n f == evalExpr ec2 n f = evalExpr e1 n f
  | otherwise = evalExpr e2 n f
evalExpr (Then e) n f = evalExpr e n f
evalExpr (Else e) n f = evalExpr e n f
evalExpr (Dec e) n f = evalExpr e n f -1
evalExpr (Call fname e) n f = evalfun f (evalExpr e n f)


-- Checks if in the evaluation (xˆ2 + 2*x + 1) == (x+1)ˆ2
fn = ( evalfun . parserFun ) "function f x = x*x + 2*x + 1"
fn' = ( evalfun . parserFun ) "function f x = (x +1) * (x +1)"
prop_fn n = n >= 0 ==> fn n == fn' n


factorial :: Integer -> Integer
factorial = ( evalfun . parserFun ) "function factorial x = if x == 0 then 1 else factorial ( dec x) * x"


prop_factorial n = n >= 0 ==> factorial n == product [1.. n]

fib :: Integer -> Integer
fib = (evalfun . parserFun)
 "function fib x = if x == 0 then 1 else (if x == 1 then 1 else fib(dec x)+fib(dec dec x))"
