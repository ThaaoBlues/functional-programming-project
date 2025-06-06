-- Add your names and student numbers to the following lines. Do not change anything else on these lines, since they are parsed.
-- Student 1: Théo Mougnibas (s3581497)
-- Student 2: Iris Borgonjen (s3192393)
-- Student 3: Mirte Verheijen (s3207803)

module BasicParsers where

import Control.Applicative
import Data.Char
import Test.QuickCheck
import PComb


-- FP2.2

-- runs the three
-- parsers in sequence, and returns the result of the second parser.
between :: Parser a -> Parser b -> Parser c -> Parser b
between p1 p2 p3 = p1 *> (p2 <* p3)

-- receives a parser p and uses it to parse
-- the input stream, while skipping all surrounding whitespaces (space, tab and
-- newline).
whitespace :: Parser a -> Parser a
whitespace p = parseWS *> (p <* parseWS)
            where parseWS = many (char ' ' <|> char '\n' <|> char '\t')


-- UTILISATION EXAMPLES
example1 :: [(Char, Stream)]
example1 = runParser (between (char 'a') (char 'b') (char 'a')) (Stream "aba")
example2 :: [(Char, Stream)]
example2 = runParser (whitespace (char 'a')) (Stream " a b ")

-- TESTS

test1 :: Bool
test1 = runParser (between (char 'a') (char 'b') (char 'a')) (Stream "aba") == [('b', Stream "")]
test2 :: Bool
test2 = runParser (whitespace (char 'a')) (Stream " a b ") == [('a', Stream "b ")]

-- FP2.1
letter :: Parser Char

-- fold the alphabet with an alternative operator
letter = foldr (\c p -> char c <|> p) failure $ ['a'..'z'] ++ ['A'..'Z']

-- parses any digit
dig :: Parser Char
dig = foldr (\c p -> char c <|> p) failure ['0'..'9']



-- UTILISATION EXAMPLES
letterExample :: [(Char, Stream)]
letterExample = runParser letter (Stream "a1b")
digExample :: [(Char, Stream)]
digExample = runParser dig (Stream "1aB")

-- TESTS
test3 :: Bool
test3 = runParser letter (Stream "a1b") == [('a',Stream "1b")]
test4 :: Bool
test4 = null $ runParser letter (Stream "1b")

test5 :: Bool
test5 = runParser dig (Stream "1ba") == [('1',Stream "ba")]
test6 :: Bool
test6 = null $ runParser dig (Stream "b1a")


-- FP2.3

--parses one or more occurrences of p, separated by s
sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p s = (:) <$> p <*> many ( s *> p)

-- The parser sep p s works as sep1 p s, but parses zero or more occurrences of p.
sep :: Parser a -> Parser b -> Parser [a]
sep p s = sep1 p s <|> pure []


-- tries to apply parser p; upon failure it results in x
option :: a -> Parser a -> Parser a
option x p = p <|> pure x

-- UTILISATION EXAMPLES

exampleLetterList1 :: [( [Char], Stream)]
exampleLetterList1 = runParser (sep1 letter (char ',')) (Stream "a,b,c")

exampleLetterList2 :: [( [Char], Stream)]
exampleLetterList2 = runParser (sep letter (char ',')) (Stream "")

exampleOption = runParser (option 'x' (char 'c')) (Stream "test")

-- TESTS

test7 :: Bool
test7 = runParser (sep1 letter (char ',')) (Stream "a,b,c") == [("a,b,c",Stream [])]

test8 :: Bool
test8 = null $ runParser (sep letter (char ',')) (Stream "")

test9 :: Bool
test9 = runParser (option 'x' (char 'c')) (Stream "test") == [('x',Stream "test")]

test10 :: Bool
test10 = runParser (option 'x' (char 't')) (Stream "test") == [('t',Stream "est")]



-- FP2.4


-- parses an given String, similar to the function char
string:: String->Parser String
-- base case different from char Parser as we must return an empty list
string = foldr (\ x -> (<*>) ((:) <$> char x)) (pure [])

-- parses an identifier surrounded by whites-pace
identifier :: Parser String
identifier = whitespace (many (letter <|> dig))

-- parses an integer surrounded by whitespace.
integer :: Parser Integer
integer = read <$> some dig

-- parses a given string surrounded by white-space
symbol :: String->Parser String
symbol s =whitespace (string s)

-- parses something using the provided parser between parentheses
parens :: Parser a -> Parser a
parens p = char '(' *> (p <* char ')')

-- parses something using the provided parser between braces.
braces :: Parser a -> Parser a
braces p = char '{' *> (p <* char '}')


-- UTILISATION EXAMPLE
exampleString = runParser (string "ceci est") (Stream "ceci est un exemple")
exampleIdentifier = runParser identifier (Stream " function := x==1")

exampleInteger = runParser integer (Stream "123456 aha")

exampleSymbol = runParser (symbol "ceci") (Stream " ceci est un exemple")

exampleParens = runParser (parens $ string "test entre parentheses") (Stream "(test entre parentheses)")

exampleBraces = runParser (braces $ string "test entre accolades") (Stream "{test entre accolades}")

-- TESTS

test11 = runParser (string "ceci est") (Stream "ceci est un exemple") == [("ceci est",Stream " un exemple")]

test12 = runParser identifier (Stream " function := x==1") == [("function",Stream ":= x==1")]

test13 = runParser integer (Stream "123456 aha") == [(123456,Stream " aha")]

test14 = runParser (symbol "ceci") (Stream " ceci est un exemple") == [("ceci",Stream "est un exemple")]

test15 = runParser (parens $ string "test entre parentheses") (Stream "(test entre parentheses)") == [("test entre parentheses", Stream [])]

test16 = runParser (braces $ string "test entre accolades") (Stream "{test entre accolades}") == [("test entre accolades", Stream [])]




