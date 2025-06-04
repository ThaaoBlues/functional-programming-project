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
            where parseWS = (char ' ') <|> (char '\n') <|> (char '\t')


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

--parses any (alphabetical) letter
letter :: Parser Char
letter = P p 
    where p (Stream []) = []
          p (Stream (x:xs)) | isAlpha x = [(x,Stream xs)]
                            | otherwise = []

-- parses any digit
dig :: Parser Char
dig = P p 
    where p (Stream []) = []
          p (Stream (x:xs)) | isDigit x = [(x,Stream xs)]
                            | otherwise = []


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

-- only allows parsers that consume input
notEmpty :: Parser a -> Parser a
notEmpty (P p) = P (\s -> [ (x, rest) | (x, rest) <- p s, rest /= s ])

--parses one or more occurrences of p, separated by s
sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p s = (:) <$> p <*> many ( s *> p)

-- The parser sep p s works as sep1 p s, but parses zero or more occurrences of p.
sep :: Parser a -> Parser b -> Parser [a]
sep p s = sep1 p s <|> failure


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



-- TODO :: FIX sep1 AND finish option
