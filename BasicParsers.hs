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
example1 = runParser (between (char 'a') (char 'b') (char 'a')) (Stream "aba")
example2 = runParser (whitespace (char 'a')) (Stream " a b ")

-- TESTS

test1 = runParser (between (char 'a') (char 'b') (char 'a')) (Stream "aba") == [('b', Stream "")]
test2 = runParser (whitespace (char 'a')) (Stream " a b ") == [('a', Stream "b ")]

