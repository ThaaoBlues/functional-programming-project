-- Add your names and student numbers to the following lines. Do not change anything else on these lines, since they are parsed.
-- Student 1: Théo Mougnibas (s3581497)
-- Student 2: Iris Borgonjen (s3192393)
-- Student 3: Mirte Verheijen (s3207803)


module PComb where
import Control.Applicative
import Data.Char
import Test.QuickCheck

-- Stream of Chars - can be extended with the location for error handling
data Stream = Stream [Char]
              deriving (Eq, Show)

-- FP1.1
-- Iris Borgonjen
-- Parser of type a, receiving a Stream
data Parser a = P {
    runParser :: Stream -> [(a, Stream)]
}

-- FP1.2
-- Iris Borgonjen
-- Functor instance of the parser
instance Functor Parser where
    fmap f (P p) = P (\inStream -> [(f x, rest) | (x, rest) <- p inStream])

-- Test
fTest = runParser (ord <$> (char 'b')) (Stream "bcc")

-- FP1.3
-- Iris Borgonjen
-- Parser that parses a single character
char :: Char -> Parser Char
char c = P p
     where
       p (Stream [])                 = []
       p (Stream (x:xs)) | c == x    = [(x, (Stream xs))]
                         | otherwise = []

-- Test
charTest = runParser (char 'a') (Stream "abc")

-- FP1.4
-- Iris Borgonjen
-- Always fails (results in empty list) since it does not consume input
failure :: Parser a
failure = P (\_ -> [])

-- Test
failTest = runParser failure (Stream "abc")

-- FP1.5
-- Iris Borgonjen
-- Instance for sequence combinator
instance Applicative Parser where
    pure x = P (\inStream -> [(x, inStream)])
    pf <*> px = P --(\inStream -> [(f a, restx) | (f, restf) <- runParser pf inStream, (a, restx) <- runParser px restf])
        (\inStream -> case runParser pf inStream of
            [] -> []
            [(f,restf)] -> case runParser px restf of
                        [] -> []
                        [(x,restx)] -> [(f x,restx)]
                 )

-- Test
pureTest = runParser (pure 1) (Stream "abc")
appTest = runParser ((,) <$> char 'a' <*> char 'b') (Stream "abc")

-- FP1.6
-- Iris Borgonjen
-- Instance that tries as few alternatives as possible
instance Alternative Parser where
    empty = failure
    -- A TA advised us to not use list comprehension 
    -- as it would not be using lazy evaluation properly
    -- and he was right, our first implementation took 162 seconds to compile at BEST
    p1 <|> p2 = P 
        (\inStream -> case runParser p1 inStream of
            [] -> runParser p2 inStream
            result -> result)

-- Test
altTest = runParser ((char 'a') <|> (char 'b')) (Stream "bac")
