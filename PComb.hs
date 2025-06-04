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

data Parser a = P {
    runParser :: Stream -> [(a, Stream)]
}

instance Functor Parser where
    fmap f (P p) = P (\inStream -> [(f x, rest) | (x, rest) <- p inStream])

char :: Char -> Parser Char
char c = P p
     where
       p (Stream [])                 = []
       p (Stream (x:xs)) | c == x    = [(x, (Stream xs))]
                         | otherwise = []

failure :: Parser a
failure = P (\_ -> [])

instance Applicative Parser where
    pure x = P (\inStream -> [(x, inStream)])
    (P pf) <*> (P px) = P (\inStream -> [(f a, restx) | (f, restf) <- pf inStream, (a, restx) <- px restf])

instance Alternative Parser where
    empty = failure
    (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = P p
        where  p inStream | null $ runParser p1 inStream = runParser p2 inStream
                          | otherwise = runParser p1 inStream


