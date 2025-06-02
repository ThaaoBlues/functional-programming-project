module Set6 where

import Data.Foldable
import Data.Monoid
import Text.Read
import Set5

addstr :: String->String->Maybe String
addstr [] [] = Just "0"
addstr x y = addstr' (readMaybe x)  ( readMaybe y)

addstr' :: Maybe Int->Maybe Int->Maybe String
addstr' (Nothing) (Just y) = Just $ show y
addstr' (Just x) (Nothing) = Just $ show x 
addstr' (Just x) (Just y) = Just $ show (x+y) 
addstr' (Nothing) (Nothing) = Nothing

------------------------------------------------------------------------------
-- Exercise 3-FP.8
------------------------------------------------------------------------------


data MyList a = Nil | Cons a (MyList a)
              deriving (Show, Eq)

instance Functor MyList where
    fmap f (Cons x s) = (Cons (f x) (fmap f s))
    fmap _ (Nil) = (Nil)

instance Applicative MyList where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _<*> Nil = Nil
    (Cons f fs) <*> (Cons x s) = Cons (f x) (fs <*> s)



mylst  = Cons 1   $ Cons 2   $ Cons 3   $ Nil
mylst2 = Cons 10  $ Cons 20  $ Cons 30  $ Nil
mylst3 = Cons 100 $ Cons 200 $ Cons 300 $ Nil

myzipWith3' :: (a -> b -> c -> d) -> MyList a -> MyList b -> MyList c -> MyList d
myzipWith3' _ Nil _ _ = Nil
myzipWith3' _ _ Nil _ = Nil
myzipWith3' _ _ _ Nil  = Nil
myzipWith3' f (Cons x xs) (Cons y ys) (Cons z zs) = Cons (f x y z) (myzipWith3' f xs ys zs)

myzipWith :: (a->b->c) -> MyList a -> MyList b -> MyList c
myzipWith _ Nil _ = Nil 
myzipWith _ _ Nil = Nil
myzipWith f c1@(Cons x xs) c2@(Cons y ys) = f <$> c1 <*> c2


myzipWith3 :: (a->b->c->d) -> MyList a -> MyList b -> MyList c -> MyList d
myzipWith3 _ Nil _ _ = Nil 
myzipWith3 _ _ Nil _ = Nil
myzipWith3 _ _ _ Nil = Nil
myzipWith3 f (Cons x xs) (Cons y ys) (Cons z zs) = f <$> (Cons x xs) <*> (Cons y ys) <*> (Cons z zs)


myIO :: IO Int
myIO = (+) <$> (read <$> getLine) <*> (read <$> getLine)


justs :: [Maybe a] -> Maybe [a]
justs [] = Just []
justs (Nothing:xs) = Nothing
justs (x:xs) = (:) <$> x <*> justs xs 

-- r is the type of whatever is parsed, such as the EDSL Expr
-- runParser is the function that will return the parsing function passed as argument to the constructor P to make a Parser 
data Parser r = P {
    runParser :: String -> [(r , String)]
}



char :: Char -> Parser Char
char c = P p
    where
        p [] = []
        p (x:xs) | c == x = [(x,xs)]
                 | otherwise = []


parseOne = char '1'

instance Functor Parser where
    fmap f (P p) = P (\input -> [(f a, rest) | (a, rest) <- p input])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P $ \input -> [(x, input)]

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (P p1) <*> (P p2) = P $ \input -> do
    (f, rest1) <- p1 input
    (x, rest2) <- p2 rest1
    return (f x, rest2)

    where (f, rest1) = p1 

parseAB :: Parser (Char, Char)
parseAB = (,) <$> char 'a' <*> char 'b'


-- Test function that should succeed
testSuccessfulParseAB :: [( (Char, Char), String)]
testSuccessfulParseAB = runParser parseAB "abc"

-- Test function that should fail
testFailedParseAB :: [( (Char, Char), String)]
testFailedParseAB = runParser parseAB "bac"

parseString :: String -> Parser String
parseString [] = pure ""
parseString (x:xs) = (:) <$> char x <*> parseString xs

------------------------------------------------------------------------------
-- Exercise 3-FP.12
------------------------------------------------------------------------------

--fact = parserFun "function factorial x = if x == 0 then 1 else factorial(dec x) * x"


fibonacci :: IO (Integer -> Integer)
fibonacci = (evalfun . parserFun) <$> (readFile "fib.txt")

fib5 :: IO Integer
fib5 = fibonacci <*>  5
fibs :: IO [Integer]
fibs = (map <$> fibonacci ) <*> pure [0..]



-- 3-FP-13
--data A = R [Int] | Q Int Char
  --deriving Show

--generateR :: IO A
--generateR = generate (R <$> arbitrary)

--generateQ :: IO A
--generateQ = generate (Q <$> arbitrary <*> arbitrary)

-- instance Arbitrary A where
  -- arbitrary = oneof [R <$> arbitrary, Q <$> arbitrary <*> arbitrary]
