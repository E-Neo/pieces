{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = let f a b = a : f b (a + b) in f 0 1

-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x' xs) = x' : streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x' = Cons x' (streamRepeat x')

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x' xs) = Cons (f x') (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Cons s (streamFromSeed f (f s))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x' xs) ys = Cons x' (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)

-- Exercise 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate (Cons n ns) = Cons (-n) (negate ns)
  (+) (Cons a as) (Cons b bs) = Cons (a + b) (as + bs)
  (*) (Cons a as) (Cons b bs) = Cons (a * b)
                                (fromInteger a * bs + as * Cons b bs)

instance Fractional (Stream Integer) where
  (/) (Cons a as) (Cons b bs) =
    let q = Cons a as / Cons b bs
    in Cons (a `div` b) (fromInteger (1 `div` b) * (as - q * bs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^(2 :: Integer))

-- Exercise 7
data Matrix = Matrix Integer Integer Integer Integer
  deriving Show

instance Num Matrix where
  (*) (Matrix a b c d) (Matrix e f g h) =
    Matrix (a*e + b*g) (a*f + b*h) (c*e + d*g) (c*f + d*h)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = let Matrix _ b _ _ = Matrix 1 1 1 0 ^ n in b
