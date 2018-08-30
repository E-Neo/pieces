{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Exercise 1

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

-- Exercise 2
indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ x)
  | i == 0 = Just x
  | otherwise = Nothing
indexJ i (Append m l1 l2)
  | i < 0 || i > s0 = Nothing
  | i < s1 = indexJ i l1
  | otherwise = indexJ (i - s1) l2
  where s0 = getSize . size $ m
        s1 = getSize . size . tag $ l1

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl@(Single _ _)
  | n <= 0 = jl
  | otherwise = Empty
dropJ n jl@(Append m l1 l2)
  | n <= 0 = jl
  | n < s1 = dropJ n l1 +++ l2
  | n < s0 = dropJ (n - s1) l2
  | otherwise = Empty
  where s0 = getSize . size $ m
        s1 = getSize . size . tag $ l1

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl@(Single _ _)
  | n <= 0 = Empty
  | otherwise = jl
takeJ n jl@(Append m l1 l2)
  | n <= 0 = Empty
  | n < s1 = takeJ n l1
  | n < s0 = l1 +++ takeJ (n - s1) l2
  | otherwise = jl
  where s0 = getSize . size $ m
        s1 = getSize . size . tag $ l1

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4
instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = foldl1 (+++) . map scoreLine' . lines
    where scoreLine' s = Single (scoreString s, Size $ length s) s
  line = indexJ
  replaceLine i s b = takeJ i b +++ fromString s +++ dropJ (i + 1) b
  numLines = getSize . snd . tag
  value = getScore . fst . tag
