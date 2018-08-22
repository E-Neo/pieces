-- Exercise 1
fun1' :: [Integer] -> Integer
fun1' [] = 1
fun1' (x:xs)
  | even x = (x - 2) * fun1' xs
  | otherwise = fun1' xs

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n | even n = n + fun2' (n `div` 2)
        | otherwise = fun2' (3 * n + 1)

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (> 1) . iterate f
  where f n = if even n then n `div` 2 else 3 * n + 1

-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving Show

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree (x:xs) = insert x (foldTree xs)
  where height :: Tree a -> Integer
        height Leaf = -1
        height (Node h _ _ _) = h

        insert :: a -> Tree a -> Tree a
        insert x' Leaf = Node 0 Leaf x' Leaf
        insert x' (Node h t1 y t2)
          | height t1 > height t2 = Node h t1 y (insert x' t2)
          | height t1 < height t2 = Node h (insert x' t1) y t2
          | height t1 == height (insert x' t1) = Node h (insert x' t1) y t2
          | height t2 == height (insert x' t2) = Node h t1 y (insert x' t2)
          | otherwise = Node (h+1) (insert x' t1) y t2

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldl1 (\a b -> a && not b || not a && b)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:).f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1).(*2)) $ foldr ($) [1..n]
  [filter (/= x) | j <- [1..n], i <- [1..j], let x = i + j + 2*i*j, x <= n]
