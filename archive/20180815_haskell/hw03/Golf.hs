module Golf where

-- Exercise 1
everyN :: Int -> [a] -> [a]
everyN n l = map snd (filter ((== 0).(`mod` n).fst) (zip [1..] l))

skips :: [a] -> [[a]]
skips l = [everyN n l | n <- [1..length l]]

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima l =
  let il = zip [0..] l :: [(Int, Integer)]
  in map (\ (_, b, _) -> b)
     (filter (\ (a, b, c) -> b > a && b > c)
      (zip3 l [snd x | x <- il, fst x > 0] [snd x | x <- il, fst x > 1]))

-- Exercise 3
insert :: [Int] -> Integer -> [Int]
insert (x:xs) n
  | n == 0 = (x + 1) : xs
  | otherwise = x : insert xs (n - 1)
insert [] _ = []

removeOne :: [Int] -> [Int]
removeOne = map (\x -> if x > 0 then x - 1 else 0)

plotRow :: [Int] -> String
plotRow = map (\x -> if x > 0 then '*' else ' ')

plot :: [Int] -> [String]
plot l
  | all (== 0) l = []
  | otherwise = plotRow l : plot (removeOne l)

histogram :: [Integer] -> String
histogram l = unlines (reverse ("==========\n0123456789" :
                                plot (foldl insert (replicate 10 0) l)))
