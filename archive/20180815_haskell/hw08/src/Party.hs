{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Data.Tree
import Employee

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons x (GL xs f) = GL (x:xs) (empFun x + f)

instance Semigroup GuestList where
  (<>) (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node r s) = f r (map (treeFold f) s)

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel x l = (maximum' $ map fst l,
                 maximum' $ map (glCons x . snd) l)
  where maximum' = foldl moreFun mempty

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . treeFold nextLevel

-- Exercise 5
formatGuestList :: GuestList -> String
formatGuestList (GL l f) = unlines $
  ("Total fun: " ++ show f) : map empName l

main :: IO ()
main = readFile "../data/company.txt" >>=
       putStr . formatGuestList . maxFun . read
