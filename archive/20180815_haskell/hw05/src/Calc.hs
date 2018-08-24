{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM
import Data.Maybe
import qualified Data.Map as M

-- Exercise 1
eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = case parseExp ExprT.Lit ExprT.Add ExprT.Mul s of
              Just x -> Just (eval x)
              Nothing -> Nothing

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 x) (Mod7 y) = lit $ x + y
  mul (Mod7 x) (Mod7 y) = lit $ x * y

-- Exercise 5
instance Expr Program where
  lit x = [PushI x]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- Exercise 6
class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Calc.Lit
  add = Calc.Add
  mul = Calc.Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x _ = Just x
  add x y m
    | isNothing (x m) || isNothing (y m) = Nothing
    | otherwise = Just $ fromJust (x m) + fromJust (y m)
  mul x y m
    | isNothing (x m) || isNothing (y m) = Nothing
    | otherwise = Just $ fromJust (x m) * fromJust (y m)

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs expr = expr $ M.fromList vs
