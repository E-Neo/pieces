{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Char

newtype Score = Score Int deriving (Show, Num)

getScore :: Score -> Int
getScore (Score x) = x

instance Semigroup Score where
  (<>) (Score x) (Score y) = Score (x + y)

instance Monoid Score where
  mempty = Score 0

score :: Char -> Score
score c
  | c' `elem` "AEILNORSTU" = Score 1
  | c' `elem` "DG" = Score 2
  | c' `elem` "BCMP" = Score 3
  | c' `elem` "FHVWY" = Score 4
  | c' `elem` "K" = Score 5
  | c' `elem` "JX" = Score 8
  | c' `elem` "QZ" = Score 10
  | otherwise = Score 0
  where c' = toUpper c

scoreString :: String -> Score
scoreString = sum . map score
