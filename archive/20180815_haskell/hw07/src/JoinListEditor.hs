module Main where

import Buffer
import Scrabble
import Sized
import JoinList
import Editor

main :: IO ()
main = runEditor editor $
       (fromString :: String -> JoinList (Score, Size) String) $ unlines
       [ "This buffer is for notes you don't want to save, and for",
         "evaluation of steam valve coefficients.",
         "To load a different file, type the character L followed",
         "by the name of the file."
       ]
