module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage m = case words m of
  "I":ts:ss -> LogMessage Info (read ts :: TimeStamp) (unwords ss)
  "W":ts:ss -> LogMessage Warning (read ts :: TimeStamp) (unwords ss)
  "E":e:ts:ss -> LogMessage (Error (read e :: Int))
                 (read ts :: TimeStamp) (unwords ss)
  _ -> Unknown m

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node lmt lm0@(LogMessage _ ts0 _) rmt)
  | ts >= ts0 = Node lmt lm0 (insert lm rmt)
  | ts < ts0 = Node (insert lm lmt) lm0 rmt
insert _ mt = mt

-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lmt lm rmt) = inOrder lmt ++ [lm] ++ inOrder rmt

-- Exercise 5
hasWentWrong :: LogMessage -> Bool
hasWentWrong (LogMessage (Error e) _ _) = e >= 50
hasWentWrong _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ m) = m
getMessage (Unknown m) = m

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = map getMessage (inOrder (build (filter hasWentWrong lms)))
