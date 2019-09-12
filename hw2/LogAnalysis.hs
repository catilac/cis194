{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Parse a message string and return a LogMessage
parseMessage :: String -> LogMessage
parseMessage msg = case ws of
  "E":errNum:timeStamp:ss -> LogMessage 
    (Error . read $ errNum) 
    (read timeStamp) 
    (unwords ss)
  "I":timeStamp:ss -> LogMessage Info (read timeStamp) (unwords ss)
  "W":timeStamp:ss -> LogMessage Warning (read timeStamp) (unwords ss)
  _ -> Unknown "This is not in the right format"
  where ws = words msg 

-- parse the lines of a log file
parse :: String -> [LogMessage]
parse logs = (map parseMessage) . lines $ logs

-- insert LogMessage into a MessageTree datastructure
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm Leaf = (Node Leaf lm Leaf)
insert lm mt
  | timeStamp > ts = (Node  left currLm (insert lm right)) -- insert right
  | timeStamp < ts = (Node (insert lm left) currLm right) -- insert left
  | otherwise = (Node Leaf lm Leaf)
  where 
    (LogMessage _ timeStamp _) = lm
    (Node left currLm right) = mt
    (LogMessage _ ts _) = currLm

-- take a list of LogMessages and build a tree
build :: [LogMessage] -> MessageTree
build lms = foldr insert Leaf lms

-- in order traversal of MessageTree, return sorted list of LogMessages
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l msg r) = (inOrder l) ++ [msg] ++  (inOrder r)

-- Take an unsorted list of LogMessages
-- return sorted list of messages for
-- errors with a severity level of >= 50
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages =
  map getMessage . filter isErrorGT50 $ msgs
  where
    msgs = inOrder . build $ messages -- sorted messages
    isErrorGT50 (LogMessage (Error n) _ _) = n >= 50
    isErrorGT50 _ = False
    getMessage (LogMessage _ _ msg) = msg
    getMessage _ = "Should not get this"
