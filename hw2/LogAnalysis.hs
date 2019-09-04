{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage s@('E':_) = LogMessage (Error (read n)) (read ts) (unwords message)
  where (_ : n : ts : message) = words s

parseMessage s@('I':_) = LogMessage Info (read ts) (unwords message)
  where (_  : ts : message) = words s

parseMessage s@('W':_) = LogMessage Warning (read ts) (unwords message)
  where (_  : ts : message) = words s

parseMessage s = Unknown s


parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s


getTimeStamp :: LogMessage -> Maybe TimeStamp
getTimeStamp (Unknown _) = Nothing
getTimeStamp (LogMessage (Error _) ts _) = Just ts
getTimeStamp (LogMessage Info ts _) = Just ts
getTimeStamp (LogMessage Warning ts _) = Just ts

getMessage :: LogMessage -> String
getMessage (Unknown m) = m
getMessage (LogMessage (Error _) _ m) = m
getMessage (LogMessage Info _ m) = m
getMessage (LogMessage Warning _ m) = m

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm Leaf = Node Leaf lm Leaf
insert lm (Node left lm' right) = if getTimeStamp lm < getTimeStamp lm'
                                     then (Node (insert lm left) lm' right)
                                     else (Node left lm' (insert lm right))


build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = (inOrder left) ++ [lm] ++ (inOrder right)


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = map getMessage . filter p . inOrder . build $ lms
  where p (LogMessage (Error n) _ _) = n >= 50
        p _ = False


