{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

isDigit :: Char -> Bool
isDigit = (`elem` "1234567890")

isNumber :: String -> Bool
isNumber = all isDigit

hasThreeElems :: [a] -> Bool
hasThreeElems (_:_:_:_) = True
hasThreeElems _ = False

parseMessage :: String -> LogMessage
parseMessage m
    | not $ hasThreeElems ws    = Unknown m
    | w1 == "I" && isNumber w2  = LogMessage Info (read w2) (unwords $ w3:ws')
    | w1 == "W" && isNumber w2  = LogMessage Warning (read w2) (unwords $ w3:ws')
    | w1 == "E" && isNumber w2 && isNumber w3
                                = LogMessage (Error $ read w2) (read w3) (unwords $ ws')
    | otherwise                 = Unknown m
    where
        ws = words m
        (w1:w2:w3:ws') = ws

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

timestamp :: LogMessage -> TimeStamp
timestamp (LogMessage _ ts _) = ts
timestamp (Unknown _) = error "No timestamp."

highPriority :: LogMessage -> Bool
highPriority (LogMessage (Error p) _ _)
    | p >= 50    = True
    | otherwise = False
highPriority _  = False

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m (Node left m' right)
    | (timestamp m) <= (timestamp m') = Node (insert m left) m' right
    | otherwise                       = Node left m' (insert m right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = inOrder left ++ (m : inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (\(LogMessage _ _ m) -> m) . inOrder . build . filter highPriority

main :: IO ()
main = testWhatWentWrong parse whatWentWrong "error.log" >>= mapM_ print
