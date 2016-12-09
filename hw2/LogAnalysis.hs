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

main :: IO ()
main = do
    print $ parseMessage "E 2 562 help help"
    print $ parseMessage "I 562 2 help help"
    print $ parseMessage "W 562 help help"
    print $ parseMessage "D 2 562 help help"
    print $ parseMessage "D"
