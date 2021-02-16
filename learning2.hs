{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage line =
    case words line of
        ("E":n:t:xs) -> LogMessage (Error (read n)) (read t) (unwords xs)
        ("I":n:xs) -> LogMessage Info (read n) (unwords xs)
        ("W":n:xs) -> LogMessage Warning (read n) (unwords xs)
        xs -> Unknown (unwords xs)

parse :: String -> [LogMessage]
parse x = map parseMessage (lines x)

-- Exercise 2

getTimestamp :: LogMessage -> Int
getTimestamp msg = case msg of
    LogMessage _ t _ -> t
    _ -> 0

insert :: LogMessage -> MessageTree -> MessageTree
insert msg tree =
    case msg of
        Unknown _ -> tree
        _ -> case tree of
            Leaf -> Node Leaf msg Leaf
            (Node l m r) -> if getTimestamp msg < getTimestamp m
                then Node (insert msg l) m r
                else Node l m (insert msg r)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
    Node Leaf m Leaf -> [m]
    Node l m Leaf -> inOrder l ++ [m]
    Node Leaf m r -> m : inOrder r
    Node l m r -> inOrder l ++ [m] ++ inOrder r
    _ -> []

isSevereError :: LogMessage -> Bool
isSevereError msg = case msg of
    LogMessage (Error s) _ _ -> s > 50
    _ -> False

getString :: LogMessage -> String
getString (LogMessage _ _ s) = s
getString _ = ""


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong s = map getString (filter isSevereError s)