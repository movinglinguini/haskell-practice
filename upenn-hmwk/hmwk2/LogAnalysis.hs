{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Exercise 1 --
type BadMessage = Bool

failMessage :: [String] -> BadMessage
failMessage ws
  | not(any (\w -> w == head(ws)) ["E", "W", "I"]) = True
  | head(ws) == "E" && length ws <= 3 = True
  | otherwise = False

getMessageType :: String -> MessageType
getMessageType s
  | head(words(s)) == "I" = Info
  | head(words(s)) == "W" = Warning
  | head(words(s)) == "E" = Error (read(head(tail(words(s))))::Int)

parseMessage :: String -> LogMessage
parseMessage s
  | failMessage (words(s)) = Unknown s
  | head (words(s)) == "E" = LogMessage (getMessageType s) (read(head(tail(tail(words(s)))))::Int) (unwords(tail(tail(tail(words(s))))))
  | otherwise = LogMessage (getMessageType s) (read(head(tail(words(s))))) (unwords(tail(tail(words(s)))))

parse :: String -> [LogMessage]
parse s
  | null (words(s)) = []
  | otherwise = [(parseMessage(head(lines(s))))] ++ (parse(unlines(tail(lines(s)))))

-- Exercise 2 --
testLeft :: LogMessage -> MessageTree -> Bool
testLeft (LogMessage _ time1 _) (Node _ (LogMessage _ time2 _) _) = time1 <= time2

testRight :: LogMessage -> MessageTree -> Bool
testRight (LogMessage _ time1 _) (Node _ (LogMessage _ time2 _) _) = time1 >= time2

getLeft :: MessageTree -> MessageTree
getLeft (Node left _ _) = left

getRight :: MessageTree -> MessageTree
getRight (Node _ _ right) = right

getMessage :: MessageTree -> LogMessage
getMessage (Node _ lm _) = lm

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert (LogMessage typ time string) Leaf = Node (Leaf) (LogMessage typ time string) (Leaf)
insert lm mt
  | testLeft lm mt = Node (insert (lm) (getLeft mt)) (getMessage(mt)) (getRight(mt))
  | testRight lm mt = Node (getLeft(mt)) (getMessage(mt)) (insert (lm) (getRight mt))

-- Exercise 3 --
build :: [LogMessage] -> MessageTree
build lms
  | null lms = Leaf
  | otherwise = insert (head lms) (build (tail lms))

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder (Leaf) = []
inOrder (Node left lm right) = (inOrder left) ++ [lm] ++ (inOrder right)

-- Exercise 5
sorted :: [LogMessage] -> [LogMessage]
sorted lms = inOrder(build(lms))

getSeverity :: LogMessage -> Int
getSeverity (Unknown _) = 0
getSeverity (LogMessage Warning _ _) = 0
getSeverity (LogMessage Info _ _) = 0
getSeverity (LogMessage (Error s) _ _) = s

getLogMessage :: LogMessage -> String
getLogMessage (Unknown s) = s
getLogMessage (LogMessage _ _ s) = s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = (whatWentWrong_ (sorted(lms)))

whatWentWrong_ :: [LogMessage] -> [String]
whatWentWrong_ sorted_lms
  | null sorted_lms = []
  | getSeverity (head(sorted_lms)) <= 50 = whatWentWrong_(tail(sorted_lms))
  | otherwise = [getLogMessage(head(sorted_lms))] ++ whatWentWrong_(tail(sorted_lms))