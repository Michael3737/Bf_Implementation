import Data.Char

blankTape :: [Int]
blankTape = [0,0..]

run :: String -> String
run xs = snd (evaluate (filter isInstruction xs) (tapeNeeded (length xs),"")) -- returning the second of the tuple, to be solved with IO

isInstruction :: Char -> Bool
isInstruction c
  | c == '>' || c == '<' || c == '+' || c == '-' || 
    c == '.' || c == '[' || c == ']' || c == ','= True -- no implementation for , yet
  | otherwise = False

tapeNeeded :: Int -> [Int]
tapeNeeded x = take x blankTape

evaluate :: String -> ([Int],String) -> ([Int],String)
evaluate _ ([],r)                    = error"no tape assigned"
evaluate [] (xs,r)                   = (xs,r)
evaluate (ch:str) (xs,r) | ch == '>' = evaluate str ((moveRight xs),r)
                         | ch == '<' = evaluate str ((moveLeft xs),r)
                         | ch == '+' = evaluate str ((increment xs),r)
                         | ch == '-' = evaluate str ((decrement xs),r)
                         | ch == '.' = evaluate str (xs,update xs r)
                         | ch == ',' = evaluate str (xs,r) -- implement me
                         | ch == '[' && head xs > 0 = evaluate (ch:str) (evaluate (loopContents str) (xs,r))
                         | ch == '[' = evaluate (beyondLoop str) (xs,r)
                         | ch == ']' = evaluate str (xs,r)
                         | otherwise = error "not a valid instruction"

moveRight :: [Int] -> [Int]
moveRight xs = tail xs ++ [head xs]

moveLeft :: [Int] -> [Int]
moveLeft xs = last xs : init xs

increment :: [Int] -> [Int]
increment (x:xs) | x+1 <= 128 = x+1 : xs
                 | otherwise = 0 : xs

decrement :: [Int] -> [Int]
decrement (x:xs) | x-1 >= 0 = x-1 : xs
                 | otherwise = 128 : xs

update :: [Int] -> String -> String
update xs r = r ++ [(chr (head xs))]

beyondLoop :: String -> String
beyondLoop [] = []
beyondLoop (ch:str) | ch == '[' = beyondLoop (beyondLoop str)
                    | ch == ']' = str
                    | otherwise = beyondLoop str

loopContents :: String -> String
loopContents [] = []
loopContents (ch:str) | ch == ']' = []
                      | ch == '[' = "[" ++ loopContents str ++ "]" ++ loopContents (beyondLoop str)
                      | otherwise = [ch] ++ loopContents str
