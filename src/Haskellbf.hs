import Data.Char

main :: IO()
main = do
  putStrLn "Please enter your bf code"
  instructions <- getLine
  let pureInstructions = filter isInstruction instructions
      memory = tapeNeeded (length instructions)

  executionLoop pureInstructions memory

executionLoop :: String -> [Int] -> IO()
executionLoop [] xs = return ()
executionLoop "," xs = return ()
executionLoop (ch:str) xs = do
    if ch == '.' then do
      putChar (chr . head  $ xs)
      executionLoop str xs
    else if ch == ',' then do
      putStrLn "Please enter an integer number to insert into the current cell"
      input <- numPrompt
      executionLoop str (input : tail xs)
    else if ch == '[' && head xs > 0 then do
      executionLoop ((loopContents str) ++ (ch:str)) xs
    else if ch == '[' then
      executionLoop (beyondLoop str) xs
    else
      executionLoop str (evaluate ch xs)

numPrompt :: Read a => IO a
numPrompt = do
  line <- getLine
  return (read line)

blankTape :: [Int]
blankTape = [0,0..]

isInstruction :: Char -> Bool
isInstruction c
  | c == '>' || c == '<' || c == '+' || c == '-' ||
    c == '.' || c == '[' || c == ']' || c == ','= True
  | otherwise = False

tapeNeeded :: Int -> [Int]
tapeNeeded x = take x blankTape

evaluate :: Char -> [Int] -> [Int]
evaluate _ []              = error"no tape assigned"
evaluate ch xs | ch == '>' = moveRight xs
               | ch == '<' = moveLeft xs
               | ch == '+' = increment xs
               | ch == '-' = decrement xs
               | ch == '.' = error ". should be handled in Loop"
               | ch == ',' = error ", should be handled in Loop"
               | ch == '[' = xs
               | ch == ']' = xs
               | ch == ']' = xs
               | otherwise = error "not a valid instruction"

moveRight :: [Int] -> [Int]
moveRight xs = tail xs ++ [head xs]

moveLeft :: [Int] -> [Int]
moveLeft xs = last xs : init xs

increment :: [Int] -> [Int]
increment (x:xs) | x == 255 = 0 : xs
                 | otherwise = x+1 : xs

decrement :: [Int] -> [Int]
decrement (x:xs) | x == 0 = 255 : xs
                 | otherwise = x-1 : xs

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
