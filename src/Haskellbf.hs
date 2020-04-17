import Data.Char

main :: IO()
main = do
  putStrLn "Please enter your bf code"
  instructions <- getLine
  let pureInstructions = filter isInstruction instructions
      memory = tapeNeeded (length instructions)

  executionLoop (pureInstructions,memory)

executionLoop :: (String,[Int]) -> IO()
executionLoop (str,xs) = do
  let output = evaluate (str,xs)

  if fst output == "" || fst output == "," then
    return ()
    else if (head (fst output)) == '.' then do
      putChar (chr . head . snd $ output)
      executionLoop (tail . fst $ output, snd output)
    else if (head . fst $ output) == ',' then do
      putStrLn "Please enter an integer number to insert into the current cell"
      input <- numPrompt
      executionLoop (tail . fst $ output,(input : (tail . snd $ output)))
    else do
      putStrLn "ahhh"
      return ()

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

evaluate :: (String,[Int]) -> (String,[Int])
evaluate (_,[])                    = error"no tape assigned"
evaluate ([],xs)                   = ([],xs)
evaluate ((ch:str),(xs)) | ch == '>' = evaluate (str,(moveRight xs))
                         | ch == '<' = evaluate (str,(moveLeft xs))
                         | ch == '+' = evaluate (str,(increment xs))
                         | ch == '-' = evaluate (str,(decrement xs))
                         | ch == '.' = (ch:str,xs)
                         | ch == ',' = (ch:str,xs)
                         | ch == '[' && head xs > 0 = evaluate ((ch:str),snd (evaluate ((loopContents str),xs)))
                         | ch == '[' = evaluate (beyondLoop str,xs)
                         | ch == ']' = evaluate (str,xs)
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
