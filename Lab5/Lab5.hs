module Lab5 where

import Data.Char (isSpace, toLower)
import Data.List (dropWhile, dropWhileEnd)
import System.IO (hFlush, stdout, readFile')
import System.IO.Error (tryIOError)
import Text.Read (readMaybe)

------------------------------------------------------------------------------

-- | QA: in the form of a decision tree
-- | And leaves are answers (strings), nodes are questions (strings) with two branches
-- | Left branch is 'yes' answer, right branch is 'no' answer
data QA
  = Question String QA QA
  | Answer String
  deriving (Show, Read)

yesAnswer :: String
yesAnswer = "yes"

noAnswer :: String
noAnswer = "no"

question :: String -> QA -> QA -> QA
question = Question

guess :: String -> QA
guess = Answer

-- | Gameplay helpers
strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace

-- | Display prompt message and get user input
-- | Strip whitespace and convert to lowercase before returning
prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  strip <$> getLine

-- | Prompts user to type yes or no and returns the result
-- | Will retry if user input is invalid 
promptYesAndNo :: IO Bool
promptYesAndNo = do
  putStr "(yes/no): "
  hFlush stdout
  ans <- map toLower . strip <$> getLine
  if ans == yesAnswer
    then do
      return True
    else
      if ans == noAnswer
        then do
          return False
        else do
          putStrLn "Please answer 'yes' or 'no'!!!"
          promptYesAndNo

-- | Default Quiz Data
-- | Taken from Lab5 diagram
defaultQA :: QA
defaultQA =
  question
    "Is this person from Europe?"
    ( question
        "Is this person a scientist?"
        (guess "Marie Curie")
        (guess "Queen Elisabeth II")
    )
    ( question
        "Is this person an actor?"
        (guess "Marilyn Monroe")
        (guess "Hillary Clinton")
    )

play :: QA -> IO QA
play (Question q yesTree noTree) = do
  putStrLn q
  ans <- promptYesAndNo
  if ans then 
    do
      -- fall into yes subtree
      newYesTree <- play yesTree
      return (Question q newYesTree noTree)
   else 
    do
       -- fall into no subtree
       newNoTree <- play noTree
       return (Question q yesTree newNoTree)

play (Answer a) = do
  putStrLn ("My guess: Is it " ++ a ++ "?")
  ans <- promptYesAndNo
  if ans
    then do
      putStrLn "Hurray! I won!"
      return (Answer a)
    else 
      do
        -- Wrong guess, learn new person
        putStrLn "OK - you won this time."
        putStrLn "Just curious: Who was your famous person?"
        correctPerson <- prompt "Enter name: "

        putStrLn ("Give me a question for which the answer for " ++ correctPerson ++ " is 'yes', and for " ++ a ++ " is 'no'.")
        newQuestion <- prompt "Enter question: "

        -- Construct the new tree node
        return (question newQuestion (guess correctPerson) (guess a))

-- | Create questions.qa from a decision tree
writeQFile :: QA -> IO ()
writeQFile qaTree = writeFile "questions.qa" (show qaTree)

-- | Parse string into QA tree
parseQATree :: String -> Maybe QA
parseQATree str = readMaybe str :: Maybe QA

-- | Game Loop
gameLoop :: QA -> IO ()
gameLoop qaTree = do
  -- Play once
  newQATree <- play qaTree

  -- Ask to play again
  putStrLn "Play again?"
  againAns <- promptYesAndNo
  if againAns
    then gameLoop newQATree
    else do
      putStrLn "Saving QA file..."
      writeQFile newQATree
      putStrLn "Bye!"

-- | Main
main :: IO ()
main = do
  -- Try to read existing questions.qa file
  qaTreeOrError <- tryIOError (readFile' "questions.qa")
  -- tryIOError returns Either IOError String
  qaTree <- case qaTreeOrError of
    Left err -> do
      putStrLn ("Error: questions.qa: " ++ show err)
      putStrLn "Using the default quiz data."
      return defaultQA
    Right rawString ->
      case parseQATree rawString of
        Just tree -> do
          putStrLn "Loaded quiz data from questions.qa."
          return tree
        Nothing -> do
          putStrLn "Error: Invalid data in questions.qa."
          putStrLn "Using the default quiz data."
          return defaultQA
  gameLoop qaTree