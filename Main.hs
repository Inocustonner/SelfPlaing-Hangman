module Main where

import Control.Applicative
import Data.Char

unique :: Eq a => [a] -> [a]
unique xs = foldr (\x uniq -> if x `notElem` uniq then x : uniq else uniq) [] xs

                                          -- guessed, ungessed, (the word)
data GameState = GameState {
               guessed :: [Char],
               unguessed :: [Char],
               triesLeft :: Int,
               theWord :: String
               }

data GameResult = Won | Lost deriving(Eq)

{- 
Game Loop:
  1. prompt char
  2. if char is right update 'guessed' and 'unguessed'
  3. decrement 'triesLeft'
  4. forward state to 'GameLoop'

Game Loop (unguessed == [] || triesLeft == 0):
  1. 'return GameState'
-}

-- Take a word with gaps, word without gaps, char and replace gaps where the char stays in the word
replaceGaps :: String -> String -> Char -> String
replaceGaps gapWord word ch = getZipList $ f <$> ZipList gapWord <*> ZipList word
  where 
    f '_' wCh = if wCh == ch then wCh else '_'
    f gCh _ = gCh


updateGameState :: Char -> GameState -> GameState
updateGameState ch (GameState g ung tries word) = GameState (replaceGaps g word ch) (filter (/= ch) ung) (tries - 1) word

-- Take gameState and return updated one
gameLoop :: GameState -> IO GameState
gameLoop state@(GameState guessed unguessed triesLeft word )
  | unguessed == [] || triesLeft == 0 = return state
  | otherwise = do
                printState

                (ch:_) <- getLine
                gameLoop $ updateGameState ch (GameState guessed unguessed triesLeft word)
  where printState = do
                     let line1 = "word: " ++ guessed
                         line2 = "tries left: " ++ show triesLeft
                         borderCh = '.'
                     putStrLn $ replicate (max (length line1) (length line2)) borderCh
                     putStrLn line1
                     putStrLn line2
                     putStrLn $ replicate (max (length line1) (length line2)) borderCh

-- takes the word and returns the result of the game
game :: String -> IO GameResult
game word = do
            state <- gameLoop $ let len = length word 
                                in GameState (replicate len '_') (unique word) (len + 3) word
            if unguessed state == []
              then return Won
              else return Lost

main :: IO ()
main = do
       putStrLn "Hangman\n"
       putStr "Enter your word: "
       word <- getLine
       result <- game word
       putStrLn $"The word was: " ++ word
       if result == Won
         then putStrLn "You Won!"
         else putStrLn "You Lost"
