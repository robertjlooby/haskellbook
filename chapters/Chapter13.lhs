---
title: Chapter 13
---

---

\begin{code}
module Chapter13 where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)
import Test.Hspec

newtype WordList =
  Wordlist [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "/usr/share/dict/words"
  return . Wordlist $ (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (Wordlist aw) <- allWords
  return . Wordlist $ (filter gameLength aw)
  where gameLength w =
          let l = length w
          in  l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (Wordlist wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
  Puzzle String [Maybe Char] [Char]
  deriving (Eq)

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
      ++ " Guessed so far: " ++ guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (map (const Nothing) word) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = elem c word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
      then Just wordChar
      else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again"
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 7 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"

main' :: IO ()
main' = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

spec :: SpecWith ()
spec = do
  describe "freshPuzzle" $ do
    it "for hello" $
      freshPuzzle "hello" `shouldBe` Puzzle "hello" [Nothing, Nothing, Nothing, Nothing, Nothing] []

  describe "charInWord" $ do
    it "for found" $
      charInWord (freshPuzzle "hello") 'h' `shouldBe` True
    it "for not found" $
      charInWord (freshPuzzle "hello") 'z' `shouldBe` False

  describe "alreadyGuessed" $ do
    it "for found" $
      alreadyGuessed (Puzzle "h" [Nothing] ['z']) 'z' `shouldBe` True
    it "for not found" $
      alreadyGuessed (Puzzle "h" [Nothing] []) 'h' `shouldBe` False

  describe "renderPuzzleChar" $ do
    it "for Nothing" $
      renderPuzzleChar Nothing `shouldBe` '_'
    it "for a letter" $
      renderPuzzleChar (Just 'h') `shouldBe` 'h'

  describe "fillInCharacter" $ do
    it "for not found" $
      fillInCharacter (Puzzle "hi" [Nothing, Nothing] "") 'z' `shouldBe` (Puzzle "hi" [Nothing, Nothing] "z")
    it "for found" $
      fillInCharacter (Puzzle "hi" [Nothing, Nothing] "") 'h' `shouldBe` (Puzzle "hi" [Just 'h', Nothing] "h")
\end{code}
