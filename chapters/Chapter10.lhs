---
title: Chapter 10
---

---

\begin{code}
module Chapter10 where

import Data.Time
import Test.Hspec

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, World!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  , DbNumber 9003
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr go []
  where
    go (DbDate date) dates = date : dates
    go _             dates = dates

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr go []
  where
    go (DbNumber number) numbers = number : numbers
    go _                 numbers = numbers

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb database = fromIntegral (sum numbers) / fromIntegral (length numbers)
  where numbers = filterDbNumber database

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

factorials :: [Integer]
factorials = scanl (*) 1 [2..]

stops  = "pbtdkg"
vowels = "aeiou"

seekritFunk :: String -> Double
seekritFunk x = totalWordLength / numberOfWords
  where
    totalWordLength = fromIntegral . sum . map length . words $ x
    numberOfWords = fromIntegral . length . words $ x

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr ((||) . (== a)) False

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 = any . (==)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a as -> if f a then a:as else as) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (a:as) = foldl (\acc a' -> if f acc a' == GT then acc else a') a as

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (a:as) = foldl (\acc a' -> if f acc a' == LT then acc else a') a as

spec :: SpecWith ()
spec = do
  describe "database processing" $ do
    it "filterDbDate gets the dates" $
      filterDbDate theDatabase `shouldBe` [ UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)
                                          , UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)
                                          ]

    it "filterDbNumber gets the numbers" $
      filterDbNumber theDatabase `shouldBe` [ 9001, 9003 ]

    it "mostRecent gets most recent date" $
      mostRecent theDatabase `shouldBe` UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)

    it "sumDb sums the numbers" $
      sumDb theDatabase `shouldBe` 18004

    it "avgDb averages the numbers" $
      avgDb theDatabase `shouldBe` 9002

  describe "scan exercises" $ do
    it "first 20 fibs" $
      take 20 fibs `shouldBe` [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765]

    it "fibs under 100" $
      takeWhile (< 100) fibs `shouldBe` [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89]

    it "first 5 factorials" $
      take 5 factorials `shouldBe` [1, 2, 6, 24, 120]

  describe "chapter review" $ do
    it "all stop-vowel-stop combos" $
      length [[stop1, vowel, stop2] | stop1 <- stops, vowel <- vowels, stop2 <- stops] `shouldBe` length stops * length vowels * length stops
    it "all p-vowel-stop combos" $
      length [[stop1, vowel, stop2] | stop1 <- stops, vowel <- vowels, stop2 <- stops, stop1 == 'p'] `shouldBe` length vowels * length stops
    it "calculates the average word length" $
      seekritFunk "hi there you hello" `shouldBe` 3.75

  describe "rewrite using folds" $ do
    describe "myOr" $ do
      it "empty list" $
        myOr [] `shouldBe` False
      it "with a True" $
        myOr [False, True] `shouldBe` True
      it "all False" $
        myOr [False, False] `shouldBe` False

    describe "myAny" $ do
      it "empty list" $
        myAny even [] `shouldBe` False
      it "False" $
        myAny even [1, 3, 5] `shouldBe` False
      it "True" $
        myAny odd [1, 3, 5] `shouldBe` True

    describe "myElem" $ do
      it "empty list" $
        myElem 1 [] `shouldBe` False
      it "False" $
        myElem 1 [2..10] `shouldBe` False
      it "True" $
        myElem 1 [1..10] `shouldBe` True

    describe "myElem2" $ do
      it "empty list" $
        myElem2 1 [] `shouldBe` False
      it "False" $
        myElem2 1 [2..10] `shouldBe` False
      it "True" $
        myElem2 1 [1..10] `shouldBe` True

    describe "myReverse" $ do
      it "empty" $
        myReverse "" `shouldBe` ""
      it "String" $
        myReverse "blah" `shouldBe` "halb"
      it "Nums" $
        myReverse [1..5] `shouldBe` [5, 4, 3, 2, 1]

    describe "myMap" $ do
      it "empty" $
        myMap (+ 1) [] `shouldBe` []
      it "Nums" $
        myMap (+1) [1..5] `shouldBe` [2..6]

    describe "myFilter" $ do
      it "empty" $
        myFilter even [] `shouldBe` []
      it "evens" $
        myFilter even [1..5] `shouldBe` [2, 4]
      it "odds" $
        myFilter odd [1..5] `shouldBe` [1, 3, 5]

    describe "squish" $ do
      it "flattens" $
        squish [[1..3], [4..6]] `shouldBe` [1..6]

    describe "squishMap" $ do
      it "maps and flattens" $
        squishMap (\x -> [1, x, 3]) [2, 4] `shouldBe` [1, 2, 3, 1, 4, 3]

    describe "squishAgain" $ do
      it "flattens" $
        squishAgain [[1..3], [4..6]] `shouldBe` [1..6]

    describe "myMaximumBy" $ do
      it "gets the max" $
        myMaximumBy compare [1, 53, 9001, 10] `shouldBe` 9001
      it "returns the last value that returned GT" $
        myMaximumBy (\_ _ -> GT) [1, 53, 9001, 10] `shouldBe` 1
      it "returns the last value that returned GT" $
        myMaximumBy (\_ _ -> LT) [1, 53, 9001, 10] `shouldBe` 10

    describe "myMinimumBy" $ do
      it "gets the min" $
        myMinimumBy compare [1, 53, 9001, 10] `shouldBe` 1
      it "returns the last value that returned LT" $
        myMinimumBy (\_ _ -> GT) [1, 53, 9001, 10] `shouldBe` 10
      it "returns the last value that returned LT" $
        myMinimumBy (\_ _ -> LT) [1, 53, 9001, 10] `shouldBe` 1
\end{code}
