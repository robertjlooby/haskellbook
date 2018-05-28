---
title: Chapter 9
---

---

\begin{code}
module Chapter09 where

import Data.Char
import Test.Hspec

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (a:as) (b:bs) = (a, b) : myZip as bs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (a:as) (b:bs) = f a b : myZipWith f as bs

myZip2 = myZipWith (,)

caps :: String -> String
caps "" = ""
caps (c:cs)
  | isUpper c = c : caps cs
  | otherwise = caps cs

capitalize :: String -> String
capitalize "" = ""
capitalize (c:cs) = toUpper c : cs

allCaps :: String -> String
allCaps "" = ""
allCaps (c:cs) = toUpper c : allCaps cs

firstLetterCapitalized :: String -> Char
firstLetterCapitalized = toUpper . head

cypher :: Int -> String -> String
cypher _ "" = ""
cypher n (c:cs)
  | elem c ['a'..'z'] = shift lowerAOffset c : cypher n cs
  | elem c ['A'..'Z'] = shift upperAOffset c : cypher n cs
  | otherwise = c : cypher n cs
  where
    lowerAOffset = ord 'a'
    upperAOffset = ord 'A'
    shift aOffset = chr . (+ aOffset) . (flip mod 26) . (+ n) . (flip (-) aOffset) . ord

unCypher :: Int -> String -> String
unCypher _ "" = ""
unCypher n (c:cs)
  | elem c ['a'..'z'] = shift lowerAOffset c : unCypher n cs
  | elem c ['A'..'Z'] = shift upperAOffset c : unCypher n cs
  | otherwise = c : cypher n cs
  where
    lowerAOffset = ord 'a'
    upperAOffset = ord 'A'
    shift aOffset = chr . (+ aOffset) . (flip mod 26) . (flip (-) n) . (flip (-) aOffset) . ord

myOr :: [Bool] -> Bool
myOr (True:_) = True
myOr (False:xs) = myOr xs
myOr [] = False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (a:as)
  | f a = True
  | otherwise = myAny f as

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (a:as)
  | e == a = True
  | otherwise = myElem e as

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 e as = myAny (== e) as

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a:as) = reverse as ++ [a]

squish :: [[a]] -> [a]
squish [] = []
squish (a:as) = a ++ squish as

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (a:as) = f a ++ squishMap f as

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [a] = a
myMaximumBy f (a:as)
  | f a maxOfRest == GT = a
  | otherwise = maxOfRest
  where maxOfRest = myMaximumBy f as

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [a] = a
myMinimumBy f (a:as)
  | f a minOfRest == LT = a
  | otherwise = minOfRest
  where minOfRest = myMinimumBy f as

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

spec :: SpecWith ()
spec = do
  describe "myZip" $ do
    it "same length" $
      myZip [1, 2] ['a', 'b'] `shouldBe` [(1, 'a'), (2, 'b')]
    it "first longer" $
      myZip [1, 2] ['a'] `shouldBe` [(1, 'a')]
    it "second longer" $
      myZip [1] ['a', 'b'] `shouldBe` [(1, 'a')]

  describe "myZipWith" $ do
    it "same length" $
      myZipWith (+) [1, 2] [3, 4] `shouldBe` [4, 6]
    it "first longer" $
      myZipWith (+) [1, 2] [3] `shouldBe` [4]
    it "second longer" $
      myZipWith (+) [1] [3, 4] `shouldBe` [4]

  describe "myZip2" $ do
    it "same length" $
      myZip2 [1, 2] ['a', 'b'] `shouldBe` [(1, 'a'), (2, 'b')]
    it "first longer" $
      myZip2 [1, 2] ['a'] `shouldBe` [(1, 'a')]
    it "second longer" $
      myZip2 [1] ['a', 'b'] `shouldBe` [(1, 'a')]

  describe "caps" $ do
    it "keeps the capitals" $
      caps "HbEfLrLxO" `shouldBe` "HELLO"

  describe "capitalize" $ do
    it "capitalizes the first letter" $
      capitalize "julie" `shouldBe` "Julie"

  describe "allCaps" $ do
    it "capitalizes all the letters" $
      allCaps "woot" `shouldBe` "WOOT"

  describe "firstLetterCapitalized" $ do
    it "gives the first letter capitalized" $
      firstLetterCapitalized "julie" `shouldBe` 'J'

  describe "cypher" $ do
    it "rotates 1 to the right" $
      cypher 1 ['a'..'z'] `shouldBe` ['b'..'z'] ++ ['a']
    it "rotates 1 to the right" $
      cypher 1 ['A'..'Z'] `shouldBe` ['B'..'Z'] ++ ['A']
    it "rotates 3 to the right" $
      cypher 3 ['a'..'z'] `shouldBe` ['d'..'z'] ++ ['a', 'b', 'c']
    it "rotates 3 to the right" $
      cypher 3 ['A'..'Z'] `shouldBe` ['D'..'Z'] ++ ['A', 'B', 'C']
    it "uncyphers lower" $
      unCypher 3 (cypher 3 ['a'..'z']) `shouldBe` ['a'..'z']
    it "uncyphers upper" $
      unCypher 3 (cypher 3 ['A'..'Z']) `shouldBe` ['A'..'Z']

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
    it "True" $
      myReverse [1..5] `shouldBe` [5, 4, 3, 2, 1]

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

  describe "myMinimumBy" $ do
    it "gets the min" $
      myMinimumBy compare [1, 53, 9001, 10] `shouldBe` 1

  describe "myMaximum" $ do
    it "gets the max" $
      myMaximum [1, 53, 9001, 10] `shouldBe` 9001

  describe "myMinimum" $ do
    it "gets the min" $
      myMinimum [1, 53, 9001, 10] `shouldBe` 1
\end{code}
