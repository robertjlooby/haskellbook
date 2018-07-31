---
title: Chapter 7
---

---

\begin{code}
module Chapter07 where

import Test.Hspec

tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    (xLast, _) = x `divMod` 10
    (_, d) = xLast `divMod` 10

hundredsDigit :: Integral a => a -> a
hundredsDigit x = d
  where
    (xLast, _) = x `divMod` 100
    (_, d) = xLast `divMod` 10

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y b =
  case b of
    True -> x
    False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
  | b = x
  | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

myWords :: String -> [String]
myWords "" = []
myWords string = firstWord : myWords restOfString
  where
    firstWord = takeWhile (/= ' ') string
    restOfString = dropWhile (== ' ') . dropWhile (/= ' ') $ string

myLines :: String -> [String]
myLines "" = []
myLines string = firstLine : myLines restOfString
  where
    firstLine = takeWhile (/= '\n') string
    restOfString = dropWhile (== '\n') . dropWhile (/= '\n') $ string

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c string = firstLine : splitOn c restOfString
  where
    firstLine = takeWhile (/= c) string
    restOfString = dropWhile (== c) . dropWhile (/= c) $ string

myWords2 :: String -> [String]
myWords2 = splitOn ' '

myLines2 :: String -> [String]
myLines2 = splitOn '\n'

mySqr = [x ^ 2 | x <- [1 .. 5]]

myCube = [y ^ 3 | y <- [1 .. 5]]

tuples = [(x, y) | x <- mySqr, y <- myCube]

tuplesUnder50 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

howManyTuplesUnder50 = length tuplesUnder50

multiplesOfThree = filter (\x -> x `mod` 3 == 0) [1 .. 30]

numberOfMultiplesOfThree = length multiplesOfThree

myFilter = filter (\word -> not $ elem word ["a", "an", "the"]) . words

spec :: SpecWith ()
spec = do
  describe "tensDigit" $ do
    it "for 1" $ tensDigit 1 `shouldBe` 0
    it "for 123" $ tensDigit 123 `shouldBe` 2
    it "for 1234" $ tensDigit 1234 `shouldBe` 3
  describe "hundredsDigit" $ do
    it "for 1" $ hundredsDigit 1 `shouldBe` 0
    it "for 123" $ hundredsDigit 123 `shouldBe` 1
    it "for 1234" $ hundredsDigit 1234 `shouldBe` 2
  describe "foldBool with case" $ do
    it "with True" $ foldBool1 1 2 True `shouldBe` 1
    it "with False" $ foldBool1 1 2 False `shouldBe` 2
  describe "foldBool with guard" $ do
    it "with True" $ foldBool2 1 2 True `shouldBe` 1
    it "with False" $ foldBool2 1 2 False `shouldBe` 2
  describe "g" $ do it "works" $ g (1 +) (2, 'a') `shouldBe` (3, 'a')
  describe "roundTrip" $ do it "works" $ roundTrip 4 `shouldBe` id 4
  describe "myWords" $ do
    it "works" $
      myWords "sheryl wants  fun" `shouldBe` ["sheryl", "wants", "fun"]
  describe "myLines" $ do
    it "works" $
      myLines "sheryl\nwants\nfun" `shouldBe` ["sheryl", "wants", "fun"]
  describe "splitOn" $ do
    it "spaces" $
      splitOn ' ' "sheryl wants  fun" `shouldBe` ["sheryl", "wants", "fun"]
    it "newlines" $
      splitOn '\n' "sheryl\nwants\nfun" `shouldBe` ["sheryl", "wants", "fun"]
    it "myWords2" $
      myWords2 "sheryl wants  fun" `shouldBe` ["sheryl", "wants", "fun"]
    it "myLines2" $
      myLines2 "sheryl\nwants\nfun" `shouldBe` ["sheryl", "wants", "fun"]
  describe "lists" $ do
    it "tuples" $
      tuples `shouldBe`
      [ (1 ^ 2, 1 ^ 3)
      , (1 ^ 2, 2 ^ 3)
      , (1 ^ 2, 3 ^ 3)
      , (1 ^ 2, 4 ^ 3)
      , (1 ^ 2, 5 ^ 3)
      , (2 ^ 2, 1 ^ 3)
      , (2 ^ 2, 2 ^ 3)
      , (2 ^ 2, 3 ^ 3)
      , (2 ^ 2, 4 ^ 3)
      , (2 ^ 2, 5 ^ 3)
      , (3 ^ 2, 1 ^ 3)
      , (3 ^ 2, 2 ^ 3)
      , (3 ^ 2, 3 ^ 3)
      , (3 ^ 2, 4 ^ 3)
      , (3 ^ 2, 5 ^ 3)
      , (4 ^ 2, 1 ^ 3)
      , (4 ^ 2, 2 ^ 3)
      , (4 ^ 2, 3 ^ 3)
      , (4 ^ 2, 4 ^ 3)
      , (4 ^ 2, 5 ^ 3)
      , (5 ^ 2, 1 ^ 3)
      , (5 ^ 2, 2 ^ 3)
      , (5 ^ 2, 3 ^ 3)
      , (5 ^ 2, 4 ^ 3)
      , (5 ^ 2, 5 ^ 3)
      ]
    it "tuplesUnder50" $
      tuplesUnder50 `shouldBe`
      [ (1 ^ 2, 1 ^ 3)
      , (1 ^ 2, 2 ^ 3)
      , (1 ^ 2, 3 ^ 3)
      , (2 ^ 2, 1 ^ 3)
      , (2 ^ 2, 2 ^ 3)
      , (2 ^ 2, 3 ^ 3)
      , (3 ^ 2, 1 ^ 3)
      , (3 ^ 2, 2 ^ 3)
      , (3 ^ 2, 3 ^ 3)
      , (4 ^ 2, 1 ^ 3)
      , (4 ^ 2, 2 ^ 3)
      , (4 ^ 2, 3 ^ 3)
      , (5 ^ 2, 1 ^ 3)
      , (5 ^ 2, 2 ^ 3)
      , (5 ^ 2, 3 ^ 3)
      ]
    it "howManyTuplesUnder50" $ howManyTuplesUnder50 `shouldBe` 15
  describe "filtering" $ do
    it "multiplesOfThree" $
      multiplesOfThree `shouldBe` [3, 6, 9, 12, 15, 18, 21, 24, 27, 30]
    it "numberOfMultiplesOfThree" $ numberOfMultiplesOfThree `shouldBe` 10
    it "myFilter" $
      myFilter "the brown dog was a goof" `shouldBe`
      ["brown", "dog", "was", "goof"]
\end{code}
