---
title: Chapter 12
---

---

\begin{code}
module Chapter12 where

import Data.List
import Test.Hspec

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel = flip elem "aeiou"

theToA :: String -> String
theToA "the" = "a"
theToA word = word

replaceThe :: String -> String
replaceThe = unwords . map theToA . words

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go 0 (words s)
  where
    startsWithVowel (c:_) = isVowel c
    go :: Integer -> [String] -> Integer
    go count [] = count
    go count [_] = count
    go count ("the":(w:ws))
      | startsWithVowel w = go (count + 1) ws
      | otherwise = go count (w : ws)
    go count (_:ws) = go count ws

countVowels :: String -> Int
countVowels = length . filter isVowel

countConsonants :: String -> Int
countConsonants = length . filter (not . isVowel)

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s
  | countVowels s <= countConsonants s = Just $ Word' s
  | otherwise = Nothing

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | n == 0 = Just Zero
  | otherwise = fmap Succ (integerToNat (n - 1))

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:_) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr catIfJust []
  where
    catIfJust Nothing acc = acc
    catIfJust (Just a) acc = a:acc

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr flipIfJust (Just [])
  where
    flipIfJust Nothing _ = Nothing
    flipIfJust _ Nothing = Nothing
    flipIfJust (Just a) (Just acc) = Just (a:acc)

lefts' :: [Either a b] -> [a]
lefts' = foldr catIfLeft []
  where
    catIfLeft (Left a) acc = a:acc
    catIfLeft _ acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr catIfRight []
  where
    catIfRight (Right b) acc = b:acc
    catIfRight _ acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr catEither ([], [])
  where
    catEither (Left a) (as, bs) = (a:as, bs)
    catEither (Right b) (as, bs) = (as, b:bs)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just $ f b

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
  case f b of
    Nothing -> []
    Just (a, b') -> a : myUnfoldr f b'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfoldBT :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfoldBT f a =
  case f a of
    Nothing -> Leaf
    Just (leftA, b, rightA) -> Node (unfoldBT f leftA) b (unfoldBT f rightA)

treeBuild :: Integer -> BinaryTree Integer
treeBuild depth = unfoldBT intTreeFn 0
  where
    intTreeFn n
      | n >= depth = Nothing
      | otherwise = Just (n + 1, n, n + 1)

spec :: SpecWith ()
spec = do
  describe "replaceThe" $ do
    it "for first word" $
      replaceThe "the cow loves us" `shouldBe` "a cow loves us"
    it "for last word" $
      replaceThe "cow loves the" `shouldBe` "cow loves a"
    it "for middle word" $
      replaceThe "cow the loves us" `shouldBe` "cow a loves us"

  describe "countTheBeforeVowel" $ do
    it "with none" $
      countTheBeforeVowel "the cow, the dog" `shouldBe` 0
    it "with all" $
      countTheBeforeVowel "the evil cow, the awesome dog" `shouldBe` 2
    it "with some" $
      countTheBeforeVowel "the evil cow, the dog" `shouldBe` 1
    it "with repeat thes" $
      countTheBeforeVowel "the the evil cow, the dog" `shouldBe` 1

  describe "countVowels" $ do
    it "with none" $
      countVowels "th cw, th dg" `shouldBe` 0
    it "with some" $
      countVowels "the cow, the dog" `shouldBe` 4

  describe "mkWord" $ do
    it "the is a word" $
      mkWord "the" `shouldBe` Just (Word' "the")
    it "at is a word" $
      mkWord "at" `shouldBe` Just (Word' "at")
    it "that is a word" $
      mkWord "that" `shouldBe` Just (Word' "that")
    it "eat is not a word" $
      mkWord "eat" `shouldBe` Nothing
    it "a is not a word" $
      mkWord "a" `shouldBe` Nothing

  describe "natToInteger" $ do
    it "for Zero" $
      natToInteger Zero `shouldBe` 0
    it "for 1" $
      natToInteger (Succ Zero) `shouldBe` 1
    it "for 2" $
      natToInteger (Succ (Succ Zero)) `shouldBe` 2

  describe "integerToNat" $ do
    it "for 0" $
      integerToNat 0 `shouldBe` Just Zero
    it "for 1" $
      integerToNat 1 `shouldBe` Just (Succ Zero)
    it "for 2" $
      integerToNat 2 `shouldBe` Just (Succ (Succ Zero))
    it "for -1" $
      integerToNat (-1) `shouldBe` Nothing

  describe "isJust" $ do
    it "for Nothing" $
      isJust Nothing `shouldBe` False
    it "for Just" $
      isJust (Just "hi") `shouldBe` True

  describe "isNothing" $ do
    it "for Nothing" $
      isNothing Nothing `shouldBe` True
    it "for Just" $
      isNothing (Just "hi") `shouldBe` False

  describe "mayybee" $ do
    it "for Nothing" $
      mayybee 0 (+ 1) Nothing `shouldBe` 0
    it "for Just" $
      mayybee 0 (+ 1) (Just 1) `shouldBe` 2

  describe "fromMaybe" $ do
    it "for Nothing" $
      fromMaybe 0 Nothing `shouldBe` 0
    it "for Just" $
      fromMaybe 0 (Just 1) `shouldBe` 1

  describe "listToMaybe" $ do
    it "for Nothing" $
      listToMaybe [] `shouldBe` (Nothing :: Maybe Int)
    it "for Just" $
      listToMaybe [1, 2, 3] `shouldBe` Just 1

  describe "maybeToList" $ do
    it "for Nothing" $
      maybeToList Nothing `shouldBe` ([] :: [Int])
    it "for Just" $
      maybeToList (Just 1) `shouldBe` [1]

  describe "catMaybes" $ do
    it "for Nothing" $
      catMaybes [Nothing, Nothing] `shouldBe` ([] :: [Int])
    it "for Just" $
      catMaybes [Just 1, Nothing, Just 2] `shouldBe` [1, 2]

  describe "flipMaybe" $ do
    it "for Nothing" $
      flipMaybe [Just 1, Nothing, Just 2] `shouldBe` Nothing
    it "for Just" $
      flipMaybe [Just 1, Just 2, Just 3] `shouldBe` Just [1, 2, 3]

  describe "lefts'" $ do
    it "gets Lefts" $
      lefts' [Left 1, Right "hi", Left 2, Right "hello"] `shouldBe` [1, 2]

  describe "rights'" $ do
    it "gets Rights" $
      rights' [Left 1, Right "hi", Left 2, Right "hello"] `shouldBe` ["hi", "hello"]

  describe "partitionEithers'" $ do
    it "groups Lefts and Rights" $
      partitionEithers' [Left 1, Right "hi", Left 2, Right "hello"] `shouldBe` ([1, 2], ["hi", "hello"])

  describe "either'" $ do
    it "for a Left" $
      either' (+ 1) (+ 2) (Left 10) `shouldBe` 11
    it "for a Right" $
      either' (+ 1) (+ 2) (Right 10) `shouldBe` 12

  describe "eitherMaybe'" $ do
    it "for a Left" $
      eitherMaybe' (+ 1) (Left 10) `shouldBe` Nothing
    it "for a Right" $
      eitherMaybe' (+ 1) (Right 10) `shouldBe` Just 11

  describe "myIterate" $ do
    it "iterates" $
      take 10 (myIterate (+1) 0) `shouldBe` take 10 (iterate (+1) 0)

  describe "myUnfoldr" $ do
    it "unfolds" $
      take 10 (myUnfoldr (\n -> Just (n, n + 1)) 0) `shouldBe` take 10 (unfoldr (\n -> Just (n, n + 1)) 0)

  describe "betterIterate" $ do
    it "iterates" $
      take 10 (betterIterate (+1) 0) `shouldBe` take 10 (iterate (+1) 0)

  describe "treeBuild" $ do
    it "for 0" $
      treeBuild 0 `shouldBe` Leaf
    it "for 1" $
      treeBuild 1 `shouldBe` Node Leaf 0 Leaf
    it "for 2" $
      treeBuild 2 `shouldBe` Node (Node Leaf 1 Leaf) 0 (Node Leaf 1 Leaf)
\end{code}
