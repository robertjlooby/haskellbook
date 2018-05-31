---
title: Chapter 11
---

---

\begin{code}
{-# LANGUAGE FlexibleInstances #-}

module Chapter11 where

import Data.Char
import Test.Hspec

data Price =
    Price Integer deriving (Eq, Show)

data Size =
    Size Integer deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle =
    Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 100)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Double where
  tooMany n = n > 42

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, n') = tooMany $ n + n'

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = preorder left ++ [a] ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder right ++ [a] ++ postorder left

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = foldTree f (f a leftSide) right
  where
    leftSide = foldTree f b left

testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

cipher :: String -> String -> String
cipher _ "" = ""
cipher key (' ':cs) = ' ' : cipher key cs
cipher (k:ks) (c:cs) = ciphered : cipher (ks ++ [k]) cs
  where
    ordA = ord 'A'
    ciphered = chr . (+ ordA) . (flip mod 26) . (+ (ord k - ordA)) . (flip (-) ordA) . ord $ c

isSubSeqOf :: Eq a => [a] -> [a] -> Bool
isSubSeqOf [] _ = True
isSubSeqOf _ [] = False
isSubSeqOf first@(a:as) (b:bs)
  | a == b = isSubSeqOf as bs
  | otherwise = isSubSeqOf first bs

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\word@(a:as) -> (word, toUpper a : as)) . words

capitalizeWord :: String -> String
capitalizeWord (a:as) = toUpper a : as

spec :: SpecWith ()
spec = do
  describe "vehicles" $ do
    describe "isCar" $ do
      it "for a car" $
        isCar myCar `shouldBe` True
      it "for a plane" $
        isCar doge `shouldBe` False

    describe "isPlane" $ do
      it "for a car" $
        isPlane myCar `shouldBe` False
      it "for a plane" $
        isPlane doge `shouldBe` True

    it "areCars for a list of vehicles" $
      areCars [myCar, urCar, clownCar, doge] `shouldBe` [True, True, True, False]

    it "getManu for a car" $
      getManu myCar `shouldBe` Mini

  describe "logic goats" $ do
    describe "for an (Int, String)" $ do
      it "ok if 42" $
        tooMany (42 :: Int, "hi") `shouldBe` False
      it "not ok if over 42" $
        tooMany (43 :: Int, "hi") `shouldBe` True

    describe "for an (Int, Int)" $ do
      it "ok if sum is 42" $
        tooMany (21 :: Int, 21 :: Int) `shouldBe` False
      it "not ok if sum is over 42" $
        tooMany (21 :: Int, 22 :: Int) `shouldBe` True

    describe "for (Num a, TooMany a) => (a, a)" $ do
      it "ok if sum is 42" $
        tooMany (21 :: Double, 21 :: Double) `shouldBe` False
      it "not ok if sum is over 42" $
        tooMany (21 :: Double, 22 :: Double) `shouldBe` True

  describe "binary tree" $ do
    describe "mapTree" $ do
      it "for testTree'" $
        mapTree (+ 1) testTree' `shouldBe` Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

    describe "to list functions" $ do
      it "preorder for testTree" $
        preorder testTree `shouldBe` [2, 1, 3]
      it "inorder for testTree" $
        inorder testTree `shouldBe` [1, 2, 3]
      it "postorder for testTree" $
        postorder testTree `shouldBe` [3, 2, 1]

    describe "foldTree" $ do
      it "for testTree'" $
        foldTree (+) 1 testTree' `shouldBe` 9

  describe "vigenere cipher" $ do
    it "encodes MEET AT DAWN with ALLY" $
      cipher "ALLY" "MEET AT DAWN" `shouldBe` "MPPR AE OYWY"

  describe "as-patterns" $ do
    describe "isSubSeqOf" $ do
      it "for same list" $
        isSubSeqOf "ALLY" "ALLY" `shouldBe` True
      it "for a subseq" $
        isSubSeqOf "ALLY" "AxLxLxY" `shouldBe` True
      it "for not a subseq" $
        isSubSeqOf "ALLY" "ALlY" `shouldBe` False
      it "for not a subseq 2" $
        isSubSeqOf "ALLY" "AL" `shouldBe` False

  describe "capitalizeWords" $ do
    it "for some words" $
      capitalizeWords "hello world" `shouldBe` [("hello", "Hello"), ("world", "World")]

  describe "capitalizeWord" $ do
    it "for a word" $
      capitalizeWord "hello" `shouldBe` "Hello"
    it "for a cap word" $
      capitalizeWord "Hello" `shouldBe` "Hello"
\end{code}
