---
title: Chapter 20
---

---

\begin{code}
module Chapter20 where

import Data.Foldable
import Data.Monoid
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes hiding (bind)

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e = foldr (\a acc -> acc || a == e) False

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr getMin Nothing
  where
    getMin a Nothing = Just a
    getMin a (Just acc)
      | a < acc = Just a
      | otherwise = Just acc

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr getMax Nothing
  where
    getMax a Nothing = Just a
    getMax a (Just acc)
      | a > acc = Just a
      | otherwise = Just acc

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ acc -> acc + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldr (<>) mempty

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty

data Constant a b =
  Constant a
  deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr _ acc _ = acc

data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f acc (Two _ b) = f b acc

data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f acc (Three _ _ c) = f c acc

data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'

data Four' a b =
  Four' a
        b
        b
        b
  deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''

filterF ::
     (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap filterIt
  where
    filterIt a
      | f a = pure a
      | otherwise = mempty

spec :: SpecWith ()
spec = do
  describe "library functions" $ do
    it "sum" $ do
      sum' [1, 2, 3, 4] `shouldBe` sum [1, 2, 3, 4]
      sum' [] `shouldBe` sum []
      sum' (Just 2) `shouldBe` sum (Just 2)
      sum' Nothing `shouldBe` sum Nothing
    it "product" $ do
      product' [1, 2, 3, 4] `shouldBe` product [1, 2, 3, 4]
      product' [] `shouldBe` product []
      product' (Just 2) `shouldBe` product (Just 2)
      product' Nothing `shouldBe` product Nothing
    it "elem" $ do
      elem' 2 [1, 2, 3, 4] `shouldBe` elem 2 [1, 2, 3, 4]
      elem' 2 [] `shouldBe` elem 2 []
      elem' 2 (Just 2) `shouldBe` elem 2 (Just 2)
      elem' 2 Nothing `shouldBe` elem 2 Nothing
      elem' 3 (Just 2) `shouldBe` elem 3 (Just 2)
    it "minimum" $ do
      minimum' [1, 2, 3, 4] `shouldBe` Just 1
      minimum' [] `shouldBe` (Nothing :: Maybe Int)
      minimum' (Just 2) `shouldBe` (Just 2)
      minimum' Nothing `shouldBe` (Nothing :: Maybe Int)
    it "maximum" $ do
      maximum' [1, 2, 3, 4] `shouldBe` Just 4
      maximum' [] `shouldBe` (Nothing :: Maybe Int)
      maximum' (Just 2) `shouldBe` (Just 2)
      maximum' Nothing `shouldBe` (Nothing :: Maybe Int)
    it "null" $ do
      null' [1, 2, 3, 4] `shouldBe` null [1, 2, 3, 4]
      null' [] `shouldBe` null []
      null' (Just 2) `shouldBe` null (Just 2)
      null' Nothing `shouldBe` null Nothing
    it "length" $ do
      length' [1, 2, 3, 4] `shouldBe` length [1, 2, 3, 4]
      length' [] `shouldBe` length []
      length' (Just 2) `shouldBe` length (Just 2)
      length' Nothing `shouldBe` length Nothing
    it "toList" $ do
      toList' [1, 2, 3, 4] `shouldBe` toList [1, 2, 3, 4]
      toList' [] `shouldBe` toList ([] :: [Int])
      toList' (Just 2) `shouldBe` toList (Just 2)
      toList' Nothing `shouldBe` toList (Nothing :: Maybe Int)
    it "fold" $ do
      fold' ["ab", "cd", "ef"] `shouldBe` fold ["ab", "cd", "ef"]
      fold' [] `shouldBe` fold ([] :: [String])
      fold' (Just "hi") `shouldBe` fold (Just "hi")
      fold' Nothing `shouldBe` fold (Nothing :: Maybe String)
    it "foldMap" $ do
      foldMap' show [1, 2, 3, 4] `shouldBe` foldMap show [1, 2, 3, 4]
      foldMap' show ([] :: [Int]) `shouldBe` ""
      foldMap' show (Just 2) `shouldBe` foldMap show (Just 2)
      foldMap' show (Nothing :: Maybe Int) `shouldBe` ""
  describe "chapter exercises" $ do
    it "Constant" $ do
      foldMap id (Constant 1 :: Constant Int String) `shouldBe` ""
      foldr (+) 5 (Constant "hi" :: Constant String Int) `shouldBe` 5
    it "Two" $ do
      foldMap id (Two 1 "hi") `shouldBe` "hi"
      foldr (+) 5 (Two "hi" 1) `shouldBe` 6
    it "Three" $ do
      foldMap id (Three 1 "hi" "there") `shouldBe` "there"
      foldr (+) 5 (Three "hi" 1 2) `shouldBe` 7
    it "Three'" $ do
      foldMap id (Three' 1 "hi" "there") `shouldBe` "hithere"
      foldr (+) 5 (Three' "hi" 1 2) `shouldBe` 8
    it "Four'" $ do
      foldMap id (Four' 1 "hi" "there" "you") `shouldBe` "hithereyou"
      foldr (+) 5 (Four' "hi" 1 2 3) `shouldBe` 11
    it "filterF" $ do
      filterF even [1 .. 6] `shouldBe` [2, 4, 6]
      filterF (> 'l') "hello world" `shouldBe` "owor"
\end{code}
