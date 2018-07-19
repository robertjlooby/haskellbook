---
title: Chapter 17
---

---

\begin{code}

module Chapter17 where

import Control.Applicative
import Data.List (elemIndex)
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Identity a =
  Identity a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure = const $ Constant mempty
  (Constant a) <*> (Constant a') = Constant $ a <> a'

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

toMyList :: [a] -> List a
toMyList [] = Nil
toMyList (a:as) = Cons a (toMyList as)

repeat' :: a -> List a
repeat' a = Cons a (repeat' a)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons a as) = Cons a (take' (n - 1) as)

instance Semigroup (List a) where
  Nil <> ys = ys
  Cons x xs <> ys = Cons x $ xs <> ys

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    as <- arbitrary
    return $ toMyList as

instance Eq a => EqProp (List a) where
  xs =-= ys = take' 3000 xs `eq` take' 3000 ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> listAs = (f <$> listAs) <> (fs <*> listAs)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' $ repeat' a
  ZipList' allFs@(Cons _ _) <*> ZipList' allAs@(Cons _ _) = ZipList' $ zip' allFs allAs
    where zip' Nil _ = Nil
          zip' _ Nil = Nil
          zip' (Cons f fs) (Cons a as) = Cons (f a) (zip' fs as)
  _ <*> _ = ZipList' Nil

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    oneof [ return $ Failure e
          , return $ Success a
          ]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  Failure e <*> Failure e' = Failure $ e <> e'
  Success f <*> Success a  = Success $ f a
  Failure e <*> _  = Failure e
  _ <*> Failure e  = Failure e

data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  Pair f f' <*> Pair a a' = Pair (f a) (f' a')

data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  Two a f <*> Two a' b = Two (a <> a') (f b)

data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  Three a b f <*> Three a' b' c = Three (a <> a') (b <> b') (f c)

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  Three' a f f' <*> Three' a' b b' = Three' (a <> a') (f b) (f' b')

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  Four a b c f <*> Four a' b' c' d = Four (a <> a') (b <> b') (c <> c') (f d)

data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return $ Four' a a' a'' b

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Monoid a => Applicative (Four' a) where
  pure b = Four' mempty mempty mempty b
  Four' a1 a2 a3 f <*> Four' a1' a2' a3' b = Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') (f b)

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

spec :: SpecWith ()
spec = do
  describe "Exercises: Lookups" $ do
    it "# 1" $
      -- original: (+ 3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
      (+ 3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6]) `shouldBe` Just (9 :: Int)
    it "# 2" $
      let
        y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]
        z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]
      in
        -- original: (,) y z
        (,) <$> y <*> z `shouldBe` Just ((6, 5) :: (Integer, Integer))
    it "# 3" $
      let
        x = elemIndex 3 [1..5]
        y = elemIndex 4 [1..5]
      in
        -- original: max x y
        max <$> x <*> y `shouldBe` Just (3 :: Int)
    it "# 4" $
      let
        xs = [1, 2, 3]
        ys = [4, 5, 6]
        x = lookup 3 $ zip xs ys
        y = lookup 2 $ zip xs ys
      in
        -- original: sum $ (,) x y
        sum <$> ((,) <$> x <*> y) `shouldBe` Just (5 :: Int)

  describe "Identity" $ do
    testBatch $ applicative (undefined :: Identity (Int, Int, Int))

  describe "Constant" $ do
    testBatch $ applicative (undefined :: Constant String (Int, Int, Int))

  describe "Exercises: Fixer Upper" $ do
    it "# 1" $
      -- original: const <$> Just "Hello" <*> "World"
      const <$> Just "Hello" <*> return "World" `shouldBe` Just "Hello"
    it "# 2" $
      -- original: (,,,) Just 90 <*> Just 10 Just "Tierness" [1, 2, 3]
      (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> return [1, 2, 3] `shouldBe` Just (90, 10, "Tierness", [1, 2, 3])

  describe "List" $ do
    testBatch $ applicative (undefined :: List (Int, Int, Int))

  describe "ZipList'" $ do
    testBatch $ applicative (undefined :: ZipList' (Int, Int, Int))
    it "works with the example from the book" $
      ZipList' (toMyList [(+ 9), (* 2), (+ 8)]) <*> ZipList' (toMyList [1..3]) `shouldBe` ZipList' (toMyList [10, 4, 11])
    it "works with an infinite list" $
      ZipList' (toMyList [(+ 9), (* 2), (+ 8)]) <*> ZipList' (toMyList $ repeat 1) `shouldBe` ZipList' (toMyList [10, 2, 9])

  describe "Validation" $ do
    testBatch $ applicative (undefined :: Validation String (Int, Int, Int))

  describe "Pair" $ do
    testBatch $ applicative (undefined :: Pair (Int, Int, Int))

  describe "Two" $ do
    testBatch $ applicative (undefined :: Two String (Int, Int, Int))

  describe "Three" $ do
    testBatch $ applicative (undefined :: Three String String (Int, Int, Int))

  describe "Three'" $ do
    testBatch $ applicative (undefined :: Three' String (Int, Int, Int))

  describe "Four" $ do
    testBatch $ applicative (undefined :: Four String String String (Int, Int, Int))

  describe "Four'" $ do
    testBatch $ applicative (undefined :: Four' String (Int, Int, Int))

  describe "Combinations" $ do
    it "generates combinations from short input lists" $
      combos "ab" "c" "de" `shouldBe` [('a', 'c', 'd'), ('a', 'c', 'e'), ('b', 'c', 'd'), ('b', 'c', 'e')]
    it "generates combinations of stop/vowel/stop" $
      let
        stops = "pbtdkg"
        vowels = "aeiou"
      in
        length (combos stops vowels stops) `shouldBe` length stops * length vowels * length stops
\end{code}
