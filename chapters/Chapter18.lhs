---
title: Chapter 18
---

---

\begin{code}
module Chapter18 where

import Control.Monad (ap, join)
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes hiding (bind)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a, return $ Second b]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  First a <*> _ = First a
  _ <*> First a = First a
  Second f <*> Second b = Second (f b)

instance Monad (Sum a) where
  return = pure
  First a >>= _ = First a
  Second b >>= f = f b

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure = const NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

data PhhhbbbtttEither b a
  = PLeft a
  | PRight b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ PLeft a, return $ PRight b]

instance (Eq a, Eq b) => EqProp (PhhhbbbtttEither a b) where
  (=-=) = eq

instance Functor (PhhhbbbtttEither b) where
  fmap _ (PRight b) = PRight b
  fmap f (PLeft a) = PLeft (f a)

instance Applicative (PhhhbbbtttEither b) where
  pure = PLeft
  PRight b <*> _ = PRight b
  _ <*> PRight b = PRight b
  PLeft f <*> PLeft a = PLeft (f a)

instance Monad (PhhhbbbtttEither b) where
  return = pure
  PLeft a >>= f = f a
  PRight b >>= _ = PRight b

data Identity a =
  Identity a
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

toMyList :: [a] -> List a
toMyList [] = Nil
toMyList (a:as) = Cons a (toMyList as)

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

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons a as >>= f = f a <> (as >>= f)

j :: Monad m => m (m a) -> m a
j mma = mma >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = ma >>= (\a -> mb >>= (\b -> return (f a b)))

a :: Monad m => m a -> m (a -> b) -> m b
a ma mab = mab >>= \f -> f <$> ma

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a:as) f = f a >>= \b -> (b :) <$> meh as f

flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id

spec :: SpecWith ()
spec = do
  describe "bind" $ do
    it "maps and then concats a list" $
      bind (\n -> replicate n n) [1, 2, 3] `shouldBe` [1, 2, 2, 3, 3, 3]
  describe "Sum" $ do
    testBatch $ monad (undefined :: Sum String (Int, Int, Int))
  describe "Nope" $ do testBatch $ monad (undefined :: Nope (Int, Int, Int))
  describe "PhhhbbbtttEither" $ do
    testBatch $ monad (undefined :: PhhhbbbtttEither String (Int, Int, Int))
  describe "Identity" $ do
    testBatch $ monad (undefined :: Identity (Int, Int, Int))
  describe "List" $ do testBatch $ monad (undefined :: List (Int, Int, Int))
  describe "j" $ do
    it "concats lists" $ j [[1, 2], [], [3]] `shouldBe` [1, 2, 3]
    it "joins two Justs" $ j (Just (Just 1)) `shouldBe` Just 1
    it "joins a Just Nothing" $
      j (Just Nothing) `shouldBe` (Nothing :: Maybe Int)
    it "joins a Nothing" $ j Nothing `shouldBe` (Nothing :: Maybe Int)
  describe "l1" $ do it "lifts" $ l1 (+ 1) [1, 2, 3] `shouldBe` [2, 3, 4]
  describe "l2" $ do
    it "lifts more" $
      l2 (+) [1, 2, 3] [4, 5, 6] `shouldBe` [5, 6, 7, 6, 7, 8, 7, 8, 9]
  describe "a" $ do it "applies" $ a [1, 2, 3] [(+ 1)] `shouldBe` [2, 3, 4]
  describe "meh" $ do
    it "transforms list to list of lists" $
      meh [1, 2] (\n -> replicate n n) `shouldBe` [[1, 2], [1, 2]]
    it "transforms list to Maybe of list" $
      meh [1, 2] (\n -> Just n) `shouldBe` Just [1, 2]
  describe "flipType" $ do
    it "transforms a list of Justs to a Just of a list" $
      flipType [Just 1, Just 2] `shouldBe` Just [1, 2]
    it "transforms a list with a Nothing to a Nothing" $
      flipType [Just 1, Nothing] `shouldBe` Nothing
\end{code}
