---
title: Chapter 21
---

---

\begin{code}
module Chapter21 where

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity a' = Identity $ a <> a'

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldr f acc (Identity a) = f a acc

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

data Constant a b = Constant
  { getConstant :: a
  } deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Semigroup a => Semigroup (Constant a b) where
  Constant a <> Constant a' = Constant $ a <> a'

instance Monoid a => Monoid (Constant a b) where
  mempty = Constant mempty

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldr _ acc _ = acc

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a

data Optional a
  = Nada
  | Yep a
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [return Nada, Yep <$> arbitrary]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Semigroup a => Semigroup (Optional a) where
  Nada <> Nada = Nada
  Yep a <> Nada = Yep a
  Nada <> Yep a = Yep a
  Yep a <> Yep a' = Yep $ a <> a'

instance Semigroup a => Monoid (Optional a) where
  mempty = Nada

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldr _ acc Nada = acc
  foldr f acc (Yep a) = f a acc

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Ord, Show)

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
  arbitrary = toMyList <$> arbitrary

instance Eq a => EqProp (List a) where
  xs =-= ys = take' 3000 xs `eq` take' 3000 ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Foldable List where
  foldr _ acc Nil = acc
  foldr f acc (Cons a as) = f a (foldr f acc as)

instance Traversable List where
  traverse f Nil = pure Nil
  traverse f (Cons a as) = (Cons <$> f a) <*> traverse f as

data Three a b c =
  Three a
        b
        c
  deriving (Eq, Ord, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldr f acc (Three _ _ c) = f c acc

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

data Pair a b =
  Pair a
       b
  deriving (Eq, Ord, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  Pair a b <> Pair a' b' = Pair (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty = Pair mempty mempty

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldr f acc (Pair _ b) = f b acc

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

data Big a b =
  Big a
      b
      b
  deriving (Eq, Ord, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance (Semigroup a, Semigroup b) => Semigroup (Big a b) where
  Big a b1 b2 <> Big a' b1' b2' = Big (a <> a') (b1 <> b1') (b2 <> b2')

instance (Monoid a, Monoid b) => Monoid (Big a b) where
  mempty = Big mempty mempty mempty

instance Functor (Big a) where
  fmap f (Big a b1 b2) = Big a (f b1) (f b2)

instance Foldable (Big a) where
  foldr f acc (Big _ b1 b2) = f b1 (f b2 acc)

instance Traversable (Big a) where
  traverse f (Big a b1 b2) = Big a <$> f b1 <*> f b2

data Bigger a b =
  Bigger a
         b
         b
         b
  deriving (Eq, Ord, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance (Semigroup a, Semigroup b) => Semigroup (Bigger a b) where
  Bigger a b1 b2 b3 <> Bigger a' b1' b2' b3' =
    Bigger (a <> a') (b1 <> b1') (b2 <> b2') (b3 <> b3')

instance (Monoid a, Monoid b) => Monoid (Bigger a b) where
  mempty = Bigger mempty mempty mempty mempty

instance Functor (Bigger a) where
  fmap f (Bigger a b1 b2 b3) = Bigger a (f b1) (f b2) (f b3)

instance Foldable (Bigger a) where
  foldr f acc (Bigger _ b1 b2 b3) = f b1 (f b2 (f b3 acc))

instance Traversable (Bigger a) where
  traverse f (Bigger a b1 b2 b3) = Bigger a <$> f b1 <*> f b2 <*> f b3

data S n a =
  S (n a)
    a
  deriving (Eq, Ord, Show)

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq

instance (Semigroup a, Semigroup (n a)) => Semigroup (S n a) where
  S n a <> S n' a' = S (n <> n') (a <> a')

instance (Monoid a, Monoid (n a)) => Monoid (S n a) where
  mempty = S mempty mempty

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (f <$> n) (f a)

instance Foldable n => Foldable (S n) where
  foldr f acc (S n a) = foldr f (f a acc) n

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a)
         a
         (Tree a)
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    left <- arbitrary
    right <- arbitrary
    frequency
      [(5, return Empty), (5, return $ Leaf a), (1, return $ Node left a right)]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

instance Semigroup (Tree a) where
  Empty <> tree = tree
  Leaf a <> tree = Node Empty a tree
  Node left a right <> tree = Node left a (right <> tree)

instance Monoid (Tree a) where
  mempty = Empty

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

instance Foldable Tree where
  foldr _ acc Empty = acc
  foldr f acc (Leaf a) = f a acc
  foldr f acc (Node left a right) = foldr f (f a (foldr f acc right)) left

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node left a right) =
    Node <$> traverse f left <*> f a <*> traverse f right

spec :: SpecWith ()
spec = do
  describe "Identity" $
    testBatch $ traversable (undefined :: Identity (String, String, String))
  describe "Constant" $
    testBatch $ traversable (undefined :: Constant Int (String, String, String))
  describe "Optional" $
    testBatch $ traversable (undefined :: Optional (String, String, String))
  describe "List" $
    testBatch $ traversable (undefined :: List (String, String, String))
  describe "Three" $
    testBatch $
    traversable (undefined :: Three Int Int (String, String, String))
  describe "Pair" $
    testBatch $ traversable (undefined :: Pair Int (String, String, String))
  describe "Big" $
    testBatch $ traversable (undefined :: Big Int (String, String, String))
  describe "Bigger" $
    testBatch $ traversable (undefined :: Bigger Int (String, String, String))
  describe "S" $
    testBatch $ traversable (undefined :: S [] (String, String, String))
  describe "Tree" $
    testBatch $ traversable (undefined :: Tree (String, String, String))
\end{code}
