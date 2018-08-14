---
title: Chapter 25
---

---

\begin{code}
{-# LANGUAGE InstanceSigs #-}

module Chapter25 where

import Data.Bifunctor
import Test.Hspec

newtype Compose f g a = Compose
  { getCompose :: f (g a)
  } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  Compose fgab <*> Compose fga = Compose $ fmap (<*>) fgab <*> fga

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative m => (a -> m b) -> Compose f g a -> m (Compose f g b)
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

data Deux a b =
  Deux a
       b
  deriving (Eq, Show)

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

newtype Const a b =
  Const a
  deriving (Eq, Show)

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

data Drei a b c =
  Drei a
       b
       c
  deriving (Eq, Show)

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c =
  SuperDrei a
            b
  deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

newtype SemiDrei a b c =
  SemiDrei a
  deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d =
  Quadzzz a
          b
          c
          d
  deriving (Eq, Show)

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Bifunctor Or where
  bimap f _ (Fst a) = Fst (f a)
  bimap _ g (Snd b) = Snd (g b)

spec :: SpecWith ()
spec = do
  describe "Applicatives" $
    it "can be composed" $ do
      pure (+ 2) <*> pure 3 `shouldBe` Compose [Just 5]
      pure (+ 2) <*> pure 3 `shouldBe` Compose (Just [5])
      Compose [Just (+ 2), Nothing, Just (* 3)] <*>
        Compose [Just 4, Nothing] `shouldBe`
        Compose [Just 6, Nothing, Nothing, Nothing, Just 12, Nothing]
      Compose (Just [(+ 2), (* 3)]) <*>
        Compose (Just [4, 5]) `shouldBe` Compose (Just [6, 7, 12, 15])
  describe "Compose Instances" $ do
    it "foldable" $ do
      foldMap show (Compose [Just 1, Nothing, Just 2]) `shouldBe` "12"
      foldMap show (Compose (Just [1, 2, 3])) `shouldBe` "123"
      foldMap show (Compose Nothing :: Compose Maybe [] Int) `shouldBe` ""
    it "traversable" $ do
      sequenceA (Compose [Just [1], Nothing, Just [2]]) `shouldBe`
        [Compose [Just 1, Nothing, Just 2]]
      sequenceA (Compose (Just [Just 1, Just 2, Just 3])) `shouldBe`
        (Just $ Compose (Just [1, 2, 3]))
      sequenceA (Compose (Just [Just 1, Nothing, Just 2])) `shouldBe` Nothing
  describe "Bifunctor Instances" $ do
    it "Deux" $ do
      first (++ "world") (Deux "hello" 1) `shouldBe` Deux "helloworld" 1
      second (+ 5) (Deux "hello" 1) `shouldBe` Deux "hello" 6
      bimap (++ "world") (+ 5) (Deux "hello" 1) `shouldBe` Deux "helloworld" 6
    it "Const" $ do
      first (++ "world") (Const "hello") `shouldBe` Const "helloworld"
      second (+ 5) (Const "hello") `shouldBe` Const "hello"
      bimap (++ "world") (+ 5) (Const "hello") `shouldBe` Const "helloworld"
    it "Drei" $ do
      first (++ "world") (Drei (Just 1) "hello" 1) `shouldBe`
        Drei (Just 1) "helloworld" 1
      second (+ 5) (Drei (Just 1) "hello" 1) `shouldBe` Drei (Just 1) "hello" 6
      bimap (++ "world") (+ 5) (Drei (Just 1) "hello" 1) `shouldBe`
        Drei (Just 1) "helloworld" 6
    it "SuperDrei" $ do
      first (++ "world") (SuperDrei (Just 1) "hello") `shouldBe`
        SuperDrei (Just 1) "helloworld"
      second (+ 5) (SuperDrei (Just 1) "hello") `shouldBe`
        SuperDrei (Just 1) "hello"
      bimap (++ "world") (+ 5) (SuperDrei (Just 1) "hello") `shouldBe`
        SuperDrei (Just 1) "helloworld"
    it "SemiDrei" $ do
      first (++ "world") (SemiDrei (Just 1)) `shouldBe` SemiDrei (Just 1)
      second (+ 5) (SemiDrei (Just 1)) `shouldBe` SemiDrei (Just 1)
      bimap (++ "world") (+ 5) (SemiDrei (Just 1)) `shouldBe` SemiDrei (Just 1)
    it "Quadzzz" $ do
      first (++ "world") (Quadzzz "test" (Just 1) "hello" 1) `shouldBe`
        Quadzzz "test" (Just 1) "helloworld" 1
      second (+ 5) (Quadzzz "test" (Just 1) "hello" 1) `shouldBe`
        Quadzzz "test" (Just 1) "hello" 6
      bimap (++ "world") (+ 5) (Quadzzz "test" (Just 1) "hello" 1) `shouldBe`
        Quadzzz "test" (Just 1) "helloworld" 6
    it "Or" $ do
      first (++ "world") (Fst "hello") `shouldBe`
        (Fst "helloworld" :: Or String Int)
      first (++ "world") (Snd 1) `shouldBe` Snd 1
      second (+ 5) (Fst "hello") `shouldBe` Fst "hello"
      second (+ 5) (Snd 1) `shouldBe` (Snd 6 :: Or String Int)
      bimap (++ "world") (+ 5) (Fst "hello") `shouldBe` Fst "helloworld"
      bimap (++ "world") (+ 5) (Snd 1) `shouldBe` Snd 6
\end{code}
