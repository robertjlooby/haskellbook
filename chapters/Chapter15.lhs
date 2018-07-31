---
title: Chapter 15
---

---

\begin{code}
module Chapter15 where

import Data.Monoid
import Test.Hspec
import Test.QuickCheck hiding (Failure, Success)

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> only = only
  only <> Nada = only
  Only a <> Only b = Only $ a <> b

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

type S = String

type B = Bool

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = a <> (b <> c) == (a <> b) <> c

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a <> mempty) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mempty <> a) == a

newtype First' a = First'
  { getFirst' :: Optional a
  } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency $ [(1, return $ First' Nada), (3, return $ First' $ Only a)]

instance Semigroup (First' a) where
  first@(First' (Only a)) <> _ = first
  _ <> first@(First' (Only a)) = first
  _ <> _ = First' Nada

instance Monoid (First' a) where
  mempty = First' Nada

data Trivial =
  Trivial
  deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

data Identity a =
  Identity a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity $ a <> b

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty

data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  Four a b c d <> Four a' b' c' d' =
    Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty

data BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True

data BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False

data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Fst a, return $ Snd b]

instance Semigroup (Or a b) where
  Snd a <> _ = Snd a
  _ <> Snd a = Snd a
  _ <> a = a

newtype Combine a b = Combine
  { unCombine :: (a -> b)
  }

combineInc :: Combine Int (Sum Int)
combineInc = Combine $ \n -> Sum (n + 1)

combineSub :: Combine Int (Sum Int)
combineSub = Combine $ \n -> Sum (n - 1)

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ \a -> f a <> g a

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine $ \_ -> mempty

newtype Comp a = Comp
  { unComp :: (a -> a)
  }

compInc :: Comp Int
compInc = Comp $ \n -> n + 1

compSub :: Comp Int
compSub = Comp $ \n -> n - 1

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp $ f . g

instance Monoid (Comp a) where
  mempty = Comp id

data Validation a b
  = Failure a
  | Success b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Failure a, return $ Success b]

instance Semigroup a => Semigroup (Validation a b) where
  Success a <> _ = Success a
  _ <> Success a = Success a
  Failure a <> Failure b = Failure $ a <> b

newtype Mem s a = Mem
  { runMem :: s -> (a, s)
  }

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem f' =
    Mem $ \s ->
      let (a1, s1) = f s
          (a2, s2) = f' s1
       in (a1 <> a2, s2)

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)

testMem = Mem $ \s -> ("hi", s + 1)

spec :: SpecWith ()
spec = do
  describe "Optional monoid" $ do
    it "with a Sum" $
      (Only $ Sum 1) `mappend` (Only $ Sum 1) `shouldBe` (Only $ Sum (2 :: Int))
    it "with a Sum and Nada" $
      (Only $ Sum 1) `mappend` Nada `shouldBe` (Only $ Sum (1 :: Int))
    it "with a Nada and Sum" $
      Nada `mappend` (Only $ Sum 1) `shouldBe` (Only $ Sum (1 :: Int))
    it "with a Product" $
      (Only $ Product 4) `mappend` (Only $ Product 2) `shouldBe`
      (Only $ Product (8 :: Int))
    it "with a List" $
      (Only $ [1]) `mappend` (Only $ [1]) `shouldBe` (Only $ ([1, 1] :: [Int]))
    it "with a List and Nada" $
      (Only $ [1]) `mappend` Nada `shouldBe` (Only $ ([1] :: [Int]))
  describe "First'" $ do
    it "obeys associativity" $
      property (semigroupAssoc :: First' S -> First' S -> First' S -> B)
    it "obeys right identity" $ property (monoidRightIdentity :: First' S -> B)
    it "obeys left identity" $ property (monoidLeftIdentity :: First' S -> B)
    it "for an Only and Nada" $
      (First' $ Only [1]) `mappend` First' Nada `shouldBe`
      (First' $ Only ([1] :: [Int]))
    it "for a Nada and Only" $
      First' Nada `mappend` (First' $ Only [1]) `shouldBe`
      (First' $ Only ([1] :: [Int]))
    it "for both Nada" $
      First' Nada `mappend` First' Nada `shouldBe`
      (First' $ (Nada :: Optional [Int]))
    it "for both Only" $
      (First' $ Only [1]) `mappend` (First' $ Only [2]) `shouldBe`
      (First' $ Only ([1] :: [Int]))
  describe "Trivial" $ do
    it "obeys associativity" $
      property (semigroupAssoc :: Trivial -> Trivial -> Trivial -> B)
    it "obeys right identity" $ property (monoidRightIdentity :: Trivial -> B)
    it "obeys left identity" $ property (monoidLeftIdentity :: Trivial -> B)
  describe "Identity" $ do
    it "obeys associativity" $
      property (semigroupAssoc :: Identity S -> Identity S -> Identity S -> B)
    it "obeys right identity" $
      property (monoidRightIdentity :: Identity S -> B)
    it "obeys left identity" $ property (monoidLeftIdentity :: Identity S -> B)
  describe "Two" $ do
    it "obeys associativity" $
      property (semigroupAssoc :: Two S S -> Two S S -> Two S S -> B)
    it "obeys right identity" $ property (monoidRightIdentity :: Two S S -> B)
    it "obeys left identity" $ property (monoidLeftIdentity :: Two S S -> B)
  describe "Three" $ do
    it "obeys associativity" $
      property
        (semigroupAssoc :: Three S S S -> Three S S S -> Three S S S -> B)
    it "obeys right identity" $
      property (monoidRightIdentity :: Three S S S -> B)
    it "obeys left identity" $ property (monoidLeftIdentity :: Three S S S -> B)
  describe "Four" $ do
    it "obeys associativity" $
      property
        (semigroupAssoc :: Four S S S S -> Four S S S S -> Four S S S S -> B)
    it "obeys right identity" $
      property (monoidRightIdentity :: Four S S S S -> B)
    it "obeys left identity" $
      property (monoidLeftIdentity :: Four S S S S -> B)
  describe "BoolConj" $ do
    it "obeys associativity" $
      property (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> B)
    it "obeys right identity" $ property (monoidRightIdentity :: BoolConj -> B)
    it "obeys left identity" $ property (monoidLeftIdentity :: BoolConj -> B)
    it "for True/True" $ BoolConj True <> BoolConj True `shouldBe` BoolConj True
    it "for True/False" $
      BoolConj True <> BoolConj False `shouldBe` BoolConj False
    it "for False/True" $
      BoolConj False <> BoolConj True `shouldBe` BoolConj False
    it "for False/False" $
      BoolConj False <> BoolConj False `shouldBe` BoolConj False
    it "for right mempty" $ BoolConj True <> mempty `shouldBe` BoolConj True
    it "for left mempty" $ mempty <> BoolConj False `shouldBe` BoolConj False
  describe "BoolDisj" $ do
    it "obeys associativity" $
      property (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> B)
    it "obeys right identity" $ property (monoidRightIdentity :: BoolDisj -> B)
    it "obeys left identity" $ property (monoidLeftIdentity :: BoolDisj -> B)
    it "for True/True" $ BoolDisj True <> BoolDisj True `shouldBe` BoolDisj True
    it "for True/False" $
      BoolDisj True <> BoolDisj False `shouldBe` BoolDisj True
    it "for False/True" $
      BoolDisj False <> BoolDisj True `shouldBe` BoolDisj True
    it "for False/False" $
      BoolDisj False <> BoolDisj False `shouldBe` BoolDisj False
    it "for right mempty" $ BoolDisj True <> mempty `shouldBe` BoolDisj True
    it "for left mempty" $ mempty <> BoolDisj False `shouldBe` BoolDisj False
  describe "Or" $ do
    it "obeys associativity" $
      property (semigroupAssoc :: Or S S -> Or S S -> Or S S -> B)
    it "for Fst/Fst" $ Fst 1 <> Fst 2 `shouldBe` (Fst 2 :: Or Int Int)
    it "for Fst/Snd" $ Fst 1 <> Snd 2 `shouldBe` (Snd 2 :: Or Int Int)
    it "for Snd/Fst" $ Snd 1 <> Fst 2 `shouldBe` (Snd 1 :: Or Int Int)
    it "for Snd/Snd" $ Snd 1 <> Snd 2 `shouldBe` (Snd 1 :: Or Int Int)
  describe "Combine" $ do
    it "for inc <> sub with 0" $
      (unCombine (combineInc <> combineSub) $ 0) `shouldBe` Sum 0
    it "for inc <> sub with 1" $
      (unCombine (combineInc <> combineSub) $ 1) `shouldBe` Sum 2
    it "for inc <> inc with 1" $
      (unCombine (combineInc <> combineInc) $ 1) `shouldBe` Sum 4
    it "for sub <> inc with 1" $
      (unCombine (combineSub <> combineInc) $ 1) `shouldBe` Sum 2
    it "for inc <> mempty with 1" $
      (unCombine (combineInc <> mempty) $ 1) `shouldBe` Sum 2
  describe "Comp" $ do
    it "for inc <> sub with 0" $ (unComp (compInc <> compSub) $ 0) `shouldBe` 0
    it "for inc <> sub with 1" $ (unComp (compInc <> compSub) $ 1) `shouldBe` 1
    it "for inc <> inc with 1" $ (unComp (compInc <> compInc) $ 1) `shouldBe` 3
    it "for sub <> inc with 1" $ (unComp (compSub <> compInc) $ 1) `shouldBe` 1
    it "for inc <> mempty with 1" $
      (unComp (compInc <> mempty) $ 1) `shouldBe` 2
  describe "Validation" $ do
    it "obeys associativity" $
      property
        (semigroupAssoc :: Validation S S -> Validation S S -> Validation S S -> B)
    it "for Failure/Failure" $
      Failure "woot" <> Failure "blah" `shouldBe`
      (Failure "wootblah" :: Validation String Int)
    it "for Failure/Success" $
      Failure "woot" <> Success 2 `shouldBe`
      (Success 2 :: Validation String Int)
    it "for Success/Failure" $
      Success 1 <> Failure "woot" `shouldBe`
      (Success 1 :: Validation String Int)
    it "for Success/Success" $
      Success 1 <> Success 2 `shouldBe` (Success 1 :: Validation String Int)
  describe "Mem" $ do
    it "rmzero" $ runMem mempty 0 `shouldBe` ("", 0)
    it "testMem" $ runMem testMem 0 `shouldBe` ("hi", 1)
    it "rmleft" $ runMem (testMem <> mempty) 0 `shouldBe` ("hi", 1)
    it "rmright" $ runMem (mempty <> testMem) 0 `shouldBe` ("hi", 1)
\end{code}
