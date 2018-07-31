---
title: Chapter 16
---

---

\begin{code}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter16 where

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = fmap (g . f) x == (fmap g . fmap f $ x)

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

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

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

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

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b =
  Four' a
        a
        a
        b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return $ Four' a a' a'' b

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = do
    a <- arbitrary
    oneof [return LolNope, return $ Yeppers a]

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers $ f a

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a, return $ Second b]

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

data Quant a b
  = Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return Finance, return $ Desk a, return $ Bloor b]

instance (Eq a, Eq b) => EqProp (Quant a b) where
  (=-=) = eq

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b

data K a b =
  K a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (K a b) where
  arbitrary = do
    a <- arbitrary
    return $ K a

instance Eq a => EqProp (K a b) where
  (=-=) = eq

instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Arbitrary b => Arbitrary (Flip K a b) where
  arbitrary = do
    b <- arbitrary
    return $ Flip (K b)

instance (Eq b, Eq (K a b)) => EqProp (Flip K a b) where
  (=-=) = eq

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b))

data EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance Arbitrary b => Arbitrary (EvilGoateeConst a b) where
  arbitrary = do
    b <- arbitrary
    return $ GoatyConst b

instance Eq b => EqProp (EvilGoateeConst a b) where
  (=-=) = eq

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance Arbitrary (f a) => Arbitrary (LiftItOut f a) where
  arbitrary = do
    fa <- arbitrary
    return $ LiftItOut fa

instance Eq (f a) => EqProp (LiftItOut f a) where
  (=-=) = eq

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut $ f <$> fa

data Parappa f g a =
  DaWrappa (f a)
           (g a)
  deriving (Eq, Show)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Parappa f g a) where
  arbitrary = do
    fa <- arbitrary
    ga <- arbitrary
    return $ DaWrappa fa ga

instance (Eq (f a), Eq (g a)) => EqProp (Parappa f g a) where
  (=-=) = eq

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (f <$> fa) (f <$> ga)

data IgnoreOne f g a b =
  IgnoringSometing (f a)
                   (g b)
  deriving (Eq, Show)

instance (Arbitrary (f a), Arbitrary (g b)) =>
         Arbitrary (IgnoreOne f g a b) where
  arbitrary = do
    fa <- arbitrary
    gb <- arbitrary
    return $ IgnoringSometing fa gb

instance (Eq (f a), Eq (g b)) => EqProp (IgnoreOne f g a b) where
  (=-=) = eq

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSometing fa gb) = IgnoringSometing fa (f <$> gb)

data Notorious g o a t =
  Notorious (g o)
            (g a)
            (g t)
  deriving (Eq, Show)

instance (Arbitrary (g o), Arbitrary (g a), Arbitrary (g t)) =>
         Arbitrary (Notorious g o a t) where
  arbitrary = do
    go <- arbitrary
    ga <- arbitrary
    gt <- arbitrary
    return $ Notorious go ga gt

instance (Eq (g o), Eq (g a), Eq (g t)) => EqProp (Notorious g o a t) where
  (=-=) = eq

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (f <$> gt)

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

fromBaseList :: [a] -> List a
fromBaseList [] = Nil
fromBaseList (a:as) = Cons a (fromBaseList as)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    as <- arbitrary
    return $ fromBaseList as

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (GoatLord a) where
  arbitrary = do
    a <- arbitrary
    goatLord1 <- arbitrary
    goatLord2 <- arbitrary
    goatLord3 <- arbitrary
    frequency
      [ (5, return NoGoat)
      , (5, return $ OneGoat a)
      , (1, return $ MoreGoats goatLord1 goatLord2 goatLord3)
      ]

instance Eq a => EqProp (GoatLord a) where
  (=-=) = eq

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats gl1 gl2 gl3) = MoreGoats (f <$> gl1) (f <$> gl2) (f <$> gl3)

data TalkToMe a
  = Halt
  | Print String
          a
  | Read (String -> a)

instance Show a => Show (TalkToMe a) where
  show Halt = "Halt"
  show (Print s a) = "Print " ++ s ++ " " ++ show a
  show (Read f) = "Read " ++ show (f "TEST")

instance Eq a => Eq (TalkToMe a) where
  Halt == Halt = True
  Print s a == Print s' a' = s == s' && a == a'
  _ == _ = False

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read r) = Read (f . r)

spec :: SpecWith ()
spec = do
  describe "Identity" $ do
    it "functor obeys identity" $
      property (functorIdentity :: Identity Int -> Bool)
    it "functor obeys composition" $
      property
        (functorCompose :: Identity Int -> (Fun Int Int) -> (Fun Int Int) -> Bool)
  describe "Pair" $ do
    it "functor obeys identity" $ property (functorIdentity :: Pair Int -> Bool)
    it "functor obeys composition" $
      property
        (functorCompose :: Pair Int -> (Fun Int Int) -> (Fun Int Int) -> Bool)
  describe "Two" $ do
    it "functor obeys identity" $
      property (functorIdentity :: Two String Int -> Bool)
    it "functor obeys composition" $
      property
        (functorCompose :: Two String Int -> (Fun Int Int) -> (Fun Int Int) -> Bool)
  describe "Three" $ do
    it "functor obeys identity" $
      property (functorIdentity :: Three String String Int -> Bool)
    it "functor obeys composition" $
      property
        (functorCompose :: Three String String Int -> (Fun Int Int) -> (Fun Int Int) -> Bool)
  describe "Three'" $ do
    it "functor obeys identity" $
      property (functorIdentity :: Three' String Int -> Bool)
    it "functor obeys composition" $
      property
        (functorCompose :: Three' String Int -> (Fun Int Int) -> (Fun Int Int) -> Bool)
  describe "Four" $ do
    it "functor obeys identity" $
      property (functorIdentity :: Four String String String Int -> Bool)
    it "functor obeys composition" $
      property
        (functorCompose :: Four String String String Int -> (Fun Int Int) -> (Fun Int Int) -> Bool)
  describe "Four'" $ do
    it "functor obeys identity" $
      property (functorIdentity :: Four' String Int -> Bool)
    it "functor obeys composition" $
      property
        (functorCompose :: Four' String Int -> (Fun Int Int) -> (Fun Int Int) -> Bool)
  describe "Possibly" $ do
    it "functor obeys identity" $
      property (functorIdentity :: Possibly Int -> Bool)
    it "functor obeys composition" $
      property
        (functorCompose :: Possibly Int -> (Fun Int Int) -> (Fun Int Int) -> Bool)
  describe "Sum" $ do
    it "functor obeys identity" $
      property (functorIdentity :: Sum String Int -> Bool)
    it "functor obeys composition" $
      property
        (functorCompose :: Sum String Int -> (Fun Int Int) -> (Fun Int Int) -> Bool)
  describe "Quant" $ do
    testBatch $ functor (undefined :: Quant String (Int, Int, Int))
  describe "K" $ do testBatch $ functor (undefined :: K String (Int, Int, Int))
  describe "Flip K" $ do
    testBatch $ functor (undefined :: Flip K String (Int, Int, Int))
  describe "EvilGoateeConst" $ do
    testBatch $ functor (undefined :: EvilGoateeConst String (Int, Int, Int))
  describe "LiftItOut" $ do
    testBatch $ functor (undefined :: LiftItOut Maybe (Int, Int, Int))
  describe "Parappa" $ do
    testBatch $ functor (undefined :: Parappa [] Maybe (Int, Int, Int))
  describe "IgnoreOne" $ do
    testBatch $ functor (undefined :: IgnoreOne [] Maybe String (Int, Int, Int))
  describe "Notorious" $ do
    testBatch $
      functor (undefined :: Notorious [] String String (Int, Int, Int))
  describe "List" $ do testBatch $ functor (undefined :: List (Int, Int, Int))
  describe "GoatLord" $ do
    testBatch $ functor (undefined :: GoatLord (Int, Int, Int))
  describe "TalkToMe" $ do
    it "fmap for Halt" $ (+ 1) <$> Halt `shouldBe` (Halt :: TalkToMe Int)
    it "fmap for Print" $
      (+ 1) <$> (Print "str" 1) `shouldBe` (Print "str" 2 :: TalkToMe Int)
    it "fmap for Read" $
      case (+ 1) <$> (Read read) of
        Read f -> f "1" `shouldBe` (2 :: Int)
        _ -> expectationFailure "wrong type of TalkToMe"
\end{code}
