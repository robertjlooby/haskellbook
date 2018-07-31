---
title: Chapter 6
---

---

\begin{code}
module Chapter06 where

import Test.Hspec

data TisAnInteger =
  TisAn Integer
  deriving (Show)

instance Eq TisAnInteger where
  TisAn a == TisAn b = a == b

data TwoIntegers =
  Two Integer
      Integer
  deriving (Show)

instance Eq TwoIntegers where
  Two a b == Two a' b' = a == a' && b == b'

data StringOrInt
  = TisAnInt Int
  | TisAString String
  deriving (Show)

instance Eq StringOrInt where
  TisAnInt a == TisAnInt b = a == b
  TisAString a == TisAString b = a == b
  _ == _ = False

data Pair a =
  Pair a
       a
  deriving (Show)

instance Eq a => Eq (Pair a) where
  Pair a b == Pair a' b' = a == a' && b == b'

data Tuple a b =
  Tuple a
        b
  deriving (Show)

instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple a b == Tuple a' b' = a == a' && b == b'

data Which a
  = ThisOne a
  | ThatOne a
  deriving (Show)

instance Eq a => Eq (Which a) where
  ThisOne a == ThisOne a' = a == a'
  ThatOne a == ThatOne a' = a == a'
  _ == _ = False

data EitherOr a b
  = Hello a
  | Goodbye b
  deriving (Show)

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello a == Hello a' = a == a'
  Goodbye b == Goodbye b' = b == b'
  _ == _ = False

spec :: SpecWith ()
spec = do
  describe "TisAnInteger" $ do
    it "tests equality" $ TisAn 8 `shouldBe` TisAn 8
    it "tests inequality" $ TisAn 8 `shouldNotBe` TisAn 11
  describe "TwoIntegers" $ do
    it "tests equality" $ Two 1 2 `shouldBe` Two 1 2
    it "tests inequality" $ do
      Two 1 2 `shouldNotBe` Two 1 1
      Two 1 2 `shouldNotBe` Two 2 2
      Two 1 2 `shouldNotBe` Two 2 1
  describe "StringOrInt" $ do
    it "tests equality" $ do
      TisAnInt 1 `shouldBe` TisAnInt 1
      TisAString "test" `shouldBe` TisAString "test"
    it "tests inequality" $ do
      TisAnInt 1 `shouldNotBe` TisAnInt 2
      TisAString "test" `shouldNotBe` TisAString "other test"
      TisAnInt 1 `shouldNotBe` TisAString "1"
  describe "Pair" $ do
    it "tests equality" $ do
      Pair 1 2 `shouldBe` Pair 1 2
      Pair "a" "b" `shouldBe` Pair "a" "b"
    it "tests inequality" $ do
      Pair 1 2 `shouldNotBe` Pair 1 1
      Pair 1 2 `shouldNotBe` Pair 2 2
      Pair 1 2 `shouldNotBe` Pair 2 1
      Pair "a" "b" `shouldNotBe` Pair "a" "a"
      Pair "a" "b" `shouldNotBe` Pair "b" "b"
      Pair "a" "b" `shouldNotBe` Pair "b" "a"
  describe "Tuple" $ do
    it "tests equality" $ do Tuple 1 "a" `shouldBe` Tuple 1 "a"
    it "tests inequality" $ do
      Tuple 1 "a" `shouldNotBe` Tuple 1 "b"
      Tuple 1 "a" `shouldNotBe` Tuple 2 "a"
      Tuple 1 "a" `shouldNotBe` Tuple 2 "b"
  describe "Which" $ do
    it "tests equality" $ do
      ThisOne 1 `shouldBe` ThisOne 1
      ThatOne "a" `shouldBe` ThatOne "a"
    it "tests inequality" $ do
      ThisOne 1 `shouldNotBe` ThisOne 2
      ThatOne "a" `shouldNotBe` ThatOne "b"
      ThisOne 1 `shouldNotBe` ThatOne 2
  describe "EitherOr" $ do
    it "tests equality" $ do
      Hello 1 `shouldBe` (Hello 1 :: EitherOr Integer String)
      Goodbye "a" `shouldBe` (Goodbye "a" :: EitherOr Integer String)
    it "tests inequality" $ do
      Hello 1 `shouldNotBe` (Hello 2 :: EitherOr Integer String)
      Goodbye "a" `shouldNotBe` (Goodbye "b" :: EitherOr Integer String)
      Hello 1 `shouldNotBe` (Goodbye 2 :: EitherOr Integer Integer)
\end{code}
