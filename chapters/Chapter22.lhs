---
title: Chapter 22
---

---

\begin{code}
{-# LANGUAGE InstanceSigs #-}

module Chapter22 where

import Control.Applicative
import Data.Char
import Data.Maybe
import Data.Monoid
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . reverse

fmapped :: String -> String
fmapped = cap <$> reverse

tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev

tupled' :: String -> (String, String)
tupled' = do
  capped <- cap
  reversed <- rev
  return (capped, reversed)

tupled'' :: String -> (String, String)
tupled'' = cap >>= (\capped -> rev >>= (\reversed -> return (capped, reversed)))

newtype Reader r a = Reader
  { runReader :: r -> a
  }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  Reader rab <*> Reader ra = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  Reader ra >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

ask :: Reader a a
ask = Reader id

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

rob :: Person
rob = Person (HumanName "Rob") (DogName "Chewie") (Address "Chicago")

getDogRM :: Person -> Dog
getDogRM = runReader reader
  where
    reader =
      Reader dogName >>= (\dn -> Reader address >>= \adr -> return $ Dog dn adr)

getDogRM' :: Person -> Dog
getDogRM' = runReader reader
  where
    reader = do
      dn <- Reader dogName
      adr <- Reader address
      return $ Dog dn adr

x = [1, 2, 3]

y = [4, 5, 6]

z = [7, 8, 9]

xs = lookup 3 $ zip x y

ys = lookup 6 $ zip y z

zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 = liftA2 (,) xs ys

x2 = liftA2 (,) ys zs

x3 = liftA2 (,) z' z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (> 3) (< 8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(> 3), (< 8), even]

s' = summed <$> ((,) <$> xs <*> ys)

spec :: SpecWith ()
spec = do
  describe "cap and rev" $ do
    it "composed" $ composed "Julie" `shouldBe` "EILUJ"
    it "fmapped" $ fmapped "Julie" `shouldBe` "EILUJ"
    it "tupled" $ tupled "Julie" `shouldBe` ("JULIE", "eiluJ")
    it "tupled'" $ tupled' "Julie" `shouldBe` ("JULIE", "eiluJ")
    it "tupled''" $ tupled'' "Julie" `shouldBe` ("JULIE", "eiluJ")
  describe "ask" $
    it "is the identity reader" $ do
      runReader ask "Hi" `shouldBe` "Hi"
      runReader ask 1 `shouldBe` 1
  describe "reading comprehension" $ do
    it "myLiftA2" $ do
      myLiftA2 (+) [1, 2] [3, 4] `shouldBe` liftA2 (+) [1, 2] [3, 4]
      myLiftA2 (++) ["hello"] ["world"] `shouldBe`
        liftA2 (++) ["hello"] ["world"]
    it "asks" $ do
      runReader (asks (++ " there")) "Hi" `shouldBe` "Hi there"
      runReader (asks (* 3)) 4 `shouldBe` 12
    it "the Reader applicative" $ do
      runReader ((+ 1) <$> Reader (* 2)) 3 `shouldBe` 7
      runReader (pure 1) "hi" `shouldBe` 1
      runReader (Reader (++) <*> Reader (++ "World")) "Hello" `shouldBe`
        "HelloHelloWorld"
  describe "reading comprehension" $ do
    it "the Reader monad" $ do
      runReader (return 1) "hi" `shouldBe` 1
      runReader (Reader (++ "World") >>= (\a -> Reader (++ a))) "Hello" `shouldBe`
        "HelloHelloWorld"
    it "getDogRM using monad" $ do
      getDogRM rob `shouldBe` Dog (DogName "Chewie") (Address "Chicago")
      getDogRM' rob `shouldBe` Dog (DogName "Chewie") (Address "Chicago")
  describe "chapter exercises" $ do
    it "the example data" $ do
      xs `shouldBe` Just 6
      ys `shouldBe` Just 9
      zs `shouldBe` Nothing
    it "the example data with applicatives" $ do
      x1 `shouldBe` Just (6, 9)
      x2 `shouldBe` Nothing
      x3 3 `shouldBe` (Just 9, Just 9)
    it "helper functions" $ do
      summed (1, 3) `shouldBe` 4
      summed <$> ((,) <$> xs <*> ys) `shouldBe` Just 15
      summed <$> ((,) <$> xs <*> zs) `shouldBe` Nothing
      bolt 3 `shouldBe` False
      bolt 5 `shouldBe` True
      bolt 8 `shouldBe` False
      bolt <$> z `shouldBe` [True, False, False]
      foldMap All (sequA 5) `shouldBe` All False
      foldMap All (sequA 6) `shouldBe` All True
      sequA <$> s' `shouldBe` Just [True, False, False]
      bolt <$> ys `shouldBe` Just False
\end{code}
