---
title: Chapter 14
---

---

\begin{code}
module Chapter14 where

import Chapter09 (cypher, unCypher)
import Chapter11 (capitalizeWord)
import Data.List (sort)
import qualified Data.Map as M
import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

type Morse = String

letterToMorse :: M.Map Char Morse
letterToMorse =
  M.fromList
    [ ('a', ".-")
    , ('b', "-...")
    , ('c', "-.-.")
    , ('d', "-..")
    , ('e', ".")
    , ('f', "..-.")
    , ('g', "--.")
    , ('h', "....")
    , ('i', "..")
    , ('j', ".---")
    , ('k', "-.-")
    , ('l', ".-..")
    , ('m', "--")
    , ('n', "-.")
    , ('o', "---")
    , ('p', ".--.")
    , ('q', "--.-")
    , ('r', ".-.")
    , ('s', "...")
    , ('t', "-")
    , ('u', "..-")
    , ('v', "...-")
    , ('w', ".--")
    , ('x', "-..-")
    , ('y', "-.--")
    , ('z', "--..")
    , ('1', ".----")
    , ('2', "..---")
    , ('3', "...--")
    , ('4', "....-")
    , ('5', ".....")
    , ('6', "-....")
    , ('7', "--...")
    , ('8', "---..")
    , ('9', "----.")
    , ('0', "-----")
    ]

morseToLetter :: M.Map Morse Char
morseToLetter = M.foldrWithKey (flip M.insert) M.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c = M.lookup c letterToMorse

stringToMorse :: String -> Maybe [Morse]
stringToMorse s = sequence $ fmap charToMorse s

morseToChar :: Morse -> Maybe Char
morseToChar m = M.lookup m morseToLetter

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

half :: Double -> Double
half x = x / 2

halfIdentity = (* 2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y = x + y == y + x

multiplyAssociative :: Int -> Int -> Int -> Bool
multiplyAssociative x y z = x * (y * z) == (x * y) * z

multiplyCommutative :: Int -> Int -> Bool
multiplyCommutative x y = x * y == y * x

quotRemProp :: Int -> NonZero Int -> Bool
quotRemProp x (NonZero y) = (quot x y) * y + (rem x y) == x

divModProp :: Int -> NonZero Int -> Bool
divModProp x (NonZero y) = (div x y) * y + (mod x y) == x

subtractionAssociative :: Int -> Int -> Int -> Bool
subtractionAssociative x y z = x - (y - z) == (x - y) - z

subtractionCommutative :: Int -> Int -> Bool
subtractionCommutative x y = x - y == y - x

square :: Num a => a -> a
square x = x * x

squareIdentity :: Double -> Bool
squareIdentity x = (square . sqrt) x == x

twice :: (a -> a) -> (a -> a)
twice f = f . f

fourTimes :: (a -> a) -> (a -> a)
fourTimes = twice . twice

spec :: SpecWith ()
spec = do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ 2 + 2 `shouldBe` 4
    it "x + 1 is always greater than x" $ property $ \x -> x + 1 > (x :: Int)
  describe "dividedBy" $ do
    it "15 divided by 3 is 5" $ dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ dividedBy 22 5 `shouldBe` (4, 2)
  describe "morse" $ do
    it "should decode what is encoded" $
      forAll charGen $ \c -> ((charToMorse c) >>= morseToChar) == Just c
  describe "half" $ do
    it "doubled half is the original" $ property $ \n -> halfIdentity n == n
  describe "sorted list" $ do
    it "is ordered" $ property $ \l -> listOrdered (sort (l :: [Int]))
  describe "addition" $ do
    it "is associative" $ property plusAssociative
    it "is commutative" $ property plusCommutative
  describe "multiplication" $ do
    it "is associative" $ property multiplyAssociative
    it "is commutative" $ property multiplyCommutative
  describe "division" $ do
    it "with quot & rem" $ property quotRemProp
    it "with div & mod" $ property divModProp
  describe "subtraction" $ do
    it "is associative" $ expectFailure subtractionAssociative
    it "is not commutative" $ expectFailure subtractionCommutative
  describe "subtraction" $ do
    it "is associative" $ expectFailure subtractionAssociative
  describe "reverse" $ do
    it "is identity when applied twice" $
      property $ \l -> (reverse . reverse) l == id (l :: [Int])
  describe "combinators" $ do
    it "dollar is function application" $ do
      property $ \f a -> (applyFun f $ a) == (applyFun f (a :: String) :: Int)
    it "dot is function composition" $ do
      property $ \f g a ->
        ((applyFun f . applyFun g) a) ==
        (applyFun f (applyFun g (a :: String) :: Int) :: String)
  describe "list folding" $ do
    it "foldr : is ++" $
      property $ \s1 s2 -> foldr (:) s2 s1 == s1 ++ (s2 :: String)
    it "foldr ++ [] is concat" $
      property $ \s -> foldr (++) [] s == concat (s :: [String])
  describe "take" $ do
    it "cannot take more elements than are in the list" $
      expectFailure $ \n xs -> length (take n (xs :: [Int])) == n
  describe "read" $ do
    it "is the inverse of show" $ property $ \n -> read (show (n :: Int)) == n
  describe "square" $ do
    it "cannot always be undone with sqrt" $ expectFailure squareIdentity
  describe "idempotence" $ do
    it "of capitalizeWord" $
      property $ \s ->
        capitalizeWord s == twice capitalizeWord s &&
        capitalizeWord s == fourTimes capitalizeWord s
    it "of sort" $
      property $ \l ->
        sort l == twice sort l && sort l == fourTimes sort (l :: [Int])
  describe "cipher" $ do
    it "decyphers cyphered value" $
      property $ \n s -> unCypher n (cypher n s) == s
\end{code}
