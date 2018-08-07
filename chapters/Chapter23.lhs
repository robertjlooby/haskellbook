---
title: Chapter 23
---

---

\begin{code}
{-# LANGUAGE InstanceSigs #-}

module Chapter23 where

import Control.Monad.Trans.State
  ( State
  , StateT(..)
  , execState
  , get
  , put
  , runState
  )
import Data.Functor.Identity
import System.Random
import Test.Hspec

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 (0, [])
  where
    go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go sum (count, rolls) gen
      | sum >= n = (count, reverse rolls)
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (count + 1, intToDie die : rolls) nextGen

newtype Moi s a = Moi
  { runMoi :: s -> (a, s)
  }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) =
    Moi $ \s ->
      let (a, s') = g s
       in (f a, s')

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  Moi sab <*> Moi sa =
    Moi $ \s ->
      let (a, s') = sa s
          (ab, s'') = sab s'
       in (ab a, s'')

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  Moi sa >>= aMsb =
    Moi $ \s ->
      let (a, s') = sa s
       in runMoi (aMsb a) s'

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo from to =
  execState (mapM_ addResult (enumFromThenTo to (to - 1) from)) []

fizzBuzzFromTo' :: Integer -> Integer -> [String]
fizzBuzzFromTo' from to = fizzBuzz <$> enumFromTo from to

get' :: State s s
get' = StateT $ \s -> Identity (s, s)

put' :: s -> State s ()
put' s = StateT $ \_ -> Identity ((), s)

exec :: State s a -> s -> s
exec (StateT sa) s = snd $ runIdentity $ sa s

eval :: State s a -> s -> a
eval (StateT sa) s = fst $ runIdentity $ sa s

modify :: (s -> s) -> State s ()
modify f = StateT $ \s -> Identity ((), f s)

spec :: SpecWith ()
spec = do
  describe "Roll Your Own" $ do
    it "rollsToGetN" $ do
      rollsToGetN 20 (mkStdGen 0) `shouldBe` 5
      rollsToGetN 17 (mkStdGen 0) `shouldBe` 4
      rollsToGetN 12 (mkStdGen 0) `shouldBe` 2
      rollsToGetN 11 (mkStdGen 0) `shouldBe` 2
    it "rollsCountLogged" $ do
      rollsCountLogged 20 (mkStdGen 0) `shouldBe`
        (5, [DieSix, DieSix, DieFour, DieOne, DieFive])
      rollsCountLogged 17 (mkStdGen 0) `shouldBe`
        (4, [DieSix, DieSix, DieFour, DieOne])
      rollsCountLogged 12 (mkStdGen 0) `shouldBe` (2, [DieSix, DieSix])
      rollsCountLogged 11 (mkStdGen 0) `shouldBe` (2, [DieSix, DieSix])
  describe "Moi" $ do
    it "has a functor instance" $ do
      runMoi (id <$> moi) 3 `shouldBe` -- identity law
        runMoi moi 3
      runMoi (f . g <$> moi) 3 `shouldBe` -- composition law
        runMoi ((fmap f . fmap g) moi) 3
      runMoi ((++ "hi") <$> moi) 3 `shouldBe` ("zzzhi", 9)
    it "has an applicative instance" $ do
      runMoi (pure "1") 5 `shouldBe` ("1", 5)
      runMoi (pure id <*> moi) 3 `shouldBe` -- identity law
        runMoi (pure id <*> moi) 3
      runMoi (pure (.) <*> moiA <*> moiB <*> moi) 3 `shouldBe` -- composition law
        runMoi (moiA <*> (moiB <*> moi)) 3
      runMoi (pure show <*> pure 4) 3 `shouldBe` -- homomorphism law
        runMoi (pure (show 4)) 3
      runMoi (moiA <*> pure "hi") 3 `shouldBe` -- interchange law
        runMoi (pure ($ "hi") <*> moiA) 3
      runMoi (moiA <*> moi) 3 `shouldBe` ("zzzwat", 18)
    it "has a monad instance" $ do
      runMoi (return "1") 5 `shouldBe` ("1", 5)
      runMoi (return "1" >>= mf) 5 `shouldBe` -- left identity law
        runMoi (mf "1") 5
      runMoi (moi >>= return) 5 `shouldBe` -- right identity law
        runMoi moi 5
      runMoi (moi >>= (\x -> mf x >>= mf)) 5 `shouldBe` -- associativity law
        runMoi ((moi >>= mf) >>= mf) 5
      runMoi (moi >>= mf) 5 `shouldBe` (replicate 75 'z', 75)
  describe "FizzBuzz Differently" $
    it "is the same as the implementation with reverse" $ do
      fizzBuzzFromTo 1 100 `shouldBe` reverse (fizzBuzzList [1 .. 100])
      fizzBuzzFromTo' 1 100 `shouldBe` reverse (fizzBuzzList [1 .. 100])
  describe "Chapter Exercises" $ do
    it "get'" $
      runState get' "curryIsAmaze" `shouldBe` ("curryIsAmaze", "curryIsAmaze")
    it "put'" $ runState (put' "blah") "woot" `shouldBe` ((), "blah")
    it "exec" $ do
      exec (put' "wilma") "daphne" `shouldBe` "wilma"
      exec get "scooby papu" `shouldBe` "scooby papu"
    it "eval" $ do
      eval (put' "wilma") "daphne" `shouldBe` ()
      eval get "scooby papu" `shouldBe` "scooby papu"
    it "eval" $ do
      runState (modify (+ 1)) 0 `shouldBe` ((), 1)
      runState (modify (+ 1) >> modify (+ 1)) 0 `shouldBe` ((), 2)
  where
    moi = Moi $ \s -> (replicate s 'z', s * 3)
    moiA = Moi $ \s -> ((++ "wat"), s * 2)
    moiB = Moi $ \s -> ((++ "wut"), s * 4)
    f = (++ "there")
    g = (++ "you")
    mf a = Moi $ \s -> (concat $ replicate s a, s * 5)
\end{code}
