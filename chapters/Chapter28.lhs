---
title: Chapter 28
---

---

\begin{code}
module Chapter28 where

import Criterion.Main
import Data.Bifunctor (first)
import Data.Bool (bool)
import qualified Data.Sequence as S
import System.Random
import Test.Hspec

infixl 9 !?

_ !? n
  | n < 0 = Nothing
[] !? _ = Nothing
(x:_) !? 0 = Just x
(_:xs) !? n = xs !? (n - 1)

infixl 9 !??

{-# INLINABLE (!??) #-}
xs !?? n
  | n < 0 = Nothing
  | otherwise =
    foldr
      (\x r k ->
         case k of
           0 -> Just x
           _ -> r (k - 1))
      (const Nothing)
      xs
      n

infixl 9 !???

{-# INLINABLE (!???) #-}
(!???) :: [a] -> Int -> Maybe a
xs !??? n
  | n < 0 = Nothing
  | otherwise =
    foldr
      (\x r k ->
         case k of
           0 -> Just x
           _ -> r (k - 1))
      (const Nothing)
      xs
      n

myList :: [Int]
myList = [1 .. 9999]

main' :: IO ()
main' =
  defaultMain
    [ bench "index list 9999" $ whnf (myList !!) 9990
    , bench "index list maybe index 9999" $ whnf (myList !?) 9990
    , bench "index list maybe index with foldr 9999" $ whnf (myList !??) 9990
    , bench "index list maybe index with foldr and type signature 9999" $
      whnf (myList !???) 9990
    ]

newtype DList a = DL
  { unDL :: [a] -> [a]
  }

{-# INLINE empty #-}
empty :: DList a
empty = DL id

{-# INLINE singleton #-}
singleton :: a -> DList a
singleton a = DL (a :)

{-# INLINE toList #-}
toList :: DList a -> [a]
toList = flip unDL []

{-# INLINE cons #-}
infixr `cons`

cons :: a -> DList a -> DList a
cons x xs = DL ((x :) . unDL xs)

{-# INLINE snoc #-}
infixl `snoc`

snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x :))

{-# INLINE append #-}
append :: DList a -> DList a -> DList a
append xs xs' = DL (unDL xs . unDL xs')

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where
    go 0 xs = xs
    go n xs = go (n - 1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where
    go 0 xs = xs
    go n xs = go (n - 1) (singleton n `append` xs)

main'' :: IO ()
main'' =
  defaultMain
    [ bench "concat list" $ whnf schlemiel 123456
    , bench "concat dlist" $ whnf constructDlist 123456
    ]

data Queue a = Queue
  { enqueue :: [a]
  , dequeue :: [a]
  } deriving (Eq, Show)

emptyQueue :: Queue a
emptyQueue = Queue [] []

push :: a -> Queue a -> Queue a
push a (Queue en de) = Queue (a : en) de

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue en (a:as)) = Just (a, Queue en as)
pop (Queue en []) = pop (Queue [] (reverse en))

newtype ListQueue a = ListQueue
  { queue :: [a]
  } deriving (Eq, Show)

lemptyQueue :: ListQueue a
lemptyQueue = ListQueue []

lpush :: a -> ListQueue a -> ListQueue a
lpush a (ListQueue q) = ListQueue (a : q)

lpop :: ListQueue a -> Maybe (a, ListQueue a)
lpop (ListQueue []) = Nothing
lpop (ListQueue l) = Just (a, ListQueue as)
  where
    (as, a) = getLast l
    getLast [a] = ([], a)
    getLast (a:as) = first (a :) (getLast as)

newtype SequenceQueue a = SequenceQueue
  { squeue :: S.Seq a
  } deriving (Eq, Show)

semptyQueue :: SequenceQueue a
semptyQueue = SequenceQueue S.empty

spush :: a -> SequenceQueue a -> SequenceQueue a
spush a (SequenceQueue q) = SequenceQueue (a S.<| q)

spop :: SequenceQueue a -> Maybe (a, SequenceQueue a)
spop (SequenceQueue q) =
  case S.viewr q of
    S.EmptyR -> Nothing
    seq S.:> a -> Just (a, SequenceQueue seq)

main''' :: IO ()
main''' =
  defaultMain
    [ bench "queue" $ nf (queueBenchmark emptyQueue push pop) 123456
    , bench "list queue" $ nf (queueBenchmark lemptyQueue lpush lpop) 123456
    , bench "sequence queue" $ nf (queueBenchmark semptyQueue spush spop) 123456
    ]

data QueueAction
  = Push
  | Pop

instance Random QueueAction where
  random g = first (bool Push Pop) (random g)
  randomR = const random

-- perform a bunch of random push/pop operations and return the list
-- of popped values at the end
queueBenchmark ::
     queue
  -> (Int -> queue -> queue)
  -> (queue -> Maybe (Int, queue))
  -> Int
  -> [Int]
queueBenchmark initialQueue pushIt popIt count =
  fst $ foldr takeStep ([], initialQueue) steps
  where
    pushNPops = take count $ randoms (mkStdGen count)
    steps = zip pushNPops [1 ..]
    takeStep (Push, n) (ints, q) = (ints, pushIt n q)
    takeStep (Pop, _) (ints, q) =
      case popIt q of
        Nothing -> (ints, q)
        Just (n, q') -> (n : ints, q')

spec :: SpecWith ()
spec = do
  describe "DList" $ do
    it "empty is the empty list" $ toList empty `shouldBe` ([] :: [Int])
    it "singleton is a one element list" $ do
      toList (singleton 1) `shouldBe` [1]
      toList (singleton 'a') `shouldBe` ['a']
    it "cons prepends an element" $ do
      toList (3 `cons` 2 `cons` singleton 1) `shouldBe` [3, 2, 1]
      toList ('c' `cons` 'b' `cons` singleton 'a') `shouldBe` ['c', 'b', 'a']
    it "snoc appends an element" $ do
      toList (singleton 1 `snoc` 2 `snoc` 3) `shouldBe` [1, 2, 3]
      toList (singleton 'a' `snoc` 'b' `snoc` 'c') `shouldBe` ['a', 'b', 'c']
    it "append appends a DList" $ do
      toList ((singleton 1 `snoc` 2) `append` (singleton 3 `snoc` 4)) `shouldBe`
        [1, 2, 3, 4]
      toList ((singleton 'a' `snoc` 'b') `append` (singleton 'c' `snoc` 'd')) `shouldBe`
        ['a', 'b', 'c', 'd']
    it "empty is an identity" $ do
      toList
        ((singleton 1 `snoc` 2) `append` empty `append` (singleton 3 `snoc` 4)) `shouldBe`
        [1, 2, 3, 4]
      toList
        ((singleton 'a' `snoc` 'b') `append` empty `append`
         (singleton 'c' `snoc` 'd')) `shouldBe`
        ['a', 'b', 'c', 'd']
  describe "Queue" $ do
    it "push adds items to the queue" $ do
      push 'a' emptyQueue `shouldBe` Queue ['a'] []
      push 'b' (push 'a' emptyQueue) `shouldBe` Queue ['b', 'a'] []
    it "pop removes items from the dequeue" $ do
      pop (Queue [] ['a']) `shouldBe` Just ('a', emptyQueue)
      pop (Queue [] ['b', 'a']) `shouldBe` Just ('b', Queue [] ['a'])
    it "can't pop from an empty queue" $
      pop (emptyQueue :: Queue Char) `shouldBe` Nothing
    it "can push and then pop" $
      (fmap fst . pop . push 'b' . push 'a' $ emptyQueue) `shouldBe` Just 'a'
  describe "ListQueue" $ do
    it "push adds items to the queue" $ do
      lpush 'a' lemptyQueue `shouldBe` ListQueue ['a']
      lpush 'b' (lpush 'a' lemptyQueue) `shouldBe` ListQueue ['b', 'a']
    it "pop removes items from the dequeue" $ do
      lpop (ListQueue ['a']) `shouldBe` Just ('a', lemptyQueue)
      lpop (ListQueue ['b', 'a']) `shouldBe` Just ('a', ListQueue ['b'])
    it "can't pop from an empty queue" $
      lpop (lemptyQueue :: ListQueue Char) `shouldBe` Nothing
    it "can push and then pop" $
      (fmap fst . lpop . lpush 'b' . lpush 'a' $ lemptyQueue) `shouldBe`
      Just 'a'
  describe "SequenceQueue" $ do
    it "push adds items to the queue" $ do
      spush 'a' semptyQueue `shouldBe` SequenceQueue (S.fromList ['a'])
      spush 'b' (spush 'a' semptyQueue) `shouldBe`
        SequenceQueue (S.fromList ['b', 'a'])
    it "pop removes items from the dequeue" $ do
      spop (SequenceQueue (S.fromList ['a'])) `shouldBe` Just ('a', semptyQueue)
      spop (SequenceQueue (S.fromList ['b', 'a'])) `shouldBe`
        Just ('a', SequenceQueue (S.fromList ['b']))
    it "can't pop from an empty queue" $
      spop (semptyQueue :: SequenceQueue Char) `shouldBe` Nothing
    it "can push and then pop" $
      (fmap fst . spop . spush 'b' . spush 'a' $ semptyQueue) `shouldBe`
      Just 'a'
\end{code}
