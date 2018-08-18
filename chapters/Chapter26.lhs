---
title: Chapter 26
---

---

\begin{code}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter26 where

import Control.Monad (guard)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.Functor.Identity
import Data.IORef
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Web.Scotty.Trans

newtype EitherT e m a = EitherT
  { runEitherT :: m (Either e a)
  }

deriving instance Eq (m (Either e a)) => Eq (EitherT e m a)

deriving instance Show (m (Either e a)) => Show (EitherT e m a)

instance Arbitrary (m (Either e a)) => Arbitrary (EitherT e m a) where
  arbitrary = EitherT <$> arbitrary

instance Eq (m (Either e a)) => EqProp (EitherT e m a) where
  (=-=) = eq

instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  EitherT fab <*> EitherT meea = EitherT $ fmap (<*>) fab <*> meea

instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  EitherT mea >>= f =
    EitherT $ do
      ea <- mea
      case ea of
        Left e -> return $ Left e
        Right a -> runEitherT $ f a

instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift = EitherT . fmap Right

instance MonadIO m => MonadIO (EitherT e m) where
  liftIO :: IO a -> EitherT e m a
  liftIO = lift . liftIO

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ swapEither <$> mea
  where
    swapEither (Left e) = Right e
    swapEither (Right a) = Left a

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mab) = do
  ab <- mab
  case ab of
    Left a -> f a
    Right b -> g b

newtype ReaderT r m a = ReaderT
  { runReaderT :: r -> m a
  }

instance (Monoid r, Show r, Show (m a)) => Show (ReaderT r m a) where
  show (ReaderT rma) =
    "ReaderT called with '" ++ show r ++ "' is '" ++ show (rma r) ++ "'"
    where
      r = mempty

instance (Arbitrary (m a), CoArbitrary r) => Arbitrary (ReaderT r m a) where
  arbitrary = ReaderT <$> arbitrary

instance (Arbitrary r, EqProp (m a), Show r) => EqProp (ReaderT r m a) where
  ReaderT rma =-= ReaderT rma' = rma =-= rma'

instance Functor m => Functor (ReaderT r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  ReaderT fmab <*> ReaderT rma = ReaderT $ fmap (<*>) fmab <*> rma

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  ReaderT rma >>= aRrmb =
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (aRrmb a) r

instance MonadTrans (ReaderT r) where
  lift :: Monad m => m a -> ReaderT r m a
  lift = ReaderT . const

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  liftIO = lift . liftIO

newtype StateT s m a = StateT
  { runStateT :: s -> m (a, s)
  }

instance (Monoid s, Show s, Show (m (a, s))) => Show (StateT s m a) where
  show (StateT sma) =
    "StateT called with '" ++ show s ++ "' is '" ++ show (sma s) ++ "'"
    where
      s = mempty

instance (Arbitrary s, EqProp (m (a, s)), Show s) => EqProp (StateT s m a) where
  StateT sma =-= StateT sma' = sma =-= sma'

instance (CoArbitrary s, Arbitrary (m (a, s))) => Arbitrary (StateT s m a) where
  arbitrary = StateT <$> arbitrary

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sma) = StateT $ (fmap . fmap) (first f) sma

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT smab <*> StateT sma =
    StateT $ \s -> do
      (ab, s') <- smab s
      (a, s'') <- sma s'
      return (ab a, s'')

instance Monad m => Monad (StateT s m) where
  return = pure
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT sma >>= aSsmb =
    StateT $ \s -> do
      (a, s') <- sma s
      runStateT (aSsmb a) s'

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift ma =
    StateT $ \s -> do
      a <- ma
      return (a, s)

instance MonadIO m => MonadIO (StateT s m) where
  liftIO :: IO a -> StateT s m a
  liftIO = lift . liftIO

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
--embedded = ??? (const (Right (Just 1)))
embedded = MaybeT (ExceptT (ReaderT (const (pure (Right (Just 1))))))

rDec :: Num a => ReaderT a Identity a
rDec = ReaderT $ Identity . flip (-) 1

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> putStrLn ("Hi: " ++ show r) $> (r + 1)

sPrintAndAccum :: (Num a, Show a) => StateT a IO String
sPrintAndAccum = StateT $ \s -> putStrLn ("Hi: " ++ show s) $> (show s, s + 1)

{- "Fix the code" -}
isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)

{- "Hit counter" -}
data Config = Config
  { counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)

type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = (M.insert k count m, count)
  where
    count = M.findWithDefault 0 k m + 1

app :: Scotty ()
-- app ~ Scotty () ~ ScottyT e m () ~ ScottyT Text (ReaderT Config IO) ()
app =
  get "/:key" $
   -- block is ~ ActionT e m () ~ ActionT Text (ReaderT Config IO) ()
   do
    unprefixed <- param "key"
    config <- lift (ReaderT return)
    let key' = mappend (prefix config) unprefixed
    newInteger <- liftIO $ atomicModifyIORef' (counts config) (bumpBoomp key')
    html $
      mconcat ["<h1>Success! Count was: ", TL.pack $ show newInteger, "</h1>"]

main' :: IO ()
main' = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR reader = runReaderT reader config
  scottyT 3000 runR app
  -- runR ~ (m Response -> IO Response) ~ (ReaderT Config IO Response -> IO Response)
  --                                     ~ ReaderT (config -> IO Response)
  -- app ~ Scotty () ~ ScottyT e m () ~ ScottyT Text (ReaderT Config IO) ()

spec :: SpecWith ()
spec = do
  describe "EitherT" $ do
    testBatch $ functor (undefined :: EitherT String Maybe (Int, Int, Int))
    testBatch $ applicative (undefined :: EitherT String Maybe (Int, Int, Int))
    testBatch $ monad (undefined :: EitherT String Maybe (Int, Int, Int))
    it "swapEitherT" $ do
      swapEitherT (EitherT (Just (Right 3))) `shouldBe`
        EitherT (Just (Left 3 :: Either Int String))
      swapEitherT (EitherT (Just (Left "hi"))) `shouldBe`
        EitherT (Just (Right "hi" :: Either Int String))
    it "eitherT" $ do
      eitherT
        (\a -> Just (a ++ "world"))
        (Just . show . (+ 2))
        (EitherT (Just (Right 3))) `shouldBe`
        Just "5"
      eitherT
        (\a -> Just (a ++ "world"))
        (Just . show . (+ 2))
        (EitherT (Just (Left "hello"))) `shouldBe`
        Just "helloworld"
  describe "ReaderT" $ do
    testBatch $ functor (undefined :: ReaderT String Maybe (Int, Int, Int))
    testBatch $ applicative (undefined :: ReaderT String Maybe (Int, Int, Int))
    testBatch $ monad (undefined :: ReaderT String Maybe (Int, Int, Int))
  describe "StateT" $ do
    testBatch $ functor (undefined :: StateT String Maybe (Int, Int, Int))
    testBatch $ applicative (undefined :: StateT String Maybe (Int, Int, Int))
    testBatch $ monad (undefined :: StateT String Maybe (Int, Int, Int))
  describe "Lift More" $ do
    it "can lift to EitherT" $ do
      lift (Just 1) `shouldBe`
        EitherT (Just $ Right 1 :: Maybe (Either String Int))
      lift Nothing `shouldBe` EitherT (Nothing :: Maybe (Either String Int))
    it "can lift to ReaderT" $ do
      runReaderT (lift $ Just 1) "hi" `shouldBe` Just 1
      runReaderT (lift Nothing) "hi" `shouldBe` (Nothing :: Maybe Int)
    it "can lift to StateT" $ do
      runStateT (lift $ Just 1) "hi" `shouldBe` Just (1, "hi")
      runStateT (lift Nothing) "hi" `shouldBe` (Nothing :: Maybe (Int, String))
  describe "Chapter Exercises" $ do
    it "rDec decrements" $ fmap (runReaderT rDec) [1 .. 10] `shouldBe` [0 .. 9]
    it "rShow shows" $ fmap (runReaderT rShow) [1 .. 10] `shouldBe` Identity .
      show <$>
      [1 .. 10]
    it "rPrintAndInc prints and increments" $ do
      nums <- traverse (runReaderT rPrintAndInc) [1 .. 10]
      nums `shouldBe` [2 .. 11]
    it "sPrintAndAccum prints and accumulates" $ do
      states <- mapM (runStateT sPrintAndAccum) [1 .. 5]
      states `shouldBe` [("1", 2), ("2", 3), ("3", 4), ("4", 5), ("5", 6)]
\end{code}
