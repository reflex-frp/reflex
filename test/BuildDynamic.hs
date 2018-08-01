{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Fix
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.These
import Data.Monoid

import Reflex
import Test.Run

pushDyn :: (Reflex t, MonadHold t m) => (a -> PushM t b) -> Dynamic t a -> m (Dynamic t b)
pushDyn f d = buildDynamic (sample (current d) >>= f) (pushAlways f (updated d))

foldedDyn :: (Reflex t, MonadHold t m) => (a -> a -> a) -> Dynamic t a -> m (Dynamic t a)
foldedDyn f d = fmap join $ flip buildDynamic never $ do
 a <- sample (current d)
 foldDyn f a (updated d)

scannedDyn :: (Reflex t, MonadHold t m) => Dynamic t a -> m (Dynamic t [a])
scannedDyn = fmap (fmap reverse) . foldedDyn (<>) . fmap pure

scanInnerDyns :: (Reflex t, MonadHold t m) => Dynamic t (Dynamic t a) -> m (Dynamic t [a])
scanInnerDyns d = do
  scans <- scannedDyn d
  return (join (fmap distributeListOverDynPure scans))


main :: IO ()
main = do

  r <- last <$> runAppB testInnerHoldsE (Just <$> [1..4])
  -- [[[0,1,2],[1,2],[2]]]]

  -- r <- runAppB testScan counter

  -- r <- runAppB testStrictness (fmap Just [1..9])


  print r
  return ()

    where
      counter = fmap Just [1..9] <> [Nothing]
      eithers = fmap Just [ Left 1, Left 2, Right 3, Left 4, Left 5, Right 6, Right 7, Right 8, Left 9 ] <> [Nothing]

testIdentity :: (Reflex t, MonadHold t m, MonadFix m)
         => Event t Int -> m (Behavior t Int)
testIdentity e = do
  d <- holdDyn 0 e
  d' <- pushDyn return d
  d'' <- pushDyn return d'

  return (current d'')

testStrictness :: (Reflex t, MonadHold t m, MonadFix m)
         => Event t Int -> m (Behavior t Int)
testStrictness e = do

  rec
    d'' <- pushDyn return d'
    d' <- pushDyn return d
    d <- holdDyn 0 e

  return (current d'')


testScan :: (Reflex t, MonadHold t m, MonadFix m)
           => Event t Int -> m (Behavior t [Int])
testScan e = do
  d <- holdDyn 0 e
  scan <- scannedDyn d
  return (current scan)

testFactor :: (Reflex t, MonadHold t m, MonadFix m)
           => Event t (Either Int Int)  -> m (Behavior t (Either Int Int))
testFactor e = do
  d <- holdDyn (Left 0) e

  eithers <- eitherDyn d
  return $ current (join (fmap unFactor eithers))

  where
    unFactor = either (fmap Left) (fmap Right)

testInnerHolds :: (Reflex t, MonadHold t m, MonadFix m)
           => Event t Int -> m (Behavior t [Int])
testInnerHolds e =  do
  d <- holdDyn 0 e

  d' <- pushDyn (\a -> foldDyn (:) [a] (updated d)) d
  return $ current (join d')

testInnerHoldsE :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
           => Event t Int -> m (Behavior t [[Int]])
testInnerHoldsE e =  do
  initial <- f 0
  d <- foldDyn snoc [initial] updates
  return $ current (join (fmap distributeListOverDynPure d))
    where
      f :: forall n. (MonadHold t n, MonadFix n) => Int -> n (Dynamic t [Int])
      f a     = foldDyn snoc [a] e
      updates = pushAlways f e

      snoc x xs = xs ++ [x]

testInnerHolds1 :: (Reflex t, MonadHold t m, MonadFix m)
           => Event t Int -> m (Behavior t [[Int]])
testInnerHolds1 e =  do
  d <- fmap pure <$> holdDyn 0 e

  d' <- pushDyn (\a -> foldDyn (flip mappend) a (updated d)) d
  current <$> scanInnerDyns d'
