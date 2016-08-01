{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Reflex.Bench.Focused where

import Reflex
import Reflex.TestPlan

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Identity
import Data.Traversable (for, traverse)

import qualified Data.Dependent.Map as DMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Data.Functor.Misc

import Data.List
import Data.List.Split
import Data.Tuple

import Data.Monoid
import Data.Word

import Control.DeepSeq

import Prelude hiding (concat, sum)


mergeTree :: Num a => (Monoid (f [a]), Functor f) => Int -> [f a] -> f a
mergeTree n es | length es <= n =  sum' es
               | otherwise = mergeTree n subTrees
  where
    merges   = chunksOf n es
    subTrees = map sum' merges
    sum'     = fmap sum . mconcat . fmap (fmap (:[]) )

mergeListDyn :: Reflex t => [Dynamic t a] -> Dynamic t [a]
mergeListDyn = mconcat . fmap (fmap pure)

mergeTreeDyn :: (Num a, Reflex t) => Int -> [Dynamic t a] -> Dynamic t a
mergeTreeDyn n dyns | length dyns <= n = sumDyns dyns
                    | otherwise = mergeTreeDyn n $ fmap sumDyns merges
  where
    merges = chunksOf n dyns
    sumDyns = fmap sum . mergeListDyn



-- N events all firing for N frames
continuous :: TestPlan t m => Word -> Word -> m [Event t Word]
continuous n frames = for [1..n] $ \i -> plan $ (, i) <$> [1..frames]

-- Fire N events once every P frames for total N frames (interspersed evenly)
occasional :: TestPlan t m => Word -> Word -> Word -> m [Event t Word]
occasional n period frames = traverse plan $ zipWith occs [1..] phases
  where
    phases       = take (fromIntegral n) $ concat (repeat [1..period])
    occs i phase = (, i) <$> [1, phase+1..frames]



-- Event which never fires (but isn't 'never')
never' :: TestPlan t m => m (Event t Word)
never' = plan []


-- Event which fires once on first frame
event :: TestPlan t m => m (Event t Word)
event = plan [(1, 0)]

-- Event which fires constantly over N frames
events :: TestPlan t m => Word -> m (Event t Word)
events n  = plan $ (\i -> (i, i)) <$> [1..n]


-- N events all originating from one event
fmapFan :: Reflex t => Word -> Event t Word -> [Event t Word]
fmapFan n e = (\i -> (+i) <$> e) <$> [1..n]


-- Make a fan using the value in the event converting it to a singleton Map
-- use a fan of size n (key is the value mod n)
fanMerge :: Reflex t => Word -> Event t Word -> Event t Word
fanMerge n e =  leftmost $ s <$> [0..n - 1] where
  s k = select f (Const2 k)
  f = fan (toMap <$> e)
  toMap k = DMap.singleton (Const2 $ k `mod` n) (Identity k)

-- Dumb version of the above using simple fmapMaybe to filter
fmapFanMerge :: Reflex t => Word -> Event t Word -> Event t Word
fmapFanMerge n e =  leftmost $ s <$> [0..n - 1] where
  s k = flip fmapMaybe e $ \j -> if k == j `mod` n
    then Just k
    else Nothing



-- Create an Event with a Map which is dense to size N
denseMap :: TestPlan t m => Word -> m (Event t (Map Word Word))
denseMap n = plan [(1, m)] where
  m = Map.fromList $ zip [1..n] [1..n]

iter :: (a -> a) -> Word -> a -> a
iter _ 0 a = a
iter f n a = iter f (n - 1) (f a)

iterM :: Monad m => (a -> m a) -> Word -> a -> m a
iterM _ 0 a = return a
iterM f n a = iterM f (n - 1) =<< f a

fmapChain :: (Functor f) => Word -> f Word -> f Word
fmapChain = iter (fmap (+1))

mapDynChain :: (Reflex t, MonadHold t m) => Word -> Dynamic t Word -> m (Dynamic t Word)
mapDynChain = iterM (return . fmap (+1))

joinDynChain :: (Reflex t, MonadHold t m) => Word -> Dynamic t Word -> m (Dynamic t Word)
joinDynChain = iterM (\d -> return $ join $ fmap (const d) d)

combineDynChain :: (Reflex t, MonadHold t m) => Word -> Dynamic t Word -> m (Dynamic t Word)
combineDynChain = iterM (\d -> return $ zipDynWith (+) d d)

pingPong :: (Reflex t, MonadHold t m, MonadFix m) => Event t a -> Event t a -> m (Event t a)
pingPong e1 e2 = do
  rec
    d <- foldDyn (const swap) (e1, e2) e
    let e = switch (fst <$> current d)
  return e

switchFactors :: (Reflex t, MonadHold t m) => Word -> Event t Word -> m (Event t Word)
switchFactors 0 e = return e
switchFactors i e = do
    b <- hold ((+1) <$> e) (e <$ ffilter ((== 0) . (i `mod`)) e)
    switchFactors (i - 1) (switch b)


switchChain :: (Reflex t, MonadHold t m, MonadFix m) => Word -> Event t Word -> m (Event t Word)
switchChain  = iterM (\e -> pingPong e ((+1) <$> e))


switchChain2 :: (Reflex t, MonadHold t m, MonadFix m) => Word -> Event t Word -> m (Event t Word)
switchChain2 = iterM (\e -> pingPong e ((+1) <$> leftmost [e, e]))

switchPromptlyChain :: (Reflex t, MonadHold t m) => Word -> Event t Word -> m (Event t Word)
switchPromptlyChain = iterM $ \e ->
    switchPromptlyDyn <$> holdDyn e (e <$ e)


coinChain :: Reflex t => Word -> Event t Word -> Event t Word
coinChain = iter $ \e -> coincidence (e <$ e)

fanMergeChain :: Reflex t => Word -> Word -> Event t Word -> Event t Word
fanMergeChain width = iter $ fanMerge width

fmapFanMergeChain :: Reflex t => Word -> Word -> Event t Word -> Event t Word
fmapFanMergeChain width = iter $ fmapFanMerge width

-- | Data type for a collection (a map) which is updated by providing differences
data UpdatedMap t k a = UpdatedMap (Map k a) (Event t (Map k (Maybe a)))

-- This will hopefully become a primitive (faster!)
switchMergeEvents ::  (MonadFix m, MonadHold t m, Reflex t, Ord k) =>  UpdatedMap t k (Event t a)  -> m (Event t (Map k a))
switchMergeEvents mapChanges = switch . fmap mergeMap  <$> holdMap mapChanges

-- switchMergeMaybe :: (MonadFix m, MonadHold t m, Reflex t, Ord k) =>  UpdatedMap t k (Event t a)  -> m (Event t (Map k a))
-- switchMergeMaybe (UpdatedMap initial changes) = switchMergeMap initial (fmap (fromMaybe never) <$> changes)

switchMergeBehaviors  :: forall a t m k. (MonadFix m, MonadHold t m, Reflex t, Ord k) =>  UpdatedMap t k (Behavior t a)  -> m (Behavior t (Map k a))
switchMergeBehaviors mapChanges = pull . joinMap <$> holdMap mapChanges
  where joinMap m = traverse sample =<< sample m

-- | Turn an UpdatedMap into a Dynamic by applying the differences to the initial value
holdMapDyn :: (Reflex t, MonadHold t m, MonadFix m, Ord k) => UpdatedMap t k a -> m (Dynamic t (Map k a))
holdMapDyn (UpdatedMap initial changes) = foldDyn (flip (Map.foldWithKey modify)) initial changes

  where
    modify k Nothing items = Map.delete k items
    modify k (Just item) items = Map.insert k item items


-- | Hold an UpdatedMap as a behavior by applying differences to the initial value
holdMap :: (Reflex t, MonadHold t m, MonadFix m, Ord k) => UpdatedMap t k a -> m (Behavior t (Map k a))
holdMap = (current <$>) . holdMapDyn


increasingMerge :: TestPlan t m => [a] -> m (UpdatedMap t Int a)
increasingMerge xs = do
  e   <- planList (zipWith Map.singleton  [1..] (Just <$> xs) ++ [reset])
  return (UpdatedMap Map.empty e)
    where
      reset = Map.fromList $ zip [1..] (const Nothing <$> xs)

decreasingMerge ::  (TestPlan t m) => [a] -> m (UpdatedMap t Int a)
decreasingMerge xs = do
  e <- planList (zipWith Map.singleton  [1..] (const Nothing <$> xs) ++ [reset])
  return (UpdatedMap initial e)
    where
      initial = Map.fromList $ zip [1..] xs
      reset = Just <$> initial

modifyMerge :: (TestPlan t m) =>  [a] -> m (UpdatedMap t Int a)
modifyMerge xs = do
  e <- planList $ zipWith Map.singleton  [1..] (Just <$> xs)
  return (UpdatedMap initial e)
    where
      initial = Map.fromList $ zip [1..] xs


countMany :: (Reflex t, MonadHold t m, MonadFix m) => [Event t a] -> m [Behavior t Int]
countMany = traverse (fmap current . count)

countManyDyn :: (Reflex t, MonadHold t m, MonadFix m) => [Event t a] -> m [Dynamic t Int]
countManyDyn = traverse count


-- Given a function which creates a sub-network (from a single event)
-- Create a plan which uses a switch to substitute it
switches :: TestPlan t m => Word -> (forall n. (MonadHold t n, MonadFix n) => Event t Word -> n (Event t a)) -> m (Event t a)
switches numFrames f = do
  es <- events numFrames
  switch <$> hold never (makeEvents es)

    where
      makeEvents es =  pushAlways (\n -> f (fmap (+n) es)) es



-- Two sets of benchmarks, one which we're interested in testing subscribe time (as well as time to fire frames)
-- the other, which we're only interested in time for running frames
subscribing :: Word -> Word -> [(String, TestCase)]
subscribing n frames =
  [ testSub "fmapFanMerge"        $ return . mergeList . fmapFan n
  , testSub "fanMerge"            $ return . fanMerge n
  , testSub "fmapFan/mergeTree"   $ return . mergeTree 8 . fmapFan n
  , testSub "fmapChain"           $ return . fmapChain n
  , testSub "switchChain"         $ switchChain n
  , testSub "switchPromptlyChain" $ switchPromptlyChain n
  , testSub "switchFactors"       $ switchFactors n
  , testSub "coincidenceChain"    $ return . coinChain n
  ]

  where
    testSub :: (Eq a, Show a, NFData a) => String -> (forall t n. (Reflex t, MonadHold t n, MonadFix n) => Event t Word -> n (Event t a)) -> (String, TestCase)
    testSub name test = testE name (switches frames test)

-- Set of benchmark to test specifically merging dynamic collections
-- a pattern which occurs frequently in reflex-dom collections
merging :: Word -> [(String, TestCase)]
merging n =
  [ testE "static events"        $ mergeList <$> sparse
  , testE "increasing events"    $ switchMergeEvents =<< increasingMerge =<< sparse
  , testE "decreasing events"    $ switchMergeEvents =<< decreasingMerge =<< sparse
  , testE "modify events"        $ switchMergeEvents =<< modifyMerge =<< sparse

  , testB "static behaviors"     $ pull . traverse sample <$> counters
  , testB "increasing behaviors" $ switchMergeBehaviors =<< increasingMerge =<< counters
  , testB "decreasing behaviors" $ switchMergeBehaviors =<< decreasingMerge =<< counters
  , testB "modify behaviors"     $ switchMergeBehaviors =<< modifyMerge =<< counters


  -- , testE "increasing events/switchMerge"    $ switchMergeMaybe =<< increasingMerge =<< sparse
  -- , testE "decreasing events/switchMerge"    $ switchMergeMaybe =<< decreasingMerge =<< sparse
  -- , testE "modify events/switchMerge"        $ switchMergeMaybe =<< modifyMerge =<< sparse
  ]

  where
    sparse :: TestPlan t m => m [Event t Word]
    sparse = fmap (fmap (+1)) <$> occasional n 8 n

    counters :: TestPlan t m => m [Behavior t Int]
    counters = countMany =<< sparse



fans :: Word -> [(String, TestCase)]
fans n =
  [  testE "fanMapChain"               $ fanMergeChain n 10 <$> e
  ,  testE "fmapFanMapChain"           $ fmapFanMergeChain n 10 <$> e

  , testE "fanMerge " $ fanMerge n <$> events n

  , testE "fmapFanMerge " $ fmapFanMerge n <$> events n
  , testE "triggerMerge" $ leftmost <$> for [1..n] (\i -> plan [(i, i)])

  ]
  where

    e :: TestPlan t m => m (Event t Word)
    e = events 100




dynamics :: Word -> [(String, TestCase)]
dynamics n =
  [ testE "mapDynChain"         $ fmap updated $ mapDynChain n =<< d
  , testE "joinDynChain"        $ fmap updated $ joinDynChain n =<< d
  , testE "combineDynChain"     $ fmap updated $ combineDynChain n =<< d
  , testE "dense mergeTree"     $ fmap (updated . mergeTreeDyn 8) dense
  , testE "sparse mergeTree"    $ fmap (updated . mergeTreeDyn 8) sparse
  , testE "mergeDyn"            $ fmap (updated . fmap sum . mergeListDyn) sparse
  ] where

    d :: TestPlan t m => m (Dynamic t Word)
    d = count =<< events 10

    sparse, dense :: TestPlan t m => m [Dynamic t Int]
    sparse = countManyDyn =<< occasional n 8 16
    dense  = countManyDyn =<< continuous n 2


firing :: Word -> [(String, TestCase)]
firing n =
  [ testE "fmapChain"           $ fmapChain n <$> e
  , testE "switchChain2"        $ switchChain2 n =<< e
  , testE "switchPromptlyChain" $ switchPromptlyChain n =<< e
  , testE "switchFactors"       $  switchFactors n =<< e
  , testE "coincidenceChain"    $ coinChain n <$> e

  , testE "dense mergeTree"      $ mergeTree 8 <$> dense
  , testE "sparse mergeTree"  $ mergeTree 8 <$> sparse

  -- Triggers test failure in Spider
  --, testE "runFrame"               $ events n

  , testB "sum counters" $ do
      counts <- counters
      return $ pull $ sum <$> traverse sample counts

  , testB "pullChain"                 $ fmapChain n . current <$> (count =<< events 4)
  , testB "pullChain2"                $ do
    es <- events 4
    e' <- plan [(0, ())]

    b <- hold (constant 0) $
      pushAlways (const $ fmapChain n <$> hold 0 es) e'

    return (join b)
  , testB "counters/pullTree" $ mergeTree 8 <$> counters

  ]
    where
      e :: TestPlan t m => m (Event t Word)
      e = events 10

      dense, sparse :: TestPlan t m => m [Event t Word]
      dense = continuous n 2
      sparse = occasional n 8 16

      counters :: TestPlan t m => m [Behavior t Int]
      counters = countMany =<< sparse





