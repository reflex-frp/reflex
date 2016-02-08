{-# LANGUAGE ConstraintKinds, GADTs, RankNTypes, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Reflex.Test.Micro (testCases) where

import Reflex
import Reflex.TestPlan

import Control.Monad
import Control.Applicative
import Data.Char
import Data.Monoid
import Data.Functor.Misc

import Data.Foldable

import Data.Map (Map)
import qualified Data.Map as Map

import Prelude

testCases :: [(String, TestCase)]
testCases =
  [ testB "hold"  $ hold "0" =<< events1
  , testB "count" $ do
    b <- current <$> (count =<< events2)
    return $ (+ (0::Int)) <$> b

  , testB "pull-1"  $ do
      b <- hold "0" =<< events1
      return (id <$> id <$> b)


  , testB "pull-2" $ do
      b1 <- behavior1
      return (id <$> pull $ liftA2 (<>) (sample b1) (sample b1))

  , testB "pull-3" $ do
      b1 <- behavior1
      b2 <- behavior2
      return (id <$> pull $ liftA2 (<>) (sample b1) (sample b2))

  , testB "pull-4" $ do
    es <- planList ["a", "b", "c"]
    e <- plan [(0, ())]

    b <- hold (constant "") $
      pushAlways (const $ hold "z" es) e

    return (join b)

  , testE "tag-1" $ do
      b1 <- behavior1
      e <- events2
      return (tag b1 e)

  , testE "tag-2" $ do
      b1 <- behavior1
      e <- events2
      return (tag (map toUpper <$>  b1) e)

  , testE "attach-1" $ do
      b1 <- behavior1
      e <- events2
      return (attachWith (++) (map toUpper <$> b1) e)

  , testE "leftmost" $ liftA2 leftmost2 events1 events2

  , testE "appendEvents-1" $ liftA2 appendEvents events1 events2
  , testE "appendEvents-2" $ liftA2 appendEvents events3 events2


  , testE "merge-1" $ do
      e <- events1
      return $ leftmost ["x" <$ e, "y" <$ e]

  , testE "merge-2" $ do
      e <- events1
      let m = mergeMap $ Map.fromList [(1::Int, "y" <$ e), (2, "z" <$ e)]

      let ee = flip pushAlways e $ const $ return m
      return $ coincidence ee

  , testE "onceE-1" $ do
      e <- events1
      onceE $ leftmost [e, e]


  , testE "onceE-2" $ do
      e <- events1
      b <- hold never (e <$ e)
      onceE $ switch b


  , testE "switch-1" $ do
      e <- events1
      b <- hold never (e <$ e)
      return $ switch b

  , testE "switch-2" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
            switch <$> hold (leftmost ["x" <$ e, "y" <$ e, "z" <$ e]) (e <$ e)

  , testE "switch-3" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
          switch <$> hold (leftmost ["x" <$ e, "y" <$ e, "z" <$ e]) never

  , testE "switch-4" $ do
      e <- events1
      switch <$> hold (deep e) (e <$ e)


  , testE "switch-5" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $
        return $ leftmost ["x" <$ e, "y" <$ e, "z" <$ e]

  , testE "switch-6" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
            switch <$> hold ("x" <$ e) (e <$ e)


  , testE "switchPromptly-1" $ do
      e <- events1
      let e' = e <$ e
      switchPromptly never $ e <$ e'

  , testE "switchPromptly-2" $ do
      e <- events1
      switchPromptly never $ deep (e <$ e)

  , testE "switchPromptly-3" $ do
      e <- events1
      switchPromptly never $ (e <$ deep e)

  , testE "switchPromptly-4" $ do
      e <- events1
      switchPromptly never $ (deep e <$ e)

  , testE "switch-5" $ do
      e <- events1
      switch <$> hold never (deep e <$ e)

  , testE "switchPromptly-5" $ do
    e <- events1
    switchPromptly never $ flip push e $
      const (Just <$> onceE e)

  , testE "switchPromptly-6" $ do
      e <- events1
      switchPromptly never $ flip pushAlways e $
        const (switchPromptly e never)

  , testE "coincidence-1" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $
        const $ return (id <$> e)

  , testE "coincidence-2" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $
        const $ return (deep e)

  , testE "coincidence-3" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $
        const $ return (coincidence (e <$ e))

  , testE "coincidence-4" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $
        const (onceE e)

  , testE "coincidence-5" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
        let e' = deep e
        return (coincidence (e' <$ e'))

  , testE "coincidence-6" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
        let e' = coincidence (e <$ e)
        return $ deep e'

  , testE "coincidence-7" $ do
      e <- events1
      return $ coincidence (deep e <$ e)

  , testB "holdWhileFiring" $ do
      e <- events1
      eo <- onceE e
      bb <- hold (constant "x") $ pushAlways (const $ hold "a" eo) eo
      return $ pull $ sample =<< sample bb

  , testE "joinDyn" $ do
      e <- events1
      bb <- hold "b" e
      bd <- hold never . fmap (const e) =<< onceE e

      eOuter <- pushAlways sample . fmap (const bb) <$> onceE e
      let eInner = switch bd
      return $ leftmost [eOuter, eInner]

  , testB "foldDyn"  $ do
      d <- foldDyn (++) "0" =<< events1
      return (current d)

  , testB "mapDyn"  $ do
      d <- foldDyn (++) "0" =<< events1
      current <$> mapDyn (map toUpper) d

  , testB "combineDyn"  $ do
      d1 <- foldDyn (++) "0" =<< events1
      d2 <- mapDyn (map toUpper) =<< foldDyn (++) "0" =<< events2

      current <$> combineDyn (<>) d1 d2
  , testE "fan-1" $ do
      e <- fmap toMap <$> events1
      let es = select (fanMap e) . Const2 <$> values

      return (mergeList es)

  , testE "fan-2" $ do
      e <- fmap toMap <$> events3
      let es = select (fanMap e) . Const2 <$> values

      return (mergeList es)

  , testE "fan-3" $ do
      f <- fanMap <$> fmap toMap <$> events3
      return $  select f (Const2 'c')

  , testE "fan-4" $ do
      e <- fmap toMap <$> events1
      return $ toUpper <$> select (fanMap e) (Const2 'a')

  , testE "fan-5" $ do
      e <- fmap toMap <$> events2
      return $ toUpper <$> select (fanMap e) (Const2 'c')

  , testE "fan-6" $ do
      f <- fanMap <$> fmap toMap <$> events1
      return $ toList <$> mergeList [ select f (Const2 'b'), select f (Const2 'b'), select f (Const2 'e'), select f (Const2 'e') ]

  -- , testE "switchMerge-1" $ switchMergeMap mempty =<< increasing

  -- , testE "switchMerge-2" $ do
  --     e <- eventsFrom 0
  --     switchMergeMap (Map.singleton 0 e) =<< increasing

  -- , testE "switchMerge-3" $ do
  --     es <- eventsMany
  --     switchMergeMap es =<< increasing

  -- , testE "switchMerge-4" $ do
  --     es <- eventsMany
  --     switchMergeMap es =<< decreasing

  ] where

    events1, events2, events3 ::  TestPlan t m => m (Event t String)
    events1 = plan [(1, "a"), (2, "b"), (5, "c"), (7, "d"), (8, "e")]
    events2 = plan [(1, "e"), (3, "d"), (4, "c"), (6, "b"), (7, "a")]

    events3 = liftA2 appendEvents events1 events2

    eventsFrom ::  TestPlan t m => Word -> m (Event t Int)
    eventsFrom n = plan $ zip [n..8] [1..8]

    increasing :: TestPlan t m => m (Event t (Map Int (Event t Int)))
    increasing = do
      es <- mapM eventsFrom [1..8]
      planList $ zipWith Map.singleton [1..8] es

    decreasing :: TestPlan t m => m (Event t (Map Int (Event t Int)))
    decreasing = planList $ zipWith Map.singleton [1..8] (repeat never)

    eventsMany :: TestPlan t m => m (Map Int (Event t Int))
    eventsMany = do
      es <- mapM eventsFrom [8,7..1]
      return $ Map.fromList $ zip [1..8] es


    values = "abcde"
    toMap str = Map.fromList $ map (\c -> (c, c)) str


    behavior1, behavior2 :: forall t m. TestPlan t m => m (Behavior t String)
    behavior1 =  hold "1" =<< events1
    behavior2 =  hold "2" =<< events2

    deep e = leftmost [e, e]
    leftmost2 e1 e2 = leftmost [e1, e2]


