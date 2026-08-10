{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Reflex.Test.Micro (testCases) where

import Reflex
import Reflex.TestPlan

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Char
import Data.Foldable
import Data.Functor.Misc
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Monoid
import Reflex.Patch.MapWithMove (moveMapKey, insertMapKey, deleteMapKey)

import Prelude

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



{-# ANN testCases "HLint: ignore Functor law" #-}
testCases :: [(String, TestCase)]
testCases =
  [ testB' "hold" [(0,"0"),(1,"0"),(2,"a"),(3,"b"),(4,"b"),(5,"b"),(6,"c"),(7,"c"),(8,"d"),(9,"e")] $ hold "0" =<< events1

  , testB' "count" [(0,0),(1,0),(2,1),(3,1),(4,2),(5,3),(6,3),(7,4),(8,5)] $ do
      b <- current <$> (count =<< events2)
      return $ (+ (0::Int)) <$> b

  , testB' "pull-1" [(0,"0"),(1,"0"),(2,"a"),(3,"b"),(4,"b"),(5,"b"),(6,"c"),(7,"c"),(8,"d"),(9,"e")] $ do
      b <- hold "0" =<< events1
      return (pull $ sample $ pull $ sample b)

  , testB' "pull-2" [(0,"11"),(1,"11"),(2,"aa"),(3,"bb"),(4,"bb"),(5,"bb"),(6,"cc"),(7,"cc"),(8,"dd"),(9,"ee")] $ do
      b1 <- behavior1
      return (pull $ liftA2 (<>) (sample b1) (sample b1))

  , testB' "pull-3" [(0,"12"),(1,"12"),(2,"ae"),(3,"be"),(4,"bd"),(5,"bc"),(6,"cc"),(7,"cb"),(8,"da"),(9,"ea")] $ do
      b1 <- behavior1
      b2 <- behavior2
      return (pull $ liftA2 (<>) (sample b1) (sample b2))

  , testB' "pull-4" [(0,""),(1,"z"),(2,"a"),(3,"b"),(4,"c")] $ do
      es <- planList ["a", "b", "c"]
      e <- plan [(0, ())]
      b <- hold (constant "") $
        pushAlways (const $ hold "z" es) e
      return (join b)

  , testE' "id" [(1,"e"),(3,"d"),(4,"c"),(6,"b"),(7,"a")] $ do
      events2

  , testE' "fmap-id" [(1,"e"),(3,"d"),(4,"c"),(6,"b"),(7,"a")] $ do
      e <- events2
      return $ fmap id e

  , testE' "tag-1" [(1,"1"),(3,"b"),(4,"b"),(6,"c"),(7,"c")] $ do
      b1 <- behavior1
      e <- events2
      return (tag b1 e)

  , testE' "tag-2" [(1,"1"),(3,"B"),(4,"B"),(6,"C"),(7,"C")] $ do
      b1 <- behavior1
      e <- events2
      return (tag (map toUpper <$>  b1) e)

  , testE' "attach-1" [(1,"1e"),(3,"Bd"),(4,"Bc"),(6,"Cb"),(7,"Ca")] $ do
      b1 <- behavior1
      e <- events2
      return (attachWith (++) (map toUpper <$> b1) e)

  , testE' "leftmost" [(1,"a"),(2,"b"),(3,"d"),(4,"c"),(5,"c"),(6,"b"),(7,"d"),(8,"e")] $ liftA2 leftmost2 events1 events2

  , testE' "appendEvents-1" [(1,"ae"),(2,"b"),(3,"d"),(4,"c"),(5,"c"),(6,"b"),(7,"da"),(8,"e")] $ liftA2 mappend events1 events2

  , testE' "appendEvents-2" [(1,"aee"),(2,"b"),(3,"dd"),(4,"cc"),(5,"c"),(6,"bb"),(7,"daa"),(8,"e")] $ liftA2 mappend events3 events2

  , testE' "merge-1" [(1,"x"),(2,"x"),(5,"x"),(7,"x"),(8,"x")] $ do
      e <- events1
      return $ leftmost ["x" <$ e, "y" <$ e]

  , testE' "merge-2" [(1,Map.fromList [(1,"y"),(2,"z")]),(2,Map.fromList [(1,"y"),(2,"z")]),(5,Map.fromList [(1,"y"),(2,"z")]),(7,Map.fromList [(1,"y"),(2,"z")]),(8,Map.fromList [(1,"y"),(2,"z")])] $ do
      e <- events1
      let m = mergeMap $ Map.fromList [(1::Int, "y" <$ e), (2, "z" <$ e)]
      let ee = flip pushAlways e $ const $ return m
      return $ coincidence ee

  , testE' "headE-1" [(1,"a")] $ do
      e <- events1
      headE $ leftmost [e, e]

  , testE' "headE-2" [(2,"b")] $ do
      e <- events1
      b <- hold never (e <$ e)
      headE $ switch b

  , testE' "headE-rec" [(1,"a")] $ do
      e <- events1
      rec !eHeadOfLater <- headE eLater
          eLater <- headE e
      return eHeadOfLater

  -- Tests for a bug caught during headE development where the argument event's
  -- heightRef was incorrectly reused as headE's heightRef.
  , testE' "headE-height-after-head" [(1,())] $ do
      unsubscribeHead <- plan [(1, ())]
      dropFromMerge <- plan [(2, ())]
      let taller = void (mergeList [unsubscribeHead, unsubscribeHead])
      parent <- hold unsubscribeHead (taller <$ unsubscribeHead)
      let switched = switch parent
      headOfSwitched <- headE switched
      mergeParents <- holdIncremental
        (IntMap.singleton (0 :: Int) headOfSwitched)
        (PatchIntMap (IntMap.singleton 0 Nothing) <$ dropFromMerge)
      -- 'switched' stays subscribed here, so it goes on subscribing (and so
      -- changing its height) after the headE has stopped listening to it:
      pure $ leftmost [void (mergeIntIncremental mergeParents), switched]

  , testE' "switch-1" [(2,"b"),(5,"c"),(7,"d"),(8,"e")] $ do
      e <- events1
      b <- hold never (e <$ e)
      return $ switch b

  , testE' "switch-2" [(1,"x"),(2,"x"),(5,"x"),(7,"x"),(8,"x")] $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
            switch <$> hold (leftmost ["x" <$ e, "y" <$ e, "z" <$ e]) (e <$ e)

  , testE' "switch-3" [(1,"x"),(2,"x"),(5,"x"),(7,"x"),(8,"x")] $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
          switch <$> hold (leftmost ["x" <$ e, "y" <$ e, "z" <$ e]) never

  , testE' "switch-4" [(1,"a"),(2,"b"),(5,"c"),(7,"d"),(8,"e")] $ do
      e <- events1
      switch <$> hold (deep e) (e <$ e)

  , testE' "switch-5" [(1,"x"),(2,"x"),(5,"x"),(7,"x"),(8,"x")] $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $
        return $ leftmost ["x" <$ e, "y" <$ e, "z" <$ e]

  , testE' "switch-6" [(1,"x"),(2,"x"),(5,"x"),(7,"x"),(8,"x")] $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
            switch <$> hold ("x" <$ e) (e <$ e)

  , testE' "switchHoldPromptly-1" [(1,"a"),(2,"b"),(5,"c"),(7,"d"),(8,"e")] $ do
      e <- events1
      let e' = e <$ e
      switchHoldPromptly never $ e <$ e'

  , testE' "switchHoldPromptly-2" [(1,"a"),(2,"b"),(5,"c"),(7,"d"),(8,"e")] $ do
      e <- events1
      switchHoldPromptly never $ deep (e <$ e)

  , testE' "switchHoldPromptly-3" [(1,"a"),(2,"b"),(5,"c"),(7,"d"),(8,"e")] $ do
      e <- events1
      switchHoldPromptly never $ (e <$ deep e)

  , testE' "switchHoldPromptly-4" [(1,"a"),(2,"b"),(5,"c"),(7,"d"),(8,"e")] $ do
      e <- events1
      switchHoldPromptly never $ (deep e <$ e)

  , testE' "switch-7" [(2,"b"),(5,"c"),(7,"d"),(8,"e")] $ do
      e <- events1
      switch <$> hold never (deep e <$ e)

  , testE' "switchHoldPromptly-5" [(1,"a"),(2,"b"),(5,"c"),(7,"d"),(8,"e")] $ do
    e <- events1
    switchHoldPromptly never $ flip push e $
      const (Just <$> headE e)

  , testE' "switchHoldPromptly-6" [(1,"a"),(2,"b"),(5,"c"),(7,"d"),(8,"e")] $ do
      e <- events1
      switchHoldPromptly never $ flip pushAlways e $
        const (switchHoldPromptly e never)

  , testE' "coincidence-1" [(1,"a"),(2,"b"),(5,"c"),(7,"d"),(8,"e")] $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $
        const $ return e

  , testE' "coincidence-2" [(1,"a"),(2,"b"),(5,"c"),(7,"d"),(8,"e")] $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $
        const $ return (deep e)

  , testE' "coincidence-3" [(1,"a"),(2,"b"),(5,"c"),(7,"d"),(8,"e")] $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $
        const $ return (coincidence (e <$ e))

  , testE' "coincidence-4" [(1,"a"),(2,"b"),(5,"c"),(7,"d"),(8,"e")] $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $
        const (headE e)

  , testE' "coincidence-5" [(1,"a"),(2,"b"),(5,"c"),(7,"d"),(8,"e")] $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
        let e' = deep e
        return (coincidence (e' <$ e'))

  , testE' "coincidence-6" [(1,"a"),(2,"b"),(5,"c"),(7,"d"),(8,"e")] $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
        let e' = coincidence (e <$ e)
        return $ deep e'

  , testE' "coincidence-7" [(1,"a"),(2,"b"),(5,"c"),(7,"d"),(8,"e")] $ do
      e <- events1
      return $ coincidence (deep e <$ e)

  , testE' "coincidence-incremental-height" [(1,()),(2,()),(3,()),(4,()),(5,()),(6,())] $ do
      tick <- ticks
      e2 <- mergeMapIncremental
              <$> holdIncremental mempty ((mempty :: PatchMap Int (Event t ())) <$ tick)
      pure (leftmost [ void $ coincidence (e2 <$ leftmost [tick, tick])
                     , tick])

  , testE' "coincidence-int-incremental-height" [(1,()),(2,()),(3,()),(4,()),(5,()),(6,())] $ do
      tick <- ticks
      h <- mergeIntIncremental
             <$> holdIncremental
                   (IntMap.singleton 1 (void tick))
                   (PatchIntMap (IntMap.singleton 1 (Just (void tick))) <$ tick)
      pure (leftmost [ void $ coincidence (void h <$ leftmost [tick, tick])
                     , tick])

  , testE' "coincidence-switch-reconnect-height" [(1,()),(2,()),(3,()),(4,()),(5,()),(6,())] $ do
      tick <- ticks
      flag <- foldDyn (\_ b -> not b) False tick
      let hi = leftmost [tick, tick]
      pure (leftmost [void (coincidence (switch (current $ (\b -> if b then hi else tick) <$> flag) <$ hi)), tick])

  , testE' "mergeWithMove-height" [(1,Map.fromList [(0,())]),(2,Map.fromList [(0,()),(1,())]),(3,Map.fromList [(1,())]),(4,Map.fromList [(0,()),(1,())]),(5,Map.fromList [(0,()),(2,())]),(6,Map.fromList [(0,())])] $ do
      tick <- ticks
      let lo = tick
          hi = leftmost [tick, tick]
      patches <- plan
        [ (1, insertMapKey 1 hi)
        , (2, deleteMapKey 0)
        , (3, insertMapKey 0 lo)
        , (4, moveMapKey 1 2)
        , (5, deleteMapKey 2) ]
      mergeMapIncrementalWithMove
        <$> holdIncremental (Map.singleton (0 :: Int) lo) patches

  , testB' "holdWhileFiring" [(0,"x"),(1,"x"),(2,"a"),(3,"a"),(4,"a"),(5,"a"),(6,"a"),(7,"a"),(8,"a"),(9,"a")] $ do
      e <- events1
      eo <- headE e
      bb <- hold (constant "x") $ pushAlways (const $ hold "a" eo) eo
      return $ pull $ sample =<< sample bb


  , testB' "foldDynWhileFiring" [(0,[]),(1,[]),(2,[["a","a"]]),(3,[["b","b"],["b","a","a"]]),(4,[["b","b"],["b","a","a"]]),(5,[["b","b"],["b","a","a"]]),(6,[["c","c"],["c","b","b"],["c","b","a","a"]]),(7,[["c","c"],["c","b","b"],["c","b","a","a"]]),(8,[["d","d"],["d","c","c"],["d","c","b","b"],["d","c","b","a","a"]]),(9,[["e","e"],["e","d","d"],["e","d","c","c"],["e","d","c","b","b"],["e","d","c","b","a","a"]])] $ do
    e <- events1
    d <- foldDyn (:) [] $
      pushAlways (\a -> foldDyn (:) [a] e) e

    return $ current (join (fmap distributeListOverDynPure d))

  , testE' "joinDyn" [(1,"b"),(2,"b"),(5,"c"),(7,"d"),(8,"e")] $ do
      e <- events1
      bb <- hold "b" e
      bd <- hold never . fmap (const e) =<< headE e

      eOuter <- pushAlways sample . fmap (const bb) <$> headE e
      let eInner = switch bd
      return $ leftmost [eOuter, eInner]

  , testE' "joinDyn-inner-occurrence" [(3, 1)] $ do
      updateOld <- plan [(3, 100 :: Int)]
      swap <- plan [(3, ())]
      innerOld <- holdDyn 0 updateOld
      innerNew <- holdDyn 1 never
      outer <- holdDyn innerOld (innerNew <$ swap)
      pure $ updated (join outer)

  , testB' "foldDyn" [(0,"0"),(1,"0"),(2,"a0"),(3,"ba0"),(4,"ba0"),(5,"ba0"),(6,"cba0"),(7,"cba0"),(8,"dcba0"),(9,"edcba0")] $ do
      d <- foldDyn (++) "0" =<< events1
      return (current d)

  , testB' "mapDyn" [(0,"0"),(1,"0"),(2,"A0"),(3,"BA0"),(4,"BA0"),(5,"BA0"),(6,"CBA0"),(7,"CBA0"),(8,"DCBA0"),(9,"EDCBA0")] $ do
      d <- foldDyn (++) "0" =<< events1
      return $ current $ fmap (map toUpper) d

  , testB' "combineDyn" [(0,"00"),(1,"00"),(2,"a0E0"),(3,"ba0E0"),(4,"ba0DE0"),(5,"ba0CDE0"),(6,"cba0CDE0"),(7,"cba0BCDE0"),(8,"dcba0ABCDE0"),(9,"edcba0ABCDE0")] $ do
      d1 <- foldDyn (++) "0" =<< events1
      d2 <- fmap (fmap (map toUpper)) $ foldDyn (++) "0" =<< events2

      return $ current $ zipDynWith (<>) d1 d2

  , testB' "buildDynamicStrictness" [(0,"0"),(1,"0"),(2,"a"),(3,"b"),(4,"b"),(5,"b"),(6,"c"),(7,"c"),(8,"d"),(9,"e")] $ do
      rec
        d'' <- pushDyn return d'
        d' <- pushDyn return d
        d <- holdDyn "0" =<< events1

      _ <- sample (current d'')
      return (current d'')

  , testB' "factorDyn" [(0,"a"),(1,"a"),(2,"e"),(3,"e"),(4,"d"),(5,"c"),(6,"c"),(7,"b"),(8,"a")] $ do
      d <- holdDyn (Left "a") =<< eithers

      eithers' <- eitherDyn d
      let unFactor = either id id
      return $ current (join (fmap unFactor eithers'))

  , testB' "pushDynDeep" [(0,[["d1d2d1"]]),(1,[["d1d2d1"]]),(2,[["d1d2a","d1ea"],["ad2a","aea"]]),(3,[["d1d2b","d1eb"],["ad2b","aeb"],["beb"]]),(4,[["d1d2b","d1eb","d1db"],["ad2b","aeb","adb"],["beb","bdb"]]),(5,[["d1d2b","d1eb","d1db","d1cb"],["ad2b","aeb","adb","acb"],["beb","bdb","bcb"]]),(6,[["d1d2c","d1ec","d1dc","d1cc"],["ad2c","aec","adc","acc"],["bec","bdc","bcc"],["ccc"]]),(7,[["d1d2c","d1ec","d1dc","d1cc","d1bc"],["ad2c","aec","adc","acc","abc"],["bec","bdc","bcc","bbc"],["ccc","cbc"]]),(8,[["d1d2d","d1ed","d1dd","d1cd","d1bd","d1ad"],["ad2d","aed","add","acd","abd","aad"],["bed","bdd","bcd","bbd","bad"],["ccd","cbd","cad"],["dbd","dad"]]),(9,[["d1d2e","d1ee","d1de","d1ce","d1be","d1ae"],["ad2e","aee","ade","ace","abe","aae"],["bee","bde","bce","bbe","bae"],["cce","cbe","cae"],["dbe","dae"],["eae"]])] $ do
      _ <- events1
      _ <- events2

      d1 <- holdDyn "d1" =<< events1
      d2 <- holdDyn "d2" =<< events2

      d <- flip pushDyn d1 $ \a ->
        flip pushDyn d2 $ \b ->
          flip pushDyn d1 $ \c ->
            return (a <> b <> c)

      d' <- pushDyn scanInnerDyns d >>= scanInnerDyns
      return $ current d'

  , testE' "fan-1" [(1,'a' :| ""),(2,'b' :| ""),(5,'c' :| ""),(7,'d' :| ""),(8,'e' :| "")] $ do
      e <- fmap toMap <$> events1
      let es = select (fanMap e) . Const2 <$> values

      return (mergeList es)

  , testE' "fan-2" [(1,'a' :| "e"),(2,'b' :| ""),(3,'d' :| ""),(4,'c' :| ""),(5,'c' :| ""),(6,'b' :| ""),(7,'a' :| "d"),(8,'e' :| "")] $ do
      e <- fmap toMap <$> events3
      let es = select (fanMap e) . Const2 <$> values

      return (mergeList es)

  , testE' "fan-3" [(4,'c'),(5,'c')] $ do
      f <- fanMap . fmap toMap <$> events3
      return $  select f (Const2 'c')

  , testE' "fan-4" [(1,'A')] $ do
      e <- fmap toMap <$> events1
      return $ toUpper <$> select (fanMap e) (Const2 'a')

  , testE' "fan-5" [(4,'C')] $ do
      e <- fmap toMap <$> events2
      return $ toUpper <$> select (fanMap e) (Const2 'c')

  , testE' "fan-6" [(2,"bb"),(8,"ee")] $ do
      f <- fanMap . fmap toMap <$> events1
      return $ toList <$> mergeList [ select f (Const2 'b'), select f (Const2 'b'), select f (Const2 'e'), select f (Const2 'e') ]

  , testE' "difference" [(2,"b"),(5,"c"),(8,"e")] $ do
      e1 <- events1
      e2 <- events2
      return $ e1 `difference ` e2

  , testE' "lazy-hold" [] $ do
      let lazyHold :: forall t m. (Reflex t, MonadHold t m, MonadFix m) => m (Event t ())
          lazyHold = do
            rec !b <- hold never e
                let e = never <$ switch b
            return $ void e
      lazyHold

  , testE' "now-1" [(1,"a"),(2,"b"),(5,"c"),(7,"d"),(8,"e")] $ do
      e1 <- events1
      switchHoldPromptly never . pushAlways (\a -> fmap (a <$) now) $ e1
  , testE' "now-2" [(1,())] $ do
      e1 <- events1
      let e = pushAlways (\a -> if a == "a" then now else return never) e1
      x <- accumDyn (<>) never e
      return . coincidence $ updated x
  , xfail ["spider"] $ testE "now-4" $ do
      now
  , testE "now-5" $ do
      e1 <- events1
      pure $ coincidence $ pushAlways (const now) e1
  , testE "now-6" $ do
      e1 <- events1
      n <- now
      pure $ coincidence $ pushAlways (const (pure n)) e1
  , xfail ["spider"] $ testE "now-7" $ do
      e1 <- plan [(0,"a"),(1,"b"),(3,"c")]
      n <- now
      pure $ coincidence $ pushAlways (const (pure n)) e1

  , testE' "dynamic-bind-lazy-function" [(3,"a")] $ do
      e <- plan [(1, "a")]
      eLater <- plan [(3, ())]
      dReady <- holdDyn False (True <$ e)
      dPayload <- holdDyn "payload" e
      let dJoined = dReady >>= \ready ->
            if ready then dPayload else error "dynamic-bind: function applied to unobserved value"
      return $ tag (current dJoined) eLater

  , testE' "dynamic-bind-forced-at-build" [(3,"a")] $ do
      e <- plan [(1, "a")]
      eLater <- plan [(3, ())]
      dReady <- holdDyn False (True <$ e)
      dPayload <- holdDyn "payload" e
      let dJoined = dReady >>= \ready ->
            if ready then dPayload else error "dynamic-bind: function applied to unobserved value"
      dJoined `seq` return (tag (current dJoined) eLater)

  -- An fmap'd Dynamic that is forced but never sampled or subscribed must not
  -- sample the Dynamic it maps over.
  , testB' "dynamic-fmap-dead" [(0,"ok"),(1,"ok"),(2,"x")] $ do
      e <- plan [(1, "x")]
      let divergingBehavior :: forall t. Reflex t => Event t () -> Behavior t String
          divergingBehavior _timelinePin = fix $ \bLoop -> fmap ('!':) bLoop
      let dDead = fmap (map toUpper) (unsafeDynamic (divergingBehavior (void e)) never)
      dDead `seq` hold "ok" e

  -- A joined Dynamic that is forced but never sampled or subscribed must not
  -- sample its outer or inner Dynamics.
  , testB' "dynamic-join-dead" [(0,"ok"),(1,"ok"),(2,"x")] $ do
      e <- plan [(1, "x")]
      -- A dynamic that refers to itself through 'join': a bottom placeholder
      -- that must stay inert while unobserved. The event argument only pins the
      -- timeline type.
      let selfReferentialDynamic :: forall t. Reflex t => Event t () -> Dynamic t String
          selfReferentialDynamic _timelinePin = fix $ \dLoop -> join (pure dLoop)
      selfReferentialDynamic (void e) `seq` hold "ok" e

  -- unsafeBuildDynamic's initial-value computation runs no earlier than the
  -- first observation of the Dynamic.
  , testE' "unsafeBuildDynamic-seed-timing" [(3,"b")] $ do
      e <- plan [(1, "a"), (2, "b")]
      eLater <- plan [(3, ())]
      b <- hold "0" e
      let d = unsafeBuildDynamic (sample b) never
      d `seq` return (tag (current d) eLater)

  -- buildDynamic's initial-value computation runs during the frame that
  -- builds it.
  , testE' "buildDynamic-seed-timing" [(3,"0")] $ do
      e <- plan [(1, "a"), (2, "b")]
      eLater <- plan [(3, ())]
      b <- hold "0" e
      d <- buildDynamic (sample b) never
      return (tag (current d) eLater)

  -- buildDynamic inside a recursive knot, whose initial-value computation
  -- scrutinizes a value sampled from a Dynamic bound later in the same knot.
  , testB' "buildDynamic-rec-scrutinize" [(0,5),(1,5)] $ do
      rec dScrutinized <- buildDynamic
            (do seed <- sample (current dLater)
                if seed > (0 :: Int) then return seed else return 0)
            never
          dLater <- holdDyn 5 never
      return (current dScrutinized)

  -- The lazy-hold knot with an fmap between the hold and the switch.
  , testE' "lazy-hold-fmap" [(1,()),(2,())] $ do
      let lazyHoldFmap :: forall t m. (Reflex t, MonadHold t m, MonadFix m) => m (Event t ())
          lazyHoldFmap = do
            rec !b <- hold never e
                let e = never <$ switch (fmap id b)
            return $ void e
      e0 <- plan [(1, ()), (2, ())]
      tickle <- lazyHoldFmap
      return $ leftmost [tickle, e0]

  , testE' "switch-fmap-behavior" [(1,"hia"),(2,"lob"),(3,"hic"),(4,"lod")] $ do
      e1 <- plan [(1, "a"), (2, "b"), (3, "c"), (4, "d")]
      rec bFlag <- hold True (not <$> tag bFlag e1)
      let bE = (\f -> if f then ("hi" ++) <$> e1 else ("lo" ++) <$> e1) <$> bFlag
      return (switch bE)

  , testE' "coincidence-pull-switch-height" [(1,()),(2,()),(3,()),(4,())] $ do
      tick <- plan [(1, ()), (2, ()), (3, ()), (4, ())]
      rec bFlag <- hold False (not <$> tag bFlag tick)
      let hi = leftmost [tick, tick]
          bE = (\f -> if f then hi else tick) <$> bFlag
      return $ leftmost [void (coincidence (switch bE <$ hi)), tick]
  ] where

    events1, events2, events3 ::  TestPlan t m => m (Event t String)
    events1 = plan [(1, "a"), (2, "b"), (5, "c"), (7, "d"), (8, "e")]
    events2 = plan [(1, "e"), (3, "d"), (4, "c"), (6, "b"), (7, "a")]
    events3 = liftA2 mappend events1 events2

    -- Unit-valued event occurrences on consecutive frames, for the height/teardown
    -- regression tests below (which need occurrences but not their values).
    ticks :: TestPlan t m => m (Event t ())
    ticks = plan [(1, ()), (2, ()), (3, ()), (4, ()), (5, ()), (6, ())]

    eithers ::  TestPlan t m => m (Event t (Either String String))
    eithers = plan [(1, Left "e"), (3, Left "d"), (4, Right "c"), (6, Right "b"), (7, Left "a")]


    values = "abcde"
    toMap str = Map.fromList $ map (\c -> (c, c)) str

    behavior1, behavior2 :: forall t m. TestPlan t m => m (Behavior t String)
    behavior1 =  hold "1" =<< events1
    behavior2 =  hold "2" =<< events2

    deep e = leftmost [e, e]
    leftmost2 e1 e2 = leftmost [e1, e2]
