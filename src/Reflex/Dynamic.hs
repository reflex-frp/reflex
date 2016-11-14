{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
-- | This module contains various functions for working with 'Dynamic' values.
-- 'Dynamic' and its primitives have been moved to the 'Reflex' class.
module Reflex.Dynamic
  ( -- * Basics
    Dynamic -- Abstract so we can preserve the law that the current value is always equal to the most recent update
  , current
  , updated
  , holdDyn
  , constDyn
  , uniqDyn
  , uniqDynBy
  , count
  , toggle
  , switchPromptlyDyn
  , tagPromptlyDyn
  , attachPromptlyDyn
  , attachPromptlyDynWith
  , attachPromptlyDynWithMaybe
  , foldDyn
  , foldDynM
  , foldDynMaybe
  , foldDynMaybeM
  , joinDynThroughMap
  , traceDyn
  , traceDynWith
  , splitDynPure
  , distributeMapOverDynPure
  , distributeDMapOverDynPure
  , distributeListOverDynPure
  , Demux
  , demux
  , demuxed
    -- Things that probably aren't very useful:
  , HList (..)
  , FHList (..)
  , collectDynPure
  , RebuildSortedHList (..)
  , IsHList (..)
  , AllAreFunctors (..)
  , HListPtr (..)
  , distributeFHListOverDynPure
    -- Unsafe
  , unsafeDynamic
    -- * Deprecated functions
  , apDyn
  , attachDyn
  , attachDynWith
  , attachDynWithMaybe
  , collectDyn
  , combineDyn
  , distributeDMapOverDyn
  , distributeFHListOverDyn
  , forDyn
  , forDynM
  , getDemuxed
  , joinDyn
  , mapDyn
  , mapDynM
  , mconcatDyn
  , nubDyn
  , splitDyn
  , tagDyn
  ) where

import Prelude hiding (mapM, mapM_)

import Data.Functor.Misc
import Reflex.Class

import Control.Applicative ((<*>))
import Control.Monad hiding (forM, forM_, mapM, mapM_)
import Control.Monad.Fix
import Control.Monad.Identity hiding (forM, forM_, mapM, mapM_)
import Data.Align
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare ((:~:) (..), GCompare (..), GEq (..), GOrdering (..))
import Data.Map (Map)
import Data.Monoid
import Data.These

import Debug.Trace

-- | Create a new 'Dynamic' that only signals changes if the values actually
-- changed.
uniqDyn :: (Reflex t, Eq a) => Dynamic t a -> Dynamic t a
uniqDyn = uniqDynBy (==)

-- | Create a new 'Dynamic' that changes only when the underlying 'Dynamic'
-- changes and the given function returns 'False' when given both the old and
-- the new values.
uniqDynBy :: Reflex t => (a -> a -> Bool) -> Dynamic t a -> Dynamic t a
uniqDynBy eq d =
  let e' = attachWithMaybe (\x x' -> if x' `eq` x then Nothing else Just x') (current d) (updated d)
  in unsafeDynamic (current d) e'

-- | Create a 'Dynamic' using the initial value and change it each time the
-- 'Event' occurs using a folding function on the previous value and the value
-- of the 'Event'.
foldDyn :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> b) -> b -> Event t a -> m (Dynamic t b)
foldDyn = accum . flip

-- | Like 'foldDyn', but the combining function is a 'PushM' action, so it
-- can 'sample' existing 'Behaviors' and 'hold' new ones.
foldDynM :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t b) -> b -> Event t a -> m (Dynamic t b)
foldDynM = accumM . flip

-- | Create a 'Dynamic' using the provided initial value and change it each time
-- the provided 'Event' occurs, using a function to combine the old value with
-- the 'Event''s value.  If the function returns 'Nothing', the value is not
-- changed; this is distinct from returning 'Just' the old value, since the
-- 'Dynamic''s 'updated' 'Event' will fire in the 'Just' case, and will not fire
-- in the 'Nothing' case.
foldDynMaybe :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> Maybe b) -> b -> Event t a -> m (Dynamic t b)
foldDynMaybe = accumMaybe . flip

-- | Like 'foldDynMaybe', but the combining function is a 'PushM' action, so it
-- can 'sample' existing 'Behaviors' and 'hold' new ones.
foldDynMaybeM :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t (Maybe b)) -> b -> Event t a -> m (Dynamic t b)
foldDynMaybeM = accumMaybeM . flip

-- | Create a new 'Dynamic' that counts the occurences of the 'Event'.
count :: (Reflex t, MonadHold t m, MonadFix m, Num b) => Event t a -> m (Dynamic t b)
count e = holdDyn 0 =<< zipListWithEvent const (iterate (+1) 1) e

-- | Create a new 'Dynamic' using the initial value that flips its
-- value every time the 'Event' occurs.
toggle :: (Reflex t, MonadHold t m, MonadFix m) => Bool -> Event t a -> m (Dynamic t Bool)
toggle = foldDyn (const not)

-- | Switches to the new 'Event' whenever it receives one.  Switching occurs
-- *before* the inner 'Event' fires - so if the 'Dynamic' changes and both the
-- old and new inner Events fire simultaneously, the output will fire with the
-- value of the *new* 'Event'.
switchPromptlyDyn :: forall t a. Reflex t => Dynamic t (Event t a) -> Event t a
switchPromptlyDyn de =
  let eLag = switch $ current de
      eCoincidences = coincidence $ updated de
  in leftmost [eCoincidences, eLag]

-- | Split a 'Dynamic' pair into a pair of 'Dynamic's
splitDynPure :: Reflex t => Dynamic t (a, b) -> (Dynamic t a, Dynamic t b)
splitDynPure d = (fmap fst d, fmap snd d)

-- | Convert a 'Map' with 'Dynamic' elements into a 'Dynamic' of a 'Map' with
-- non-'Dynamic' elements.
distributeMapOverDynPure :: (Reflex t, Ord k) => Map k (Dynamic t v) -> Dynamic t (Map k v)
distributeMapOverDynPure = fmap dmapToMap . distributeDMapOverDynPure . mapWithFunctorToDMap

-- | Convert a list with 'Dynamic' elements into a 'Dynamic' of a list with
-- non-'Dynamic' elements, preserving the order of the elements.
distributeListOverDynPure :: Reflex t => [Dynamic t v] -> Dynamic t [v]
distributeListOverDynPure =
  fmap (map fromDSum . DMap.toAscList) .
  distributeDMapOverDynPure .
  DMap.fromDistinctAscList .
  zipWith toDSum [0..]
  where
    toDSum :: Int -> Dynamic t a -> DSum (Const2 Int a) (Dynamic t)
    toDSum k v = Const2 k :=> v
    fromDSum :: DSum (Const2 Int a) Identity -> a
    fromDSum (Const2 _ :=> Identity v) = v

--TODO: Generalize this to functors other than Maps
-- | Combine a 'Dynamic' of a 'Map' of 'Dynamic's into a 'Dynamic'
-- with the current values of the 'Dynamic's in a map.
joinDynThroughMap :: forall t k a. (Reflex t, Ord k) => Dynamic t (Map k (Dynamic t a)) -> Dynamic t (Map k a)
joinDynThroughMap = joinDyn . fmap distributeMapOverDynPure

-- | Print the value of the 'Dynamic' on each change and prefix it
-- with the provided string. This should /only/ be used for debugging.
--
-- Note: Just like Debug.Trace.trace, the value will only be shown if something
-- else in the system is depending on it.
traceDyn :: (Reflex t, Show a) => String -> Dynamic t a -> Dynamic t a
traceDyn s = traceDynWith $ \x -> s <> ": " <> show x

-- | Print the result of applying the provided function to the value
-- of the 'Dynamic' on each change. This should /only/ be used for
-- debugging.
--
-- Note: Just like Debug.Trace.trace, the value will only be shown if something
-- else in the system is depending on it.
traceDynWith :: Reflex t => (a -> String) -> Dynamic t a -> Dynamic t a
traceDynWith f d =
  let e' = traceEventWith f $ updated d
      getV0 = do
        x <- sample $ current d
        trace (f x) $ return x
  in unsafeBuildDynamic getV0 e'

-- | Replace the value of the 'Event' with the current value of the 'Dynamic'
-- each time the 'Event' occurs.
--
-- Note: `tagPromptlyDyn d e` differs from `tag (current d) e` in the case that `e` is firing
-- at the same time that `d` is changing.  With `tagPromptlyDyn d e`, the *new* value of `d`
-- will replace the value of `e`, whereas with `tag (current d) e`, the *old* value
-- will be used, since the 'Behavior' won't be updated until the end of the frame.
-- Additionally, this means that the output 'Event' may not be used to directly change
-- the input 'Dynamic', because that would mean its value depends on itself.  When creating
-- cyclic data flows, generally `tag (current d) e` is preferred.
tagPromptlyDyn :: Reflex t => Dynamic t a -> Event t b -> Event t a
tagPromptlyDyn = attachPromptlyDynWith const

-- | Attach the current value of the 'Dynamic' to the value of the
-- 'Event' each time it occurs.
--
-- Note: `attachPromptlyDyn d` is not the same as `attach (current d)`.  See 'tagPromptlyDyn' for details.
attachPromptlyDyn :: Reflex t => Dynamic t a -> Event t b -> Event t (a, b)
attachPromptlyDyn = attachPromptlyDynWith (,)

-- | Combine the current value of the 'Dynamic' with the value of the
-- 'Event' each time it occurs.
--
-- Note: `attachPromptlyDynWith f d` is not the same as `attachWith f (current d)`.  See 'tagPromptlyDyn' for details.
attachPromptlyDynWith :: Reflex t => (a -> b -> c) -> Dynamic t a -> Event t b -> Event t c
attachPromptlyDynWith f = attachPromptlyDynWithMaybe $ \a b -> Just $ f a b

-- | Create a new 'Event' by combining the value at each occurence with the
-- current value of the 'Dynamic' value and possibly filtering if the combining
-- function returns 'Nothing'.
--
-- Note: `attachPromptlyDynWithMaybe f d` is not the same as `attachWithMaybe f
-- (current d)`.  See 'tagPromptlyDyn' for details.
attachPromptlyDynWithMaybe :: Reflex t => (a -> b -> Maybe c) -> Dynamic t a -> Event t b -> Event t c
attachPromptlyDynWithMaybe f d e =
  let e' = attach (current d) e
  in fforMaybe (align e' $ updated d) $ \case
       This (a, b) -> f a b -- Only the tagging event is firing, so use that
       These (_, b) a -> f a b -- Both events are firing, so use the newer value
       That _ -> Nothing -- The tagging event isn't firing, so don't fire

--------------------------------------------------------------------------------
-- Demux
--------------------------------------------------------------------------------

-- | Represents a time changing value together with an 'EventSelector' that can
-- efficiently detect when the underlying 'Dynamic' has a particular value.
-- This is useful for representing data like the current selection of a long
-- list.
--
-- Semantically,
--
-- > demuxed (demux d) k === fmap (== k) d
--
-- However, the when getDemuxed is used multiple times, the complexity is only
-- /O(log(n))/, rather than /O(n)/ for fmap.
data Demux t k = Demux { demuxValue :: Behavior t k
                       , demuxSelector :: EventSelector t (Const2 k Bool)
                       }

-- | Demultiplex an input value to a 'Demux' with many outputs.  At any given
-- time, whichever output is indicated by the given 'Dynamic' will be 'True'.
demux :: (Reflex t, Ord k) => Dynamic t k -> Demux t k
demux k = Demux (current k)
                (fan $ attachWith (\k0 k1 -> if k0 == k1
                                                then DMap.empty
                                                else DMap.fromList [Const2 k0 :=> Identity False,
                                                                    Const2 k1 :=> Identity True])
                                  (current k) (updated k))

-- | Select a particular output of the 'Demux'; this is equivalent to (but much
-- faster than) mapping over the original 'Dynamic' and checking whether it is
-- equal to the given key.
demuxed :: (Reflex t, Eq k) => Demux t k -> k -> Dynamic t Bool
demuxed d k =
  let e = select (demuxSelector d) (Const2 k)
  in unsafeBuildDynamic (fmap (==k) $ sample $ demuxValue d) e

--------------------------------------------------------------------------------
-- collectDyn
--------------------------------------------------------------------------------

--TODO: This whole section is badly in need of cleanup

-- | A heterogeneous list whose type and length are fixed statically.  This is
-- reproduced from the 'HList' package due to integration issues, and because
-- very little other functionality from that library is needed.
data HList (l::[*]) where
  HNil  :: HList '[]
  HCons :: e -> HList l -> HList (e ': l)

infixr 2 `HCons`

type family HRevApp (l1 :: [k]) (l2 :: [k]) :: [k]
type instance HRevApp '[] l = l
type instance HRevApp (e ': l) l' = HRevApp l (e ': l')

hRevApp :: HList l1 -> HList l2 -> HList (HRevApp l1 l2)
hRevApp HNil l = l
hRevApp (HCons x l) l' = hRevApp l (HCons x l')

hReverse :: HList l -> HList (HRevApp l '[])
hReverse l = hRevApp l HNil

hBuild :: (HBuild' '[] r) => r
hBuild =  hBuild' HNil

class HBuild' l r where
    hBuild' :: HList l -> r

instance (l' ~ HRevApp l '[])
      => HBuild' l (HList l') where
  hBuild' = hReverse

instance HBuild' (a ': l) r
      => HBuild' l (a->r) where
  hBuild' l x = hBuild' (HCons x l)

-- | Like 'HList', but with a functor wrapping each element.
data FHList f l where
  FHNil :: FHList f '[]
  FHCons :: f e -> FHList f l -> FHList f (e ': l)

instance GEq (HListPtr l) where
  HHeadPtr `geq` HHeadPtr = Just Refl
  HHeadPtr `geq` HTailPtr _ = Nothing
  HTailPtr _ `geq` HHeadPtr = Nothing
  HTailPtr a `geq` HTailPtr b = a `geq` b

instance GCompare (HListPtr l) where -- Warning: This ordering can't change, dmapTo*HList will break
  HHeadPtr `gcompare` HHeadPtr = GEQ
  HHeadPtr `gcompare` HTailPtr _ = GLT
  HTailPtr _ `gcompare` HHeadPtr = GGT
  HTailPtr a `gcompare` HTailPtr b = a `gcompare` b

-- | A typed index into a typed heterogeneous list.
data HListPtr l a where
  HHeadPtr :: HListPtr (h ': t) h
  HTailPtr :: HListPtr t a -> HListPtr (h ': t) a

fhlistToDMap :: forall (f :: * -> *) l. FHList f l -> DMap (HListPtr l) f
fhlistToDMap = DMap.fromList . go
  where go :: forall l'. FHList f l' -> [DSum (HListPtr l') f]
        go = \case
          FHNil -> []
          FHCons h t -> (HHeadPtr :=> h) : map (\(p :=> v) -> HTailPtr p :=> v) (go t)

-- | This class allows 'HList's and 'FHlist's to be built from regular lists;
-- they must be contiguous and sorted.
class RebuildSortedHList l where
  rebuildSortedFHList :: [DSum (HListPtr l) f] -> FHList f l
  rebuildSortedHList :: [DSum (HListPtr l) Identity] -> HList l

instance RebuildSortedHList '[] where
  rebuildSortedFHList l = case l of
    [] -> FHNil
    _ : _ -> error "rebuildSortedFHList{'[]}: empty list expected"
  rebuildSortedHList l = case l of
    [] -> HNil
    _ : _ -> error "rebuildSortedHList{'[]}: empty list expected"

instance RebuildSortedHList t => RebuildSortedHList (h ': t) where
  rebuildSortedFHList l = case l of
    ((HHeadPtr :=> h) : t) -> FHCons h . rebuildSortedFHList . map (\(HTailPtr p :=> v) -> p :=> v) $ t
    _ -> error "rebuildSortedFHList{h':t}: non-empty list with HHeadPtr expected"
  rebuildSortedHList l = case l of
    ((HHeadPtr :=> Identity h) : t) -> HCons h . rebuildSortedHList . map (\(HTailPtr p :=> v) -> p :=> v) $ t
    _ -> error "rebuildSortedHList{h':t}: non-empty list with HHeadPtr expected"

dmapToHList :: forall l. RebuildSortedHList l => DMap (HListPtr l) Identity -> HList l
dmapToHList = rebuildSortedHList . DMap.toList

-- | Collect a hetereogeneous list whose elements are all 'Dynamic's into a
-- single 'Dynamic' whose value represents the current values of all of the
-- input 'Dynamic's.
distributeFHListOverDynPure :: (Reflex t, RebuildSortedHList l) => FHList (Dynamic t) l -> Dynamic t (HList l)
distributeFHListOverDynPure l = fmap dmapToHList $ distributeDMapOverDynPure $ fhlistToDMap l

-- | Indicates that all elements in a type-level list are applications of the
-- same functor.
class AllAreFunctors (f :: a -> *) (l :: [a]) where
  type FunctorList f l :: [*]
  toFHList :: HList (FunctorList f l) -> FHList f l
  fromFHList :: FHList f l -> HList (FunctorList f l)

instance AllAreFunctors f '[] where
  type FunctorList f '[] = '[]
  toFHList l = case l of
    HNil -> FHNil
#if !defined(__GLASGOW_HASKELL__) || __GLASGOW_HASKELL__ < 800
    _ -> error "toFHList: impossible" -- Otherwise, GHC complains of a non-exhaustive pattern match; see https://ghc.haskell.org/trac/ghc/ticket/4139
#endif
  fromFHList FHNil = HNil

instance AllAreFunctors f t => AllAreFunctors f (h ': t) where
  type FunctorList f (h ': t) = f h ': FunctorList f t
  toFHList l = case l of
    a `HCons` b -> a `FHCons` toFHList b
#if !defined(__GLASGOW_HASKELL__) || __GLASGOW_HASKELL__ < 800
    _ -> error "toFHList: impossible" -- Otherwise, GHC complains of a non-exhaustive pattern match; see https://ghc.haskell.org/trac/ghc/ticket/4139
#endif
  fromFHList (a `FHCons` b) = a `HCons` fromFHList b

-- | Convert a datastructure whose constituent parts are all 'Dynamic's into a
-- single 'Dynamic' whose value represents all the current values of the input's
-- consitutent 'Dynamic's.
collectDynPure :: ( RebuildSortedHList (HListElems b)
                  , IsHList a, IsHList b
                  , AllAreFunctors (Dynamic t) (HListElems b)
                  , Reflex t
                  , HListElems a ~ FunctorList (Dynamic t) (HListElems b)
                  ) => a -> Dynamic t b
collectDynPure ds = fmap fromHList $ distributeFHListOverDynPure $ toFHList $ toHList ds

-- | Poor man's 'Generic's for product types only.
class IsHList a where
  type HListElems a :: [*]
  toHList :: a -> HList (HListElems a)
  fromHList :: HList (HListElems a) -> a

instance IsHList (a, b) where
  type HListElems (a, b) = [a, b]
  toHList (a, b) = hBuild a b
  fromHList l = case l of
    a `HCons` b `HCons` HNil -> (a, b)
#if !defined(__GLASGOW_HASKELL__) || __GLASGOW_HASKELL__ < 800
    _ -> error "fromHList: impossible" -- Otherwise, GHC complains of a non-exhaustive pattern match; see https://ghc.haskell.org/trac/ghc/ticket/4139
#endif

instance IsHList (a, b, c, d) where
  type HListElems (a, b, c, d) = [a, b, c, d]
  toHList (a, b, c, d) = hBuild a b c d
  fromHList l = case l of
    a `HCons` b `HCons` c `HCons` d `HCons` HNil -> (a, b, c, d)
#if !defined(__GLASGOW_HASKELL__) || __GLASGOW_HASKELL__ < 800
    _ -> error "fromHList: impossible" -- Otherwise, GHC complains of a non-exhaustive pattern match; see https://ghc.haskell.org/trac/ghc/ticket/4139
#endif

instance IsHList (a, b, c, d, e, f) where
  type HListElems (a, b, c, d, e, f) = [a, b, c, d, e, f]
  toHList (a, b, c, d, e, f) = hBuild a b c d e f
  fromHList l = case l of
    a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` HNil -> (a, b, c, d, e, f)
#if !defined(__GLASGOW_HASKELL__) || __GLASGOW_HASKELL__ < 800
    _ -> error "fromHList: impossible" -- Otherwise, GHC complains of a non-exhaustive pattern match; see https://ghc.haskell.org/trac/ghc/ticket/4139
#endif

--------------------------------------------------------------------------------
-- Deprecated functions
--------------------------------------------------------------------------------

-- | Map a function over a 'Dynamic'.
{-# DEPRECATED mapDyn "Use 'return . fmap f' instead of 'mapDyn f'; consider eliminating monadic style" #-}
mapDyn :: (Reflex t, Monad m) => (a -> b) -> Dynamic t a -> m (Dynamic t b)
mapDyn f = return . fmap f

-- | Map a sampling function over a 'Dynamic'.  The sampling function will use the input 'Dynamic''s 'current' 'Behavior' until the first time its 'updated' 'Event' fires, at which point it will be invoked only once each time the 'Event' fires.
{-# DEPRECATED mapDynM "Consider using the Monad instance for Dynamic instead." #-}
mapDynM :: forall t m a b. (Reflex t, MonadHold t m) => (forall m'. MonadSample t m' => a -> m' b) -> Dynamic t a -> m (Dynamic t b)
mapDynM f d = do
  let e' = push (fmap Just . f :: a -> PushM t (Maybe b)) $ updated d
      eb' = fmap constant e'
      v0 = pull $ f =<< sample (current d)
  bb' :: Behavior t (Behavior t b) <- hold v0 eb'
  let b' = pull $ sample =<< sample bb'
  return $ unsafeDynamic b' e'

-- | Flipped version of 'mapDyn'.
{-# DEPRECATED forDyn "Use 'return . ffor a' instead of 'forDyn a'; consider eliminating monadic style" #-}
forDyn :: (Reflex t, Monad m) => Dynamic t a -> (a -> b) -> m (Dynamic t b)
forDyn a = return . ffor a

-- | Flipped version of 'mapDynM'
{-# DEPRECATED forDynM "Consider using the Monad instance for Dynamic instead." #-}
forDynM :: forall t m a b. (Reflex t, MonadHold t m) => Dynamic t a -> (forall m'. MonadSample t m' => a -> m' b) -> m (Dynamic t b)
forDynM d f = mapDynM f d

-- | Split the 'Dynamic' into two 'Dynamic's, each taking the respective value
-- of the tuple.
{-# DEPRECATED splitDyn "Use 'return . splitDynPure' instead; consider eliminating monadic style" #-}
splitDyn :: (Reflex t, Monad m) => Dynamic t (a, b) -> m (Dynamic t a, Dynamic t b)
splitDyn = return . splitDynPure

-- | Merge the 'Dynamic' values using their 'Monoid' instance.
{-# DEPRECATED mconcatDyn "Use 'return . mconcat' instead; consider eliminating monadic style" #-}
mconcatDyn :: forall t m a. (Reflex t, Monad m, Monoid a) => [Dynamic t a] -> m (Dynamic t a)
mconcatDyn = return . mconcat

-- | This function no longer needs to be monadic; see 'distributeMapOverDynPure'.
{-# DEPRECATED distributeDMapOverDyn "Use 'return . distributeDMapOverDynPure' instead; consider eliminating monadic style" #-}
distributeDMapOverDyn :: (Reflex t, Monad m, GCompare k) => DMap k (Dynamic t) -> m (Dynamic t (DMap k Identity))
distributeDMapOverDyn = return . distributeDMapOverDynPure

-- | Merge two 'Dynamic's into a new one using the provided function. The new
-- 'Dynamic' changes its value each time one of the original 'Dynamic's changes
-- its value.
{-# DEPRECATED combineDyn "Use 'return (zipDynWith f a b)' instead of 'combineDyn f a b'; consider eliminating monadic style" #-}
combineDyn :: forall t m a b c. (Reflex t, Monad m) => (a -> b -> c) -> Dynamic t a -> Dynamic t b -> m (Dynamic t c)
combineDyn f a b = return $ zipDynWith f a b

-- | A psuedo applicative version of ap for 'Dynamic'. Example useage:
--
-- > do
-- >    person <- Person `mapDyn` dynFirstName
-- >                     `apDyn` dynListName
-- >                     `apDyn` dynAge
-- >                     `apDyn` dynAddress
{-# DEPRECATED apDyn "Use 'ffor m (<*> a)' instead of 'apDyn m a'; consider eliminating monadic style, since Dynamics are now Applicative and can be used with applicative style directly" #-}
{-# ANN apDyn "HLint: ignore Use fmap" #-}
apDyn :: forall t m a b. (Reflex t, Monad m)
      => m (Dynamic t (a -> b))
      -> Dynamic t a
      -> m (Dynamic t b)
apDyn m a = liftM (<*> a) m

--TODO: The pattern of using hold (sample b0) can be reused in various places as a safe way of building certain kinds of Dynamics; see if we can factor this out
-- | This function no longer needs to be monadic, so it has been replaced by
-- 'demuxed', which is pure.
{-# DEPRECATED getDemuxed "Use 'return . demuxed d' instead of 'getDemuxed d'; consider eliminating monadic style" #-}
getDemuxed :: (Reflex t, Monad m, Eq k) => Demux t k -> k -> m (Dynamic t Bool)
getDemuxed d = return . demuxed d

-- | This function no longer needs to be monadic, so it has been replaced by
-- 'distributeFHListOverDynPure', which is pure.
{-# DEPRECATED distributeFHListOverDyn "Use 'return . distributeFHListOverDynPure' instead; consider eliminating monadic style" #-}
distributeFHListOverDyn :: forall t m l. (Reflex t, Monad m, RebuildSortedHList l) => FHList (Dynamic t) l -> m (Dynamic t (HList l))
distributeFHListOverDyn = return . distributeFHListOverDynPure

-- | This function no longer needs to be monadic, so it has been replaced by
-- 'collectDynPure', which is pure.
{-# DEPRECATED collectDyn "Use 'return . collectDynPure' instead; consider eliminating monadic style" #-}
collectDyn :: ( RebuildSortedHList (HListElems b)
              , IsHList a, IsHList b
              , AllAreFunctors (Dynamic t) (HListElems b)
              , Reflex t, Monad m
              , HListElems a ~ FunctorList (Dynamic t) (HListElems b)
              ) => a -> m (Dynamic t b)
collectDyn = return . collectDynPure

-- | This function has been renamed to 'tagPromptlyDyn' to clarify its
-- semantics.
{-# DEPRECATED tagDyn "Use 'tagPromptlyDyn' instead" #-}
tagDyn :: Reflex t => Dynamic t a -> Event t b -> Event t a
tagDyn = tagPromptlyDyn

-- | This function has been renamed to 'attachPromptlyDyn' to clarify its
-- semantics.
{-# DEPRECATED attachDyn "Use 'attachPromptlyDyn' instead" #-}
attachDyn :: Reflex t => Dynamic t a -> Event t b -> Event t (a, b)
attachDyn = attachPromptlyDyn

-- | This function has been renamed to 'attachPromptlyDynWith' to clarify its
-- semantics.
{-# DEPRECATED attachDynWith "Use 'attachPromptlyDynWith' instead" #-}
attachDynWith :: Reflex t => (a -> b -> c) -> Dynamic t a -> Event t b -> Event t c
attachDynWith = attachPromptlyDynWith

-- | This function has been renamed to 'attachPromptlyDynWithMaybe' to clarify
-- its semantics.
{-# DEPRECATED attachDynWithMaybe "Use 'attachPromptlyDynWithMaybe' instead" #-}
attachDynWithMaybe :: Reflex t => (a -> b -> Maybe c) -> Dynamic t a -> Event t b -> Event t c
attachDynWithMaybe = attachPromptlyDynWithMaybe

-- | Combine an inner and outer 'Dynamic' such that the resulting 'Dynamic''s
-- current value will always be equal to the current value's current value, and
-- will change whenever either the inner or the outer (or both) values change.
{-# DEPRECATED joinDyn "Use 'join' instead" #-}
joinDyn :: Reflex t => Dynamic t (Dynamic t a) -> Dynamic t a
joinDyn = join

-- | 'nubDyn''s behavior is not quite analogous to 'nub''s behavior, so it has
-- been renamed to 'uniqDyn'
{-# DEPRECATED nubDyn "Use 'uniqDyn' instead" #-}
nubDyn :: (Reflex t, Eq a) => Dynamic t a -> Dynamic t a
nubDyn = uniqDyn
