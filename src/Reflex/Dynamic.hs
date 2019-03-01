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
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  , mapDynM
  , forDynM
  , constDyn
  , count
  , toggle
  , switchDyn
  , switchPromptlyDyn
  , tagPromptlyDyn
  , attachPromptlyDyn
  , attachPromptlyDynWith
  , attachPromptlyDynWithMaybe
  , maybeDyn
  , eitherDyn
  , factorDyn
  , scanDyn
  , scanDynMaybe
  , holdUniqDyn
  , holdUniqDynBy
  , improvingMaybe
  , foldDyn
  , foldDynM
  , foldDynMaybe
  , foldDynMaybeM
  , joinDynThroughMap
  , joinDynThroughTraversable
  , joinDynThroughDistributive
  , traceDyn
  , traceDynWith
  , splitDynPure
  , distributeMapOverDynPure
  , distributeDMapOverDynPure
  , distributeListOverDynPure
  , Demux
  , demux
  , demuxed
    -- * Miscellaneous
    -- Things that probably aren't very useful:
  , HList (..)
  , FHList (..)
  , collectDynPure
  , RebuildSortedHList (..)
  , IsHList (..)
  , AllAreFunctors (..)
  , HListPtr (..)
  , distributeFHListOverDynPure
    -- * Unsafe
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
  , getDemuxed
  , joinDyn
  , mapDyn
  , mconcatDyn
  , nubDyn
  , splitDyn
  , tagDyn
  , uniqDyn
  , uniqDynBy
  ) where

import Data.Functor.Compose
import Data.Functor.Misc
import Reflex.Class

import Control.Applicative ((<*>))
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.State
import Data.Align
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import Data.Distributive
import Data.Foldable
import Data.Functor.Product
import Data.GADT.Compare ((:~:) (..), GCompare (..), GEq (..), GOrdering (..))
import Data.Map (Map)
import Data.Maybe
import Data.Monoid hiding (Product)
import Data.These

import Debug.Trace

-- | Map a sampling function over a 'Dynamic'.
mapDynM :: forall t m a b. (Reflex t, MonadHold t m) => (forall m'. MonadSample t m' => a -> m' b) -> Dynamic t a -> m (Dynamic t b)
mapDynM f d = buildDynamic (f =<< sample (current d)) $ pushAlways f (updated d)

-- | Flipped version of 'mapDynM'
forDynM :: forall t m a b. (Reflex t, MonadHold t m) => Dynamic t a -> (forall m'. MonadSample t m' => a -> m' b) -> m (Dynamic t b)
forDynM d f = mapDynM f d

-- | Create a new 'Dynamic' that only signals changes if the values actually
-- changed.
holdUniqDyn :: (Reflex t, MonadHold t m, MonadFix m, Eq a) => Dynamic t a -> m (Dynamic t a)
holdUniqDyn = holdUniqDynBy (==)

-- | Create a new 'Dynamic' that changes only when the underlying 'Dynamic'
-- changes and the given function returns 'False' when given both the old and
-- the new values.
holdUniqDynBy :: (Reflex t, MonadHold t m, MonadFix m) => (a -> a -> Bool) -> Dynamic t a -> m (Dynamic t a)
holdUniqDynBy eq = scanDynMaybe id (\new old -> if new `eq` old then Nothing else Just new)

-- | @/Dynamic Maybe/@ that can only update from @/Nothing/@ to @/Just/@ or @/Just/@ to @/Just/@ (i.e., cannot revert to @/Nothing/@)
improvingMaybe :: (Reflex t, MonadHold t m, MonadFix m) => Dynamic t (Maybe a) -> m (Dynamic t (Maybe a))
improvingMaybe = scanDynMaybe id (\new _ -> if isJust new then Just new else Nothing)

-- | Create a 'Dynamic' that accumulates values from another 'Dynamic'.  This
-- function does not force its input 'Dynamic' until the output 'Dynamic' is
-- forced.
scanDyn :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b) -> (a -> b -> b) -> Dynamic t a -> m (Dynamic t b)
scanDyn z f = scanDynMaybe z (\a b -> Just $ f a b)

-- | Like 'scanDyn', but the the accumulator function may decline to update the
-- result 'Dynamic''s value.
scanDynMaybe :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b) -> (a -> b -> Maybe b) -> Dynamic t a -> m (Dynamic t b)
scanDynMaybe z f d = do
  rec d' <- buildDynamic (z <$> sample (current d)) $ flip push (updated d) $ \a -> do
        b <- sample $ current d'
        return $ f a b
  return d'

-- | Create a 'Dynamic' using the initial value and change it each time the
-- 'Event' occurs using a folding function on the previous value and the value
-- of the 'Event'.
foldDyn :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> b) -> b -> Event t a -> m (Dynamic t b)
foldDyn = accumDyn . flip

-- | Like 'foldDyn', but the combining function is a 'PushM' action, so it
-- can 'sample' existing 'Behaviors' and 'hold' new ones.
foldDynM :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t b) -> b -> Event t a -> m (Dynamic t b)
foldDynM = accumMDyn . flip

-- | Create a 'Dynamic' using the provided initial value and change it each time
-- the provided 'Event' occurs, using a function to combine the old value with
-- the 'Event''s value.  If the function returns 'Nothing', the value is not
-- changed; this is distinct from returning 'Just' the old value, since the
-- 'Dynamic''s 'updated' 'Event' will fire in the 'Just' case, and will not fire
-- in the 'Nothing' case.
foldDynMaybe :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> Maybe b) -> b -> Event t a -> m (Dynamic t b)
foldDynMaybe = accumMaybeDyn . flip

-- | Like 'foldDynMaybe', but the combining function is a 'PushM' action, so it
-- can 'sample' existing 'Behaviors' and 'hold' new ones.
foldDynMaybeM :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t (Maybe b)) -> b -> Event t a -> m (Dynamic t b)
foldDynMaybeM = accumMaybeMDyn . flip

-- | Create a new 'Dynamic' that counts the occurrences of the 'Event'.
count :: (Reflex t, MonadHold t m, MonadFix m, Num b) => Event t a -> m (Dynamic t b)
count e = holdDyn 0 =<< zipListWithEvent const (iterate (+1) 1) e

-- | Create a new 'Dynamic' using the initial value that flips its
-- value every time the 'Event' occurs.
toggle :: (Reflex t, MonadHold t m, MonadFix m) => Bool -> Event t a -> m (Dynamic t Bool)
toggle = foldDyn (const not)

-- | Switches to the new 'Event' whenever it receives one. Only the old event is
-- considered the moment a new one is switched in; the output event will fire at
-- that moment if only if the old event does.
--
-- Prefer this to 'switchPromptlyDyn' where possible. The lack of doing double
-- work when the outer and (new) inner fires means this imposes fewer "timing
-- requirements" and thus is far more easy to use without introducing fresh
-- failure cases. 'switchDyn' is also more performant.
switchDyn :: forall t a. Reflex t => Dynamic t (Event t a) -> Event t a
switchDyn d = switch (current d)

-- | Switches to the new 'Event' whenever it receives one.  Switching occurs
-- __before__ the inner 'Event' fires - so if the 'Dynamic' changes and both the
-- old and new inner Events fire simultaneously, the output will fire with the
-- value of the __new__ 'Event'.
--
-- Prefer 'switchDyn' to this where possible. The timing requirements that
-- switching before imposes are likely to bring down your app unless you are
-- very careful. 'switchDyn' is also more performant.
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

-- | Combine a 'Dynamic' of a 'Map' of 'Dynamic's into a 'Dynamic'
-- with the current values of the 'Dynamic's in a map.
joinDynThroughMap :: forall t k a. (Reflex t, Ord k) => Dynamic t (Map k (Dynamic t a)) -> Dynamic t (Map k a)
joinDynThroughMap = joinDyn . fmap distributeMapOverDynPure

-- | Combine a 'Dynamic' of a 'Traversable' of 'Dynamic's into a 'Dynamic'
-- with the current values of the 'Dynamic's in the functor.
joinDynThroughTraversable :: (Reflex t, Traversable f) => Dynamic t (f (Dynamic t a)) -> Dynamic t (f a)
joinDynThroughTraversable = join . fmap (unsafeViaList distributeListOverDynPure)
  where
    -- Requires 'f' to preserve number and order of elements
    unsafeViaList :: (Functor f, Traversable t) => ([a] -> f [b]) -> t a -> f (t b)
    unsafeViaList f t = unsafeReifyContents t <$> f (toList t)

    -- Requires the list to have at least as many elements as the traversable
    unsafeReifyContents :: Traversable t => t a -> [b] -> t b
    unsafeReifyContents = evalState . sequence . (f <$)
      where
       f = do
         (b:bs') <- get
         put bs'
         return b


-- | Combine a 'Dynamic' of a 'Distributive' of 'Dynamic's into a functor of 'Dynamic's.
-- The current value at each hole in the resulting functor will be the current value of that hole in the current original functor
joinDynThroughDistributive :: (Reflex t, Distributive f) => Dynamic t (f (Dynamic t a)) -> f (Dynamic t a)
joinDynThroughDistributive = fmap join . distribute

-- | Print the value of the 'Dynamic' when it is first read and on each
-- subsequent change that is observed (as 'traceEvent'), prefixed with the
-- provided string. This should /only/ be used for debugging.
--
-- Note: Just like Debug.Trace.trace, the value will only be shown if something
-- else in the system is depending on it.
traceDyn :: (Reflex t, Show a) => String -> Dynamic t a -> Dynamic t a
traceDyn s = traceDynWith $ \x -> s <> ": " <> show x

-- | Print the result of applying the provided function to the value
-- of the 'Dynamic' when it is first read and on each subsequent change
-- that is observed (as 'traceEvent'). This should /only/ be used for
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
-- Note: @/tagPromptlyDyn d e/@ differs from @/tag (current d) e/@ in the case that @/e/@ is firing
-- at the same time that @/d/@ is changing.  With @/tagPromptlyDyn d e/@, the __new__ value of @/d/@
-- will replace the value of @/e/@, whereas with @/tag (current d) e/@, the __old__ value
-- will be used, since the 'Behavior' won't be updated until the end of the frame.
-- Additionally, this means that the output 'Event' may not be used to directly change
-- the input 'Dynamic', because that would mean its value depends on itself.  __When creating__
-- __cyclic data flows, generally @/tag (current d) e/@ is preferred.__
tagPromptlyDyn :: Reflex t => Dynamic t a -> Event t b -> Event t a
tagPromptlyDyn = attachPromptlyDynWith const

-- | Attach the current value of the 'Dynamic' to the value of the
-- 'Event' each time it occurs.
--
-- Note: @/attachPromptlyDyn d/@ is not the same as @/attach (current d)/@.  See 'tagPromptlyDyn' for details.
attachPromptlyDyn :: Reflex t => Dynamic t a -> Event t b -> Event t (a, b)
attachPromptlyDyn = attachPromptlyDynWith (,)

-- | Combine the current value of the 'Dynamic' with the value of the
-- 'Event' each time it occurs.
--
-- Note: @/attachPromptlyDynWith f d/@ is not the same as @/attachWith f (current d)/@.  See 'tagPromptlyDyn' for details.
attachPromptlyDynWith :: Reflex t => (a -> b -> c) -> Dynamic t a -> Event t b -> Event t c
attachPromptlyDynWith f = attachPromptlyDynWithMaybe $ \a b -> Just $ f a b

-- | Create a new 'Event' by combining the value at each occurrence with the
-- current value of the 'Dynamic' value and possibly filtering if the combining
-- function returns 'Nothing'.
--
-- Note: @/attachPromptlyDynWithMaybe f d/@ is not the same as @/attachWithMaybe f (current d)/@.  See 'tagPromptlyDyn' for details.
attachPromptlyDynWithMaybe :: Reflex t => (a -> b -> Maybe c) -> Dynamic t a -> Event t b -> Event t c
attachPromptlyDynWithMaybe f d e =
  let e' = attach (current d) e
  in fforMaybe (align e' $ updated d) $ \case
       This (a, b) -> f a b -- Only the tagging event is firing, so use that
       These (_, b) a -> f a b -- Both events are firing, so use the newer value
       That _ -> Nothing -- The tagging event isn't firing, so don't fire

-- | Factor a @/Dynamic t (Maybe a)/@ into a @/Dynamic t (Maybe (Dynamic t a))/@,
-- such that the outer 'Dynamic' is updated only when the "Maybe"'s constructor
-- chages from 'Nothing' to 'Just' or vice-versa.  Whenever the constructor
-- becomes 'Just', an inner 'Dynamic' will be provided, whose value will track
-- the 'a' inside the 'Just'; when the constructor becomes 'Nothing', the
-- existing inner 'Dynamic' will become constant, and will not change when the
-- outer constructor changes back to 'Nothing'.
maybeDyn :: forall t a m. (Reflex t, MonadFix m, MonadHold t m) => Dynamic t (Maybe a) -> m (Dynamic t (Maybe (Dynamic t a)))
maybeDyn = fmap (fmap unpack) . eitherDyn . fmap pack
  where pack = \case
          Nothing -> Left ()
          Just a -> Right a
        unpack = \case
          Left _ -> Nothing
          Right a -> Just a

eitherDyn :: forall t a b m. (Reflex t, MonadFix m, MonadHold t m) => Dynamic t (Either a b) -> m (Dynamic t (Either (Dynamic t a) (Dynamic t b)))
eitherDyn = fmap (fmap unpack) . factorDyn . fmap eitherToDSum
  where unpack :: DSum (EitherTag a b) (Compose (Dynamic t) Identity) -> Either (Dynamic t a) (Dynamic t b)
        unpack = \case
          LeftTag :=> Compose a -> Left $ coerceDynamic a
          RightTag :=> Compose b -> Right $ coerceDynamic b

factorDyn :: forall t m k v. (Reflex t, MonadHold t m, GEq k)
          => Dynamic t (DSum k v) -> m (Dynamic t (DSum k (Compose (Dynamic t) v)))
factorDyn d = buildDynamic (sample (current d) >>= holdKey) update  where
  update :: Event t (DSum k (Compose (Dynamic t) v))
  update = flip push (updated d) $ \(newKey :=> newVal) -> do
     (oldKey :=> _) <- sample (current d)
     case newKey `geq` oldKey of
      Just Refl -> return Nothing
      Nothing -> Just <$> holdKey (newKey :=> newVal)

  holdKey (k :=> v) = do
    inner' <- filterEventKey k (updated d)
    inner <- holdDyn v inner'
    return $ k :=> Compose inner
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

deriving instance Eq (HListPtr l a)
deriving instance Ord (HListPtr l a)

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

-- | Flipped version of 'mapDyn'.
{-# DEPRECATED forDyn "Use 'return . ffor a' instead of 'forDyn a'; consider eliminating monadic style" #-}
forDyn :: (Reflex t, Monad m) => Dynamic t a -> (a -> b) -> m (Dynamic t b)
forDyn a = return . ffor a

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
#ifdef USE_TEMPLATE_HASKELL
{-# ANN apDyn "HLint: ignore Use fmap" #-}
#endif
apDyn :: forall t m a b. (Reflex t, Monad m)
      => m (Dynamic t (a -> b))
      -> Dynamic t a
      -> m (Dynamic t b)
apDyn m a = fmap (<*> a) m

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

-- | WARNING: This function is only pure if @a@'s 'Eq' instance tests
-- representational equality.  Use 'holdUniqDyn' instead, which is pure in all
-- circumstances.  Also, note that, unlike 'nub', this function does not prevent
-- all recurrences of a value, only consecutive recurrences.
{-# DEPRECATED nubDyn "Use 'holdUniqDyn' instead; note that it returns a MonadHold action rather than a pure Dynamic" #-}
nubDyn :: (Reflex t, Eq a) => Dynamic t a -> Dynamic t a
nubDyn = uniqDyn

-- | WARNING: This function is only pure if @a@'s 'Eq' instance tests
-- representational equality.  Use 'holdUniqDyn' instead, which is pure in all
-- circumstances.
{-# DEPRECATED uniqDyn "Use 'holdUniqDyn' instead; note that it returns a MonadHold action rather than a pure Dynamic" #-}
uniqDyn :: (Reflex t, Eq a) => Dynamic t a -> Dynamic t a
uniqDyn = uniqDynBy (==)

-- | WARNING: This function is impure.  Use 'holdUniqDynBy' instead.
{-# DEPRECATED uniqDynBy "Use 'holdUniqDynBy' instead; note that it returns a MonadHold action rather than a pure Dynamic" #-}
uniqDynBy :: Reflex t => (a -> a -> Bool) -> Dynamic t a -> Dynamic t a
uniqDynBy eq d =
  let e' = attachWithMaybe (\x x' -> if x' `eq` x then Nothing else Just x') (current d) (updated d)
  in unsafeDynamic (current d) e'
