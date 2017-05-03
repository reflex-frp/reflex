{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module           : Generics.SOP.DMapUtilities
Description      : Utilities for converting between the NS/NP types of generics-sop and Dependent Maps.
-}

module Generics.SOP.DMapUtilities
  (
    -- * Type Functions
    FunctorWrapTypeList
  , FunctorWrapTypeListOfLists

    -- * Types
  , TypeListTag

    -- * Conversions
    -- ** 'NP' \<-\> 'DM.DMap'
  , npToDMap
  , dMapToNP
    -- ** 'NS' \<-\> 'DSum'
  , nsToDSum
  , dSumToNS

  -- * Functor wrapping/unwrapping utilities for 'NP'
  , npUnCompose
  , nsOfnpUnCompose
  , npReCompose
  , nsOfnpReCompose

  -- * Utilities
  , npSequenceViaDMap

  -- * Proofs
  , functorWrappedSListIsSList
  )where

import Generics.SOP ((:.:) (Comp), K (..), NP (..), NS (..), Proxy (..), SList (..), SListI (..), SListI2, fn,
                     hcollapse, hmap, unComp)
import Generics.SOP.Dict (Dict (Dict), withDict)
import Generics.SOP.NP (sequence'_NP)
import Generics.SOP.NS (ap_NS)

import qualified Data.Dependent.Map as DM
import Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.Dependent.Sum as DS

import Data.GADT.Compare ((:~:) (..), GCompare (..), GEq (..), GOrdering (..))

import Data.Functor.Identity (Identity (runIdentity))

-- |A Tag type for making a 'DM.DMap' keyed by a type-level list
data TypeListTag (xs :: [k]) (x :: k) where -- x is in xs
  TLHead :: TypeListTag (x ': xs) x          -- x begins xs
  TLTail :: TypeListTag xs x -> TypeListTag (y ': xs) x -- given that x is in xs, x is also in (y ': xs)

instance GEq (TypeListTag xs) where
  geq TLHead TLHead = Just Refl
  geq (TLTail x) (TLTail y) = geq x y
  geq _ _  = Nothing

instance GCompare (TypeListTag xs) where
  gcompare TLHead TLHead = GEQ
  gcompare TLHead (TLTail _) = GLT
  gcompare (TLTail _) TLHead = GGT
  gcompare (TLTail x) (TLTail y) = gcompare x y

-- |Convert an 'NP' indexed by typelist xs into a 'DM.DMap' indexed by 'TypeListTag' xs
npToDMap :: NP f xs -> DM.DMap (TypeListTag xs) f
npToDMap Nil = DM.empty
npToDMap (fx :* npTail) = DM.insert TLHead fx $ DM.mapKeysMonotonic TLTail $ npToDMap npTail

-- |Convert a 'DM.DMap' indexed by 'TypeListTag' xs to an 'NP'
-- |NB: This can fail since there is no guarantee that the 'DM.DMap' has entries for every tag. Hence it returns a 'Maybe'
dMapToNP :: forall xs f. SListI xs => DM.DMap (TypeListTag xs) f -> Maybe (NP f xs)
dMapToNP dm = sequence'_NP $ hmap (Comp . flip DM.lookup dm) makeTypeListTagNP

-- |Convert a 'NS' indexed by a typelist xs to a 'DS.DSum' indexed by 'TypeListTag' xs
nsToDSum :: SListI xs => NS f xs -> DS.DSum (TypeListTag xs) f
nsToDSum = hcollapse . ap_NS (hmap (\tag -> (fn $ \val -> K (tag :=> val))) makeTypeListTagNP)
{-  let nsFToNSDSum::SListI xs=>NS f xs -> NS (K (DS.DSum (TypeListTag xs) f)) xs
      nsFToNSDSum ns' = ap_NS (tagsToFs makeTypeListTagNP) ns'
      tagsToFs::SListI xs=>NP (TypeListTag xs) xs -> NP (f -.-> K (DS.DSum (TypeListTag xs) f)) xs
      tagsToFs = hmap (\tag -> (Fn $ \val -> K (tag :=> val)))
  in hcollapse . nsFToNSDSum
-}

-- |Convert a 'DS.DSum' indexed by 'TypeListTag' xs into a 'NS' indexed by xs
dSumToNS :: SListI xs => DS.DSum (TypeListTag xs) f -> NS f xs
dSumToNS (tag :=> fa) = go tag fa
  where
    go::TypeListTag ys y -> f y -> NS f ys
    go TLHead fy = Z fy
    go (TLTail nextTag) fy = S $ go nextTag fy

-- | Produce an NP of TypeListTags matching the type-list of the NP
makeTypeListTagNP :: SListI xs => NP (TypeListTag xs) xs
makeTypeListTagNP = go sList
  where
    go :: forall ys. SListI ys => SList ys -> NP (TypeListTag ys) ys
    go SNil = Nil
    go SCons = TLHead :* hmap TLTail (go sList)

-- these are here to allow moving functors in and out of typelists
type family FunctorWrapTypeList (f :: * -> *) (xs :: [*]) :: [*] where
  FunctorWrapTypeList f '[] = '[]
  FunctorWrapTypeList f (x ': xs) = f x ': FunctorWrapTypeList f xs

type family FunctorWrapTypeListOfLists (f :: * -> *) (xss :: [[*]]) :: [[*]] where
  FunctorWrapTypeListOfLists f '[] = '[]
  FunctorWrapTypeListOfLists f (xs ': xsTail) = FunctorWrapTypeList f xs ': FunctorWrapTypeListOfLists f xsTail

-- | Transform a type-list indexed product of composed functorial values into a type-list indexed product of functorial values where the inner part of the functor
-- composition has been moved to the type-list.  The values in the product remain the same (up to types representing composition of the functors). E.g.,
--
-- > (f :.: g) 2 :* (f :.: g) 3.0 :* 'Nil :: NP (f :.: g) '[Int,Double] -> f (g 2) :* f (g 3.0) :* 'Nil :: NP f '[g Int, g Double]
npUnCompose :: forall f g xs. SListI xs => NP (f :.: g) xs -> NP f (FunctorWrapTypeList g xs)
npUnCompose = go
  where
    go :: NP (f :.: g) ys -> NP f (FunctorWrapTypeList g ys)
    go Nil = Nil
    go (fgx :* npTail) = unComp fgx :* go npTail

-- | UnCompose all of the 'NP's in an "NS (NP f) xss".
nsOfnpUnCompose :: forall f g xss. (SListI xss, SListI2 xss) => NS (NP (f :.: g)) xss -> NS (NP f) (FunctorWrapTypeListOfLists g xss)
nsOfnpUnCompose = go sList where
  go :: forall yss. (SListI yss, SListI2 yss) => SList yss -> NS (NP (f :.: g)) yss -> NS (NP f) (FunctorWrapTypeListOfLists g yss)
  go SNil _ = error "An NS cannot be empty"
  go SCons (Z np) = Z (npUnCompose np)
  go SCons (S ns') = S (go sList ns')

-- | The inverse of 'npUnCompose'.  Given a type-list indexed product where all the types in the list are applications of the same functor,
-- remove that functor from all the types in the list and put it in the functor parameter of the 'NP'.  The values in the product itself remain the same up
-- to types representing composition of the functors.
npReCompose :: forall f g xs. SListI xs => NP f (FunctorWrapTypeList g xs) -> NP (f :.: g) xs
npReCompose = go sList
  where
    go :: forall ys. SListI ys => SList ys ->  NP f (FunctorWrapTypeList g ys) -> NP (f :.: g) ys
    go SNil Nil = Nil
    go SCons (fgx :* npTail) = Comp fgx :* go sList npTail

-- | ReCompose all the 'NP's in an "NS (NP f) xss".
nsOfnpReCompose :: forall f g xss. (SListI xss, SListI2 xss) => NS (NP f) (FunctorWrapTypeListOfLists g xss) -> NS (NP (f :.: g)) xss
nsOfnpReCompose = go sList
  where
    go :: forall yss. (SListI2 yss, SListI yss) => SList yss -> NS (NP f) (FunctorWrapTypeListOfLists g yss) -> NS (NP (f :.: g)) yss
    go SNil _ = error "NS must be non-empty." -- this shouldn't happen since an NS can't be empty
    go SCons (Z np) = Z $ npReCompose np
    go SCons (S nsTail) = S $ go sList nsTail

-- | Prove that "SListI xs=>(FunctorWrapTypeList f xs)" is also an instance of SListI
functorWrappedSListIsSList :: forall f xs. SListI xs => Proxy f -> SList xs -> Dict SListI (FunctorWrapTypeList f xs)
functorWrappedSListIsSList _ SNil  = Dict
functorWrappedSListIsSList pf SCons = goCons (sList :: SList xs)
  where
    goCons :: forall y ys. SList (y ': ys) -> Dict SListI (FunctorWrapTypeList f (y ': ys))
    goCons SCons =
      let dict = functorWrappedSListIsSList pf $ (sList :: SList ys)
      in withDict dict Dict

-- | sequence (in the sense of 'Data.Traversable.sequenceA') a functor f inside an 'NP'
-- using a function defined over a 'DM.DMap' indexed by the same type-level-list.
-- This is useful in cases where an efficient general solution exists for DMaps.
-- This can be done more simply for Applicative f but the efficiency will depend on
-- the particular functor and given function over 'DM.DMap'.
npSequenceViaDMap :: forall k (f :: * -> *)  (g :: * -> *) (xs :: [*]). (Functor f, SListI xs, DM.GCompare k
                                                                        , k ~ TypeListTag (FunctorWrapTypeList g xs))
  =>(DM.DMap k f -> f (DM.DMap k Identity))->NP (f :.: g) xs -> f (NP g xs)
npSequenceViaDMap sequenceDMap =
  withDict (functorWrappedSListIsSList (Proxy :: Proxy g) (sList :: SList xs)) $
  fmap (hmap (runIdentity . unComp) . npReCompose . (\(Just x) -> x) . dMapToNP) .  sequenceDMap . npToDMap . npUnCompose
-- NB: The (\(Just x)->x) in there is safe!
-- dMapToNP has to return Maybe NP since the DMap could be incomplete.
-- But since we built this DMap from an NP, we know it's complete and dMapToNp will return a Just.
