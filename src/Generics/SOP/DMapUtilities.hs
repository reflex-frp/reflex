{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
{-|
Module           : Generics.SOP.DMapUtilities
Description      : Utilities for converting between the NS/NP types of generics-sop and Dependent Maps.
-}

module Generics.SOP.DMapUtilities
  (
    -- * Type Functions
    -- ** Add functors around types in a typelist or typelist of typelists
    FunctorWrapTypeList
  , FunctorWrapTypeListOfLists
  
    -- * Types
  , TypeListTag
  
    -- * Conversions
    -- ** NP <-> DMap
  , npToDMap
  , dMapToNP
    -- ** NS <-> DSum
  , nsToDSum
  , dSumToNS
  
  -- * Functor wrapping/unwrapping utilities for 'NP' 
  , npUnCompose
  , npReCompose
  , nsOfnpReCompose
  
  -- * Utilities
  , npSequenceViaDMap
  -- * Proofs
  
  , functorWrappedSListIsSList
  )where

import           Generics.SOP          (hmap,hcollapse,NS(..),NP(..),SListI(..)
                                       ,SListI2,SList(..),K(..)
                                       , type (-.->)(Fn), (:.:)(Comp), unComp,Proxy(..))
import           Generics.SOP.NP       (sequence'_NP)
import           Generics.SOP.NS       (ap_NS)
import           Generics.SOP.Dict     (Dict(Dict),withDict)

import qualified Data.Dependent.Map as DM
import           Data.Dependent.Sum    (DSum ((:=>)))
import qualified Data.Dependent.Sum as DS

import           Data.GADT.Compare     ((:~:) (..), GCompare (..), GEq (..),
                                        GOrdering (..))


import Data.Functor.Identity           (Identity(runIdentity))


-- |A Tag type for making DMaps of type-level lists
data TypeListTag (xs :: [k]) (x :: k) where -- x is in xs
  Here  :: TypeListTag (x ': xs) x          -- x begins xs
  There :: TypeListTag xs x -> TypeListTag (y ': xs) x -- given that x is in xs, x is also in (y ': xs)

instance GEq (TypeListTag xs) where
  geq Here      Here      = Just Refl
  geq (There x) (There y) = geq x y
  geq _         _         = Nothing

instance GCompare (TypeListTag xs) where
  gcompare Here Here = GEQ
  gcompare Here (There _) = GLT
  gcompare (There _) Here = GGT
  gcompare (There x) (There y) = gcompare x y



-- |Convert an 'NP' indexed by typelist xs into a 'DM.DMap' indexed by 'TypeListTag' xs
npToDMap::NP f xs -> DM.DMap (TypeListTag xs) f
npToDMap Nil = DM.empty
npToDMap (fx :* np') = DM.insert Here fx $ DM.mapKeysMonotonic There $ npToDMap np'

-- |Convert a 'DM.DMap' indexed by 'TypeListTag' xs to an 'NP'
-- |NB: This can fail since there is no guarantee that the 'DM.DMap' has entries for every tag. Hence it returns a 'Maybe'
dMapToNP::forall xs f.SListI xs=>DM.DMap (TypeListTag xs) f -> Maybe (NP f xs)
dMapToNP dm = sequence'_NP $ hmap (\tag -> Comp $ DM.lookup tag dm) makeTypeListTagNP

-- |Convert a 'NS' indexed by a typelist xs to a 'DS.DSum' indexed by 'TypeListTag' xs 
nsToDSum::SListI xs=>NS f xs -> DS.DSum (TypeListTag xs) f
nsToDSum ns =
  let nsFToNSDSum::SListI xs=>NS f xs -> NS (K (DS.DSum (TypeListTag xs) f)) xs
      nsFToNSDSum ns' = ap_NS (tagsToFs makeTypeListTagNP) ns'
      tagsToFs::SListI xs=>NP (TypeListTag xs) xs -> NP (f -.-> K (DS.DSum (TypeListTag xs) f)) xs
      tagsToFs = hmap (\tag -> (Fn $ \val -> K (tag :=> val)))
  in hcollapse $ nsFToNSDSum ns

-- |Convert a 'DS.DSum' indexed by 'TypeListTag' xs into a 'NS' indexed by xs
dSumToNS::SListI xs=>DS.DSum (TypeListTag xs) f -> NS f xs
dSumToNS (tag :=> fa) = go tag fa where
  go::TypeListTag ys y->f y->NS f ys
  go Here fy = Z fy
  go (There tag') fy = S (go tag' fy)

makeTypeListTagNP::SListI xs=>NP (TypeListTag xs) xs
makeTypeListTagNP = go sList where
  go::forall ys.SListI ys=>SList ys->NP (TypeListTag ys) ys
  go SNil = Nil
  go SCons = Here :* hmap There (go sList)


-- these are here to allow moving functors in and out of typelists
type family FunctorWrapTypeList (f :: * -> *) (xs :: [*]) :: [*] where
  FunctorWrapTypeList f '[] = '[]
  FunctorWrapTypeList f (x ': xs) = f x ': FunctorWrapTypeList f xs

type family FunctorWrapTypeListOfLists (f :: * -> *) (xss :: [[*]]) :: [[*]] where
  FunctorWrapTypeListOfLists f '[] = '[]
  FunctorWrapTypeListOfLists f (xs ': xss') = FunctorWrapTypeList f xs ': FunctorWrapTypeListOfLists f xss'

npUnCompose::forall f g xs.SListI xs=>NP (f :.: g) xs -> NP f (FunctorWrapTypeList g xs)
npUnCompose np = go np where
  go::NP (f :.: g) ys -> NP f (FunctorWrapTypeList g ys)
  go Nil = Nil
  go (fgx :* np') = unComp fgx :* go np'


npReCompose::forall f g xs.SListI xs=>NP f (FunctorWrapTypeList g xs) -> NP (f :.: g) xs -- (RemoveFunctor g (AddFunctor g xs))
npReCompose = go sList where
  go::forall ys.SListI ys=>SList ys ->  NP f (FunctorWrapTypeList g ys) -> NP (f :.: g) ys
  go SNil Nil = Nil
  go SCons (fgx :* np') = Comp fgx :* go sList np'

nsOfnpReCompose::forall f g xss.(SListI xss, SListI2 xss)=>NS (NP f) (FunctorWrapTypeListOfLists g xss) -> NS (NP (f :.: g)) xss
nsOfnpReCompose = go sList
  where
    go::forall yss.(SListI2 yss, SListI yss)=>SList yss->NS (NP f) (FunctorWrapTypeListOfLists g yss) -> NS (NP (f :.: g)) yss
    go SNil _ = undefined -- this shouldn't' happen since an NS can't be empty
    go SCons (Z np) = Z (npReCompose np)
    go SCons (S ns') = S (go sList ns')


-- required to prove the wrapped typelist is an instance of SListI
functorWrappedSListIsSList :: forall f xs . SListI xs=>Proxy f -> SList xs -> Dict SListI (FunctorWrapTypeList f xs)
functorWrappedSListIsSList _ SNil  = Dict
functorWrappedSListIsSList pf SCons = goCons (sList :: SList xs)
  where
    goCons :: forall y ys . SList (y ': ys) -> Dict SListI (FunctorWrapTypeList f (y ': ys))
    goCons SCons = withDict (functorWrappedSListIsSList  pf (sList :: SList ys)) Dict


-- NB: THe fromJust in there is safe!
-- dMapToNP has to return Maybe NP since the DMap could be incomplete.
-- But since we built this DMap from an NP, we know it's complete and dMapToNp will return a Just.  
npSequenceViaDMap::forall k (f:: * -> *)  (g:: * -> *) (xs::[*]).(Functor f
                                                                 , SListI xs
                                                                 , DM.GCompare k
                                                                 , k ~ TypeListTag (FunctorWrapTypeList g xs))
  =>(DM.DMap k f -> f (DM.DMap k Identity))->NP (f :.: g) xs -> f (NP g xs)
npSequenceViaDMap sequenceDMap =
  withDict (functorWrappedSListIsSList (Proxy :: Proxy g) (sList :: SList xs)) $
  fmap (hmap (runIdentity . unComp) . npReCompose . (\(Just x)->x) . dMapToNP) .  sequenceDMap . npToDMap . npUnCompose
