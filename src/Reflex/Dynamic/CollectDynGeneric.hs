{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-} -- only requried for copied section
{-# LANGUAGE FlexibleInstances          #-} -- only required for copied section
{-# LANGUAGE ExplicitNamespaces         #-}
{-# OPTIONS -fno-warn-unused-imports    #-}
{-# OPTIONS -fno-warn-unused-top-binds  #-}
{-|
Module: Reflex.Dynamic.CollectDynGeneric
Description: Generic (generics-sop) implementation of CollectDynPure and distributeFHListOverDynPure
-}
module Reflex.Dynamic.CollectDynGeneric
  (
    distributeNPOverDyn
  , collectDynGeneric
  , collectDynPureNP
  ) where

import Generics.SOP               (NS, NP,SListI, SListI2, hmap,I(I),unI
                                  , (:.:)(Comp),unComp,from,to, Generic
                                  ,Code,SOP(..),unSOP
                                  ,hsequence',hliftA, hcliftA, Proxy(..))
       
import Generics.SOP.DMapUtilities (npReCompose,nsOfnpReCompose, npSequenceViaDMap
                                  ,FunctorWrapTypeList,FunctorWrapTypeListOfLists)
       
import Reflex.Class               (Reflex) -- where is the canonical, least surface area place, to get this?
import Reflex.Dynamic             (Dynamic,distributeDMapOverDynPure)



distributeNPOverDyn::(Reflex t, SListI xs)=>NP I (FunctorWrapTypeList (Dynamic t) xs) -> Dynamic t (NP I xs)
distributeNPOverDyn = collectDynPureNP . hliftA (unI . unComp) . npReCompose

collectDynGeneric::(Reflex t,Generic a, Generic b, (Code a) ~ FunctorWrapTypeListOfLists (Dynamic t) (Code b))=>a -> Dynamic t b
collectDynGeneric = fmap (to . SOP) . hsequence' . collectDynPureNSNP . aToNSNPI

aToNSNPI::(Generic a, Code a ~ FunctorWrapTypeListOfLists (Dynamic t) xss, SListI2 xss) =>a -> NS (NP (I :.: Dynamic t)) xss
aToNSNPI = nsOfnpReCompose . unSOP . from

collectDynPureNSNP::(Reflex t,SListI2 xss)=>NS (NP (I :.: Dynamic t)) xss -> NS (Dynamic t :.: NP I) xss
collectDynPureNSNP =
  let slistIC = Proxy :: Proxy SListI
  in hcliftA slistIC (Comp . collectDynPureNP . hliftA (unI . unComp))

collectDynPureNP::(Reflex t, SListI xs)=>NP (Dynamic t) xs -> Dynamic t (NP I xs)
collectDynPureNP = npSequenceViaDMap distributeDMapOverDynPure . hliftA (Comp . fmap I) 


                   
