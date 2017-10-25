{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Reflex.Dynamic.FactorDynGeneric
  ( factorDynGeneric
  -- * Examples
  , maybeDynGeneric
  , eitherDynGeneric
  ) where

import Control.Monad.Fix (MonadFix)
import Data.Functor.Compose (getCompose)
import Reflex.Class (Dynamic, MonadHold, Reflex)
import Reflex.Dynamic (factorDyn)

import Generics.SOP ((:.:) (Comp), Code, Generic, I (I), Proxy (Proxy), SListI, SOP (SOP), from, hcmap, hmap,
                     to, unSOP)
import Generics.SOP.Distribute (distributeI_NP)
import Generics.SOP.DMapUtilities (FunctorWrapTypeListOfLists, dSumToNS, nsOfnpUnCompose, nsToDSum)

-- | Use factorDyn and generics-sop to turn a Dynamic of a type into a factored Dynamic of a type with Dynamic fields.  See below functions for examples
factorDynGeneric :: forall t m a b.
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  , Generic a
  , Generic b
  , (Code b) ~ FunctorWrapTypeListOfLists (Dynamic t) (Code a))
  => Dynamic t a -> m (Dynamic t b)
factorDynGeneric da = do
  let dSumDyn = nsToDSum . unSOP . from <$> da -- Dynamic t (DSum (TypeListTag (Code a)) (NP I xs))
  dSumDyn' <- factorDyn dSumDyn  -- Dynamic t (DSum (TypeListTag (Code a)) (NP I))
  let nsnpDyn = dSumToNS <$> dSumDyn' -- Dynamic t (NS (Compose (Dynamic t) (NP I)) (Code a))
      sListIC = Proxy :: Proxy SListI
      nsnpDyn' = hcmap sListIC (hmap (Comp. I) . distributeI_NP . getCompose) <$> nsnpDyn -- Dynamic t (NS (NP (I :.: Dynamic t)) (Code a))
      nsnpDyn'' = nsOfnpUnCompose <$> nsnpDyn' -- Dynamic t (NS (NP I) (Code b))
      result = to . SOP <$> nsnpDyn''
  return result

-- | Example implementation of maybeDyn using factorDynGeneric
maybeDynGeneric :: (Reflex t, MonadHold t m, MonadFix m) => Dynamic t (Maybe a) -> m (Dynamic t (Maybe (Dynamic t a)))
maybeDynGeneric = factorDynGeneric

-- | Example implementation of eitherDyn using factorDynGeneric
eitherDynGeneric :: (Reflex t, MonadHold t m, MonadFix m) => Dynamic t (Either a b) -> m (Dynamic t (Either (Dynamic t a) (Dynamic t b)))
eitherDynGeneric = factorDynGeneric
