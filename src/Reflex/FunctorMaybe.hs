-- | This module defines the FunctorMaybe class, which extends Functors with the
-- ability to delete values.
module Reflex.FunctorMaybe
  ( FunctorMaybe (..)
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

--TODO: See if there's a better class in the standard libraries already

-- | A class for values that combines filtering and mapping using 'Maybe'.
-- Morally, @'FunctorMaybe' ~ KleisliFunctor 'Maybe'@. Also similar is the
-- @Witherable@ typeclass, but it requires @Foldable f@ and @Traverable f@,
-- and e.g. 'Event' is instance of neither.
--
-- A definition of 'fmapMaybe' must satisfy the following laws:
--
-- [/identity/]
--   @'fmapMaybe' 'Just' ≡ 'id'@
--
-- [/composition/]
--   @'fmapMaybe' (f <=< g) ≡ 'fmapMaybe' f . 'fmapMaybe' g@
class FunctorMaybe f where
  -- | Combined mapping and filtering function.
  fmapMaybe :: (a -> Maybe b) -> f a -> f b

-- | @fmapMaybe = (=<<)
instance FunctorMaybe Maybe where
  fmapMaybe = (=<<)

-- | @fmapMaybe = mapMaybe@
instance FunctorMaybe [] where
  fmapMaybe = mapMaybe

instance FunctorMaybe (Map k) where
  fmapMaybe = Map.mapMaybe

instance FunctorMaybe IntMap where
  fmapMaybe = IntMap.mapMaybe
