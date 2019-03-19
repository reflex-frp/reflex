{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:
--   Reflex.Patch
-- Description:
--   This module defines the 'Patch' class, which is used by Reflex to manage
--   changes to 'Reflex.Class.Incremental' values.
module Reflex.Patch
  ( module Reflex.Patch
  , module X
  ) where

import Reflex.Patch.Class as X
import Reflex.Patch.DMap as X hiding (getDeletions)
import Reflex.Patch.DMapWithMove as X (PatchDMapWithMove, const2PatchDMapWithMoveWith, mapPatchDMapWithMove,
                                       patchDMapWithMoveToPatchMapWithMoveWith,
                                       traversePatchDMapWithMoveWithKey, unPatchDMapWithMove,
                                       unsafePatchDMapWithMove, weakenPatchDMapWithMoveWith)
import Reflex.Patch.IntMap as X hiding (getDeletions)
import Reflex.Patch.Map as X
import Reflex.Patch.MapWithMove as X (PatchMapWithMove, patchMapWithMoveNewElements,
                                      patchMapWithMoveNewElementsMap, unPatchMapWithMove,
                                      unsafePatchMapWithMove)
import Reflex.Patch.MapWithMove2 as X (PatchMapWithMove2, patchMapWithMove2NewElements,
                                      patchMapWithMove2NewElementsMap, unPatchMapWithMove2,
                                      unsafePatchMapWithMove2)

import Data.Map.Monoidal (MonoidalMap)
import Data.Semigroup (Semigroup (..), (<>))

-- | A 'Group' is a 'Monoid' where every element has an inverse.
class (Semigroup q, Monoid q) => Group q where
  negateG :: q -> q
  (~~) :: q -> q -> q
  r ~~ s = r <> negateG s

-- | An 'Additive' 'Semigroup' is one where (<>) is commutative
class Semigroup q => Additive q where

-- | The elements of an 'Additive' 'Semigroup' can be considered as patches of their own type.
newtype AdditivePatch p = AdditivePatch { unAdditivePatch :: p }

instance Additive p => Patch (AdditivePatch p) where
  type PatchTarget (AdditivePatch p) = p
  apply (AdditivePatch p) q = Just $ p <> q

instance (Ord k, Group q) => Group (MonoidalMap k q) where
  negateG = fmap negateG

instance (Ord k, Additive q) => Additive (MonoidalMap k q)
