{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
import Data.Map.Monoidal (MonoidalMap)
import Data.Semigroup (Semigroup (..), (<>))
import GHC.Generics
import Data.Functor.Identity
import Data.Functor.Const
import Data.Proxy
import Control.Applicative

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

-- | Trivial group.
instance Group () where
  negateG _ = ()
  _ ~~ _ = ()
instance Additive ()

-- | Product group.  A Pair of groups gives rise to a group
instance (Group a, Group b) => Group (a, b) where
  negateG (a, b) = (negateG a, negateG b)
  (a, b) ~~ (c, d) = (a ~~ c, b ~~ d)
instance (Additive a, Additive b) => Additive (a, b)

-- See https://gitlab.haskell.org/ghc/ghc/issues/11135#note_111802 for the reason Compose is not also provided.
-- Base does not define Monoid (Compose f g a) so this is the best we can
-- really do for functor composition.
instance Group (f (g a)) => Group ((f :.: g) a) where
  negateG (Comp1 xs) = Comp1 (negateG xs)
  Comp1 xs ~~ Comp1 ys = Comp1 (xs ~~ ys)
instance Additive (f (g a)) => Additive ((f :.: g) a)

-- | Product of groups, Functor style.
instance (Group (f a), Group (g a)) => Group ((f :*: g) a) where
  negateG (a :*: b) = negateG a :*: negateG b
  (a :*: b) ~~ (c :*: d) = (a ~~ c) :*: (b ~~ d)
instance (Additive (f a), Additive (g a)) => Additive ((f :*: g) a)

-- | Trivial group, Functor style
instance Group (Proxy x) where
  negateG _ = Proxy
  _ ~~ _ = Proxy
instance Additive (Proxy x)

-- | Const lifts groups into a functor.
deriving instance Group a => Group (Const a x)
instance Additive a => Additive (Const a x)
-- | Ideitnty lifts groups pointwise (at only one point)
deriving instance Group a => Group (Identity a)
instance Additive a => Additive (Identity a)

-- | Functions lift groups pointwise.
instance Group b => Group (a -> b) where
  negateG f = negateG . f
  (~~) = liftA2 (~~)
instance Additive b => Additive (a -> b)
