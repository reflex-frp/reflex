{-# LANGUAGE TypeFamilies #-}
-- | The interface for types which represent changes made to other types
module Reflex.Patch.Class where

import Control.Monad.Identity
import Data.Maybe

-- | A 'Patch' type represents a kind of change made to a datastructure.
class Patch p where
  type PatchTarget p :: *
  -- | Apply the patch @p a@ to the value @a@.  If no change is needed, return
  -- 'Nothing'.
  apply :: p -> PatchTarget p -> Maybe (PatchTarget p)

-- | Apply a 'Patch'; if it does nothing, return the original value
applyAlways :: Patch p => p -> PatchTarget p -> PatchTarget p
applyAlways p t = fromMaybe t $ apply p t

-- | 'Identity' can be used as a 'Patch' that always fully replaces the value
instance Patch (Identity a) where
  type PatchTarget (Identity a) = a
  apply (Identity a) _ = Just a
