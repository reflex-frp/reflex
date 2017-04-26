{-# LANGUAGE TypeFamilies #-}
module Reflex.Patch.Class where

import Control.Monad.Identity
import Data.Maybe

-- | A 'Patch' type represents a kind of change made to a datastructure.
class Patch p where
  type PatchTarget p :: *
  -- | Apply the patch @p a@ to the value @a@.  If no change is needed, return
  -- 'Nothing'.
  apply :: p -> PatchTarget p -> Maybe (PatchTarget p)

applyAlways :: Patch p => p -> PatchTarget p -> PatchTarget p
applyAlways p t = fromMaybe t $ apply p t

instance Patch (Identity a) where
  type PatchTarget (Identity a) = a
  apply (Identity a) _ = Just a

