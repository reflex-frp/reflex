-- | The type-safety of this module depends on two assumptions:
-- 1. If the `s` parameters on two `TagGen`s can unify, then they contain the same MutVar
-- 2. Two Tag values made from the same TagGen never contain the same Int

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
module Data.Unique.Tag.Local.Internal where

import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.GADT.Compare
import Data.Some
import GHC.Exts (Int (..), Int#, MutVar#, unsafeCoerce#)

import Unsafe.Coerce

-- `x` is which generator it's from
-- `a` is the type of the thing it's tagging
newtype Tag x a = Tag Int

tagId :: Tag x a -> Int
tagId (Tag n) = n

-- | WARNING: If you construct a tag with the wrong type, it will result in
-- incorrect unsafeCoerce applications, which can segfault or cause arbitrary
-- other damage to your program
unsafeTagFromId :: Int -> Tag x a
unsafeTagFromId = Tag

-- We use Int because it is supported by e.g. IntMap
newtype TagGen ps s = TagGen { unTagGen :: MutVar ps Int }

instance Show (TagGen ps s) where
  show (TagGen (MutVar m)) = show $ I# ((unsafeCoerce# :: MutVar# ps Int -> Int#) m)

instance GEq (TagGen ps) where
  TagGen a `geq` TagGen b =
    if a == b
    then Just $ unsafeCoerce Refl
    else Nothing

newTag :: PrimMonad m => TagGen (PrimState m) s -> m (Tag s a)
newTag (TagGen r) = do
  n <- atomicModifyMutVar' r $ \x -> (succ x, x)
  pure $ Tag n

newTagGen :: PrimMonad m => m (Some (TagGen (PrimState m)))
newTagGen = Some . TagGen <$> newMutVar minBound

withTagGen :: PrimMonad m => (forall s. TagGen (PrimState m) s -> m a) -> m a
withTagGen f = do
  g <- newTagGen
  withSome g f
