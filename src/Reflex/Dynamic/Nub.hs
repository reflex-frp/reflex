{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | This module provides a variation of 'Dynamic' values that uses cheap
-- pointer equality checks to reduce the amount of signal propagation needed.
module Reflex.Dynamic.Nub
       ( NubDynamic
       , nubDynamic
       , fromNubDynamic
       , alreadyNubbedDynamic
       ) where

import Control.Applicative (Applicative (..))
import GHC.Exts
import Reflex.Class
import Reflex.Dynamic

-- | A 'Dynamic' whose 'updated' 'Event' will never fire with the same value as
-- the 'current' 'Behavior''s contents.  In order to maintain this constraint,
-- the value inside a 'NubDynamic' is always evaluated to
-- <https://wiki.haskell.org/Weak_head_normal_form weak head normal form>.
--
-- Internally, 'NubDynamic' uses pointer equality as a heuristic to avoid
-- unnecessary update propagation; this is much more efficient than performing
-- full comparisons.  However, when the 'NubDynamic' is converted back into a
-- regular 'Dynamic', a full comparison is performed.
newtype NubDynamic t a = NubDynamic { unNubDynamic :: Dynamic t a }

-- | Construct a 'NubDynamic' by eliminating redundant updates from a 'Dynamic'.
nubDynamic :: Reflex t => Dynamic t a -> NubDynamic t a
nubDynamic d = NubDynamic $ unsafeBuildDynamic (sample $ current d) $ flip push (updated d) $ \new -> do --TODO: It would be very nice if we had an uncached push here
  old <- sample $ current d --TODO: Is it better to sample ourselves here?
  return $ unsafeJustChanged old new

-- | Retrieve a normal 'Dynamic' from a 'NubDynamic'.  This will perform a final
-- check using the output type's 'Eq' instance to ensure deterministic behavior.
--
-- WARNING: If used with a type whose 'Eq' instance is not law-abiding -
-- specifically, if there are cases where @x /= x@, 'fromNubDynamic' may
-- eliminate more 'updated' occurrences than it should.  For example, NaN values
-- of 'Double' and 'Float' are considered unequal to themselves by the 'Eq'
-- instance, but can be equal by pointer equality.  This may cause 'NubDynamic'
-- to lose changes from NaN to NaN.
fromNubDynamic :: (Reflex t, Eq a) => NubDynamic t a -> Dynamic t a
fromNubDynamic (NubDynamic d) = uniqDynBy superEq d
  where
    -- Only consider values different if they fail both pointer equality /and/
    -- 'Eq' equality.  This is to make things a bit more deterministic in the
    -- case of unlawful 'Eq' instances.  However, it is still possible to
    -- achieve nondeterminism by constructing elements that are identical in
    -- value, unequal according to 'Eq', and nondeterministically equal or
    -- nonequal by pointer quality.  I suspect that it is impossible to make the
    -- behavior deterministic in this case.
    superEq a b = a `unsafePtrEq` b || a == b

-- | Create a NubDynamic without nubbing it on creation.  This will be slightly
-- faster than nubDynamic when used with a Dynamic whose values are always (or
-- nearly always) different from its previous values; if used with a Dynamic
-- whose values do not change frequently, it may be much slower than nubDynamic
alreadyNubbedDynamic :: Dynamic t a -> NubDynamic t a
alreadyNubbedDynamic = NubDynamic

unsafePtrEq :: a -> a -> Bool
unsafePtrEq a b = case a `seq` b `seq` reallyUnsafePtrEquality# a b of
  0# -> False
  _ -> True

unsafeJustChanged :: a -> a -> Maybe a
unsafeJustChanged old new =
  if old `unsafePtrEq` new
  then Nothing
  else Just new

instance Reflex t => Accumulator t (NubDynamic t) where
  accumMaybeM f z e = do
    let f' old change = do
          mNew <- f old change
          return $ unsafeJustChanged old =<< mNew
    d <- accumMaybeM f' z e
    return $ NubDynamic d
  mapAccumMaybeM f z e = do
    let f' old change = do
          (mNew, output) <- f old change
          return (unsafeJustChanged old =<< mNew, output)
    (d, out) <- mapAccumMaybeM f' z e
    return (NubDynamic d, out)

instance Reflex t => Functor (NubDynamic t) where
  fmap f (NubDynamic d) = nubDynamic $ fmap f d

instance Reflex t => Applicative (NubDynamic t) where
  pure = NubDynamic . constDyn
  NubDynamic a <*> NubDynamic b = nubDynamic $ a <*> b
  _ *> b = b
  a <* _ = a

instance Reflex t => Monad (NubDynamic t) where
  NubDynamic x >>= f = nubDynamic $ x >>= unNubDynamic . f
  _ >> b = b
  return = pure
