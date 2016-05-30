{-# LANGUAGE MagicHash, FlexibleInstances, MultiParamTypeClasses #-}
module Reflex.Dynamic.Nub
       ( NubDynamic
       , nubDynamic
       , fromNubDynamic
       , alreadyNubbedDynamic
       ) where

import Reflex.Class
import Reflex.Dynamic

import GHC.Exts

newtype NubDynamic t a = NubDynamic { unNubDynamic :: Dynamic t a }

nubDynamic :: Reflex t => Dynamic t a -> NubDynamic t a
nubDynamic d = NubDynamic $ unsafeBuildDynamic (sample $ current d) $ flip push (updated d) $ \new -> do --TODO: It would be very nice if we had an uncached push here
  old <- sample $ current d --TODO: Is it better to sample ourselves here?
  return $ unsafeJustChanged old new

fromNubDynamic :: (Reflex t, Eq a) => NubDynamic t a -> Dynamic t a
fromNubDynamic (NubDynamic d) = nubDyn d

-- | Create a NubDynamic without nubbing it on creation
-- This will be slightly faster than nubDynamic when used with a Dynamic whose values are always (or nearly always) different from its previous values; if used with a Dynamic whose values do not change frequently, it may be much slower than nubDynamic
alreadyNubbedDynamic :: Dynamic t a -> NubDynamic t a
alreadyNubbedDynamic = NubDynamic

unsafeJustChanged :: a -> a -> Maybe a
unsafeJustChanged old new = case old `seq` new `seq` reallyUnsafePtrEquality# old new of
  0# -> Just new
  _ -> Nothing

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
