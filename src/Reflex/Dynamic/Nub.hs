{-# LANGUAGE MagicHash #-}
module Reflex.Dynamic.Nub
       ( NubDynamic
       , nubDynamic
       , fromNubDynamic
       , foldNubDyn
       , foldNubDynM
       , foldNubDynMaybe
       , foldNubDynMaybeM
       ) where

import Reflex.Class
import Reflex.Dynamic

import Control.Monad.Fix
import GHC.Exts

newtype NubDynamic t a = NubDynamic { unNubDynamic :: Dynamic t a }

nubDynamic :: Reflex t => Dynamic t a -> NubDynamic t a
nubDynamic d = NubDynamic $ unsafeBuildDynamic (sample $ current d) $ flip push (updated d) $ \new -> do --TODO: It would be very nice if we had an uncached push here
  old <- sample $ current d --TODO: Is it better to sample ourselves here?
  return $ unsafeJustChanged old new

fromNubDynamic :: (Reflex t, Eq a) => NubDynamic t a -> Dynamic t a
fromNubDynamic (NubDynamic d) = nubDyn d

unsafeJustChanged :: a -> a -> Maybe a
unsafeJustChanged old new = case old `seq` new `seq` reallyUnsafePtrEquality# old new of
  0# -> Just new
  _ -> Nothing

foldNubDyn :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> b) -> b -> Event t a -> m (NubDynamic t b)
foldNubDyn f = foldNubDynMaybe $ \o v -> Just $ f o v

foldNubDynM :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t b) -> b -> Event t a -> m (NubDynamic t b)
foldNubDynM f = foldNubDynMaybeM $ \o v -> fmap Just $ f o v

foldNubDynMaybe :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> Maybe b) -> b -> Event t a -> m (NubDynamic t b)
foldNubDynMaybe f = foldNubDynMaybeM $ \o v -> return $ f o v

foldNubDynMaybeM :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t (Maybe b)) -> b -> Event t a -> m (NubDynamic t b)
foldNubDynMaybeM f z e = do
  let f' change old = do
        x <- f change old
        return $ case x of
          Nothing -> Nothing
          Just new -> unsafeJustChanged old new
  d <- foldDynMaybeM f' z e
  return $ NubDynamic d

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
