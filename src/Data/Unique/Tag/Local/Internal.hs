{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Unique.Tag.Local.Internal where

import Control.Monad.Exception
import Control.Monad.Primitive
import Control.Monad.Reader
import Data.Primitive.MutVar

newtype Tag x a = Tag Int

tagId :: Tag x a -> Int
tagId (Tag n) = n

-- | WARNING: If you construct a tag with the wrong type, it will result in
-- incorrect unsafeCoerce applications, which can segfault or cause arbitrary
-- other damage to your program
unsafeTagFromId :: Int -> Tag x a
unsafeTagFromId n = Tag n

class TagGen m where
  type TagScope m :: *
  -- | Note: this will throw an exception if its internal counter would wrap around
  mkTag :: m (Tag (TagScope m) a)

newtype TagGenT (s :: *) m a = TagGenT { unTagGenT :: ReaderT (MutVar (PrimState m) Int) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException)

instance MonadTrans (TagGenT s) where
  lift = TagGenT . lift

data TagGenTScope (ps :: *) (s :: *)

instance PrimMonad m => TagGen (TagGenT s m) where
  type TagScope (TagGenT s m) = s
  mkTag = TagGenT $ ReaderT $ \r -> do
    n <- atomicModifyMutVar' r $ \x -> (succ x, x)
    pure $ Tag n

runTagGenT :: forall m a. PrimMonad m => (forall s. TagGenT s m a) -> m a
runTagGenT (TagGenT a :: TagGenT (TagGenTScope (PrimState m) ()) m a) = do
  r <- newMutVar minBound
  runReaderT a r

mapTagGenT :: PrimState m ~ PrimState n => (m a -> n b) -> TagGenT s m a -> TagGenT s n b
mapTagGenT f (TagGenT a) = TagGenT $ mapReaderT f a
