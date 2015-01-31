{-# LANGUAGE RecursiveDo, TypeFamilies, LambdaCase #-}
module Control.Monad.Ref where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Data.IORef

class Monad m => MonadRef m where
  type Ref m :: * -> *
  newRef :: a -> m (Ref m a)
  readRef :: Ref m a -> m a
  writeRef :: Ref m a -> a -> m ()
  atomicModifyRef :: Ref m a -> (a -> (a, b)) -> m b

instance MonadRef IO where
  type Ref IO = IORef
  {-# INLINE newRef #-}
  newRef = newIORef
  {-# INLINE readRef #-}
  readRef = readIORef
  {-# INLINE writeRef #-}
  writeRef = writeIORef
  {-# INLINE atomicModifyRef #-}
  atomicModifyRef r f = do
    result <- atomicModifyIORef' r f
--    evaluate =<< readIORef r --TODO: Verify that ghcjs now strictly evaluates the values in atomicModifyIORef', then remove this line
    return result

{-# INLINE cacheM #-}
cacheM :: (MonadRef m, MonadRef m', Ref m ~ Ref m') => m' a -> m (m' a, m ())
cacheM a = do
  r <- newRef undefined
  let invalidate = writeRef r $ do
        result <- a
        writeRef r $ return result
        return result
  invalidate
  return (join $ readRef r, invalidate)

{-# INLINE cacheMWithTry #-}
cacheMWithTry :: (MonadRef m, MonadRef m', Ref m ~ Ref m') => m' a -> m (m' a, m (Maybe a), m ())
cacheMWithTry a = do
  r <- newRef $ Left a
  let invalidate = writeRef r $ Left a
      get = readRef r >>= \case
        Left a' -> do
          result <- a'
          writeRef r $ Right result
          return result
        Right result -> return result
      tryGet = readRef r >>= \case
        Left _ -> return Nothing
        Right result -> return $ Just result
  return (get, tryGet, invalidate)

-- | Not thread-safe or reentrant
{-# INLINE memoM #-}
memoM :: (MonadRef m, MonadRef m', Ref m ~ Ref m') => m' a -> m (m' a)
memoM = liftM fst . cacheM

{-# INLINE replaceRef #-}
replaceRef :: MonadRef m => Ref m a -> a -> m a
replaceRef r new = atomicModifyRef r $ \old -> (new, old)

{-# INLINE modifyRef #-}
modifyRef :: MonadRef m => Ref m a -> (a -> a) -> m ()
modifyRef r f = atomicModifyRef r $ \a -> (f a, ())

instance (Monoid w, MonadRef m) => MonadRef (WriterT w m) where
  {-# SPECIALIZE instance Monoid w => MonadRef (WriterT w IO) #-}
  type Ref (WriterT w m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r
  atomicModifyRef r f = lift $ atomicModifyRef r f

instance MonadRef m => MonadRef (ReaderT r m) where
  {-# SPECIALIZE instance MonadRef (ReaderT r IO) #-}
  type Ref (ReaderT r m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r
  atomicModifyRef r f = lift $ atomicModifyRef r f
