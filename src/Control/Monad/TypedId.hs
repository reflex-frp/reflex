{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Control.Monad.TypedId ( MonadTypedId (..)
                             ) where

import Data.GADT.Compare
import Data.IORef

import Control.Monad.State
import Control.Monad.Reader

import Unsafe.Coerce
import System.IO.Unsafe

class (Monad m, GCompare (TypedId m)) => MonadTypedId m where
  type TypedId m :: * -> *
  getTypedId :: m (TypedId m a)

nextTypedIdIO :: IORef Int
{-# NOINLINE nextTypedIdIO #-}
nextTypedIdIO = unsafePerformIO $ newIORef 1

newtype TypedId_IO a = TypedId_IO Int deriving (Show)

instance MonadTypedId IO where
  type TypedId IO = TypedId_IO
  {-# INLINE getTypedId #-}
  getTypedId = do
    n <- atomicModifyIORef' nextTypedIdIO $ \n -> (succ n, n)
    return $ TypedId_IO n

instance GCompare TypedId_IO where
  {-# INLINE gcompare #-}
  TypedId_IO a `gcompare` TypedId_IO b = case a `compare` b of
    LT -> GLT
    EQ -> unsafeCoerce GEQ
    GT -> GGT

instance GEq TypedId_IO where
  {-# INLINE geq #-}
  TypedId_IO a `geq` TypedId_IO b = if a == b then Just $ unsafeCoerce Refl else Nothing

instance MonadTypedId m => MonadTypedId (StateT s m) where
  type TypedId (StateT s m) = TypedId m
  {-# INLINE getTypedId #-}
  getTypedId = lift getTypedId

instance MonadTypedId m => MonadTypedId (ReaderT r m) where
  type TypedId (ReaderT r m) = TypedId m
  {-# INLINE getTypedId #-}
  getTypedId = lift getTypedId
