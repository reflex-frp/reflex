{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Postpone.Base where

import Reflex.Class
import Reflex.Host.Class
import Reflex.Postpone.Class

import Control.Concurrent.MVar
import Control.Monad.Exception
import Control.Monad.Primitive
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Sequence as Seq
import System.IO.Unsafe

newtype PostponeT m a = PostponeT { unPostponeT :: StateT (Seq (PostponeT m ())) m a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException)

runPostponeT :: Monad m => PostponeT m a -> m a
runPostponeT a = do
  (result, postponed) <- runStateT (unPostponeT a) mempty
  when (not $ Seq.null postponed) $ do
    runPostponeT $ sequence_ postponed
  return result

instance MonadIO m => MonadPostpone (PostponeT m) where
  postpone a = PostponeT $ do
    v <- liftIO $ newEmptyMVar
    modify (|> (a >>= liftIO . putMVar v))
    liftIO $ unsafeInterleaveIO $ takeMVar v

instance PrimMonad m => PrimMonad (PostponeT m) where
  type PrimState (PostponeT m) = PrimState m
  primitive = lift . primitive

instance MonadTrans PostponeT where
  lift = PostponeT . lift

instance MonadSample t m => MonadSample t (PostponeT m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (PostponeT m) where
  {-# INLINABLE hold #-}
  hold v0 v' = lift $ hold v0 v'
  {-# INLINABLE holdDyn #-}
  holdDyn v0 v' = lift $ holdDyn v0 v'
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 v' = lift $ holdIncremental v0 v'

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (PostponeT m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance MonadRef m => MonadRef (PostponeT m) where
  type Ref (PostponeT m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (PostponeT m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r
