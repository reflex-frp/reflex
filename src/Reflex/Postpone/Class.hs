module Reflex.Postpone.Class where

import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

class Monad m => MonadPostpone m where
  -- | Postpone an action until all containing 'mfix'es can be expected to have
  -- finished executing
  postpone :: m a -> m a

instance MonadPostpone m => MonadPostpone (ReaderT r m) where
  postpone = liftThrough postpone

instance (MonadPostpone m, Monoid w) => MonadPostpone (WriterT w m) where
  postpone = liftThrough postpone

