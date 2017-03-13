module Reflex.Postpone.Class where

class Monad m => MonadPostpone m where
  -- | Postpone an action until all containing 'mfix'es can be expected to have
  -- finished executing
  postpone :: m a -> m a
