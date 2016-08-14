{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Deletable.Class where

import Reflex

import Control.Monad.Reader
import Control.Monad.Trans.Control

class (Reflex t, Monad m) => Deletable t m | m -> t where
  deletable :: Event t () -> m a -> m a

instance Deletable t m => Deletable t (ReaderT r m) where
  {-# INLINABLE deletable #-}
  deletable = liftThrough . deletable
