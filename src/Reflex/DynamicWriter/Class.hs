-- | This module defines the 'MonadDynamicWriter' class.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.DynamicWriter.Class
  ( MonadDynamicWriter (..)
  ) where

import Control.Monad.Reader (ReaderT, lift)
import Reflex.Class (Dynamic)

-- | 'MonadDynamicWriter' efficiently collects 'Dynamic' values using 'tellDyn'
-- and combines them monoidally to provide a 'Dynamic' result.
class (Monad m, Monoid w) => MonadDynamicWriter t w m | m -> t w where
  tellDyn :: Dynamic t w -> m ()

instance MonadDynamicWriter t w m => MonadDynamicWriter t w (ReaderT r m) where
  tellDyn = lift . tellDyn

