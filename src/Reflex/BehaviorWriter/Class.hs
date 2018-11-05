{-|
Module: Reflex.BehaviorWriter.Class
Description: This module defines the 'MonadBehaviorWriter' class
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.BehaviorWriter.Class
  ( MonadBehaviorWriter (..)
  ) where

import Control.Monad.Reader (ReaderT, lift)
import Reflex.Class (Behavior)

-- | 'MonadBehaviorWriter' efficiently collects 'Behavior' values using 'tellBehavior'
-- and combines them monoidally to provide a 'Behavior' result.
class (Monad m, Monoid w) => MonadBehaviorWriter t w m | m -> t w where
  tellBehavior :: Behavior t w -> m ()

instance MonadBehaviorWriter t w m => MonadBehaviorWriter t w (ReaderT r m) where
  tellBehavior = lift . tellBehavior
