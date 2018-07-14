{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Workflow (
  -- * Workflows
    Workflow (..)
  , workflow
  , workflowView
  , mapWorkflow
  , mapWorkflowCheap
  ) where

import Control.Arrow ((***))
import Control.Monad.Fix (MonadFix)

import Reflex.Class
import Reflex.Network
import Reflex.NotReady.Class
import Reflex.PostBuild.Class

newtype Workflow t m a = Workflow { unWorkflow :: m (a, Event t (Workflow t m a)) }

workflow :: forall t m a. (Reflex t, Adjustable t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (Dynamic t a)
workflow w0 = do
  rec eResult <- networkHold (unWorkflow w0) $ fmap unWorkflow $ switch $ snd <$> current eResult
  return $ fmap fst eResult

workflowView :: forall t m a. (Reflex t, NotReady t m, Adjustable t m, MonadFix m, MonadHold t m, PostBuild t m) => Workflow t m a -> m (Event t a)
workflowView w0 = do
  rec eResult <- networkView . fmap unWorkflow =<< holdDyn w0 eReplace
      eReplace <- fmap switch $ hold never $ fmap snd eResult
  return $ fmap fst eResult

mapWorkflow :: (Reflex t, Functor m) => (a -> b) -> Workflow t m a -> Workflow t m b
mapWorkflow f (Workflow x) = Workflow (fmap (f *** fmap (mapWorkflow f)) x)

mapWorkflowCheap :: (Reflex t, Functor m) => (a -> b) -> Workflow t m a -> Workflow t m b
mapWorkflowCheap f (Workflow x) = Workflow (fmap (f *** fmapCheap (mapWorkflowCheap f)) x)
