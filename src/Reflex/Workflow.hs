{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module:
--   Reflex.Workflow
-- Description:
--   Provides a convenient way to describe a series of interrelated widgets that
--   can send data to, invoke, and replace one another. Useful for modeling user interface
--   "workflows."
module Reflex.Workflow (
    Workflow (..)
  , runWorkflow
  , workflow
  , workflowView
  , mapWorkflow
  , mapWorkflowCheap
  ) where

import Control.Arrow ((***))
import Control.Monad ((<=<))
import Control.Monad.Fix (MonadFix)

import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.PostBuild.Class

-- | A widget in a workflow
-- When the 'Event' returned by a 'Workflow' fires, the current 'Workflow' is replaced by the one inside the firing 'Event'. A series of 'Workflow's must share the same return type.
newtype Workflow t m a = Workflow { unWorkflow :: m (a, Event t (Workflow t m a)) }

-- | Runs a 'Workflow' and returns the initial value together with an 'Event' of the value produced whenever one 'Workflow' is replaced by another.
runWorkflow :: (Adjustable t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (a, Event t a)
runWorkflow w0 = mdo
  ((a, e0), eResult) <- runWithReplace (unWorkflow w0) (fmap unWorkflow eReplace)
  eReplace <- switchHold e0 $ fmap snd eResult
  return (a, fmap fst eResult)

-- | Similar to 'runWorkflow' but combines the result into a 'Dynamic'.
workflow :: (Adjustable t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (Dynamic t a)
workflow = uncurry holdDyn <=< runWorkflow

-- | Similar to 'workflow', but outputs an 'Event' that fires at post-build time and whenever the current 'Workflow' is replaced by the next 'Workflow'.
workflowView :: (Adjustable t m, MonadFix m, MonadHold t m, PostBuild t m) => Workflow t m a -> m (Event t a)
workflowView w = do
  postBuildEv <- getPostBuild
  (initialValue, replaceEv) <- runWorkflow w
  pure $ leftmost [initialValue <$ postBuildEv, replaceEv]

-- | Map a function over a 'Workflow', possibly changing the return type.
mapWorkflow :: (Reflex t, Functor m) => (a -> b) -> Workflow t m a -> Workflow t m b
mapWorkflow f (Workflow x) = Workflow (fmap (f *** fmap (mapWorkflow f)) x)

-- | Map a "cheap" function over a 'Workflow'. Refer to the documentation for 'pushCheap' for more information and performance considerations.
mapWorkflowCheap :: (Reflex t, Functor m) => (a -> b) -> Workflow t m a -> Workflow t m b
mapWorkflowCheap f (Workflow x) = Workflow (fmap (f *** fmapCheap (mapWorkflowCheap f)) x)
