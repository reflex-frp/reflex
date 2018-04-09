{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Workflow (
  -- * Workflows
    Workflow (..)
  , workflow
  , workflowView
  , mapWorkflow
  , mapWorkflowCheap
  -- * Better workflows
  , W (..)
  , runW
  , prompt
  ) where

import Control.Arrow ((***))
import Control.Monad.Fix (MonadFix)
import Control.Monad.Free.Church
import Data.Functor.Compose

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

--------------------------------------------------------------------------------
-- Workflow monad
--------------------------------------------------------------------------------

type WInternal t m = F (Compose m (Event t))
newtype W t m a = W { unW :: WInternal t m a } deriving (Functor, Applicative, Monad)

runW :: forall t m a. (Adjustable t m, MonadHold t m, MonadFix m, PostBuild t m) => W t m a -> m (Event t a)
runW (W w0) = do
  let go :: WInternal t m a -> m (Event t (WInternal t m a))
      go w = runF w
        (\l -> (return l <$) <$> getPostBuild) --TODO: Can this just be blank?
        (\(Compose r) -> fmap (fmapCheap (wrap . Compose)) r)
  rec (next0, built) <- runWithReplace (go w0) $ go <$> next
      next <- switch <$> hold next0 built
  return $ fmapMaybe (\w -> runF w Just (const Nothing)) next

prompt :: (Reflex t, Functor m) => m (Event t a) -> W t m a
prompt = W . wrap . fmap return . Compose
