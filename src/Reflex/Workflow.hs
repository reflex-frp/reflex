{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module:
--   Reflex.Workflow
-- Description:
--   Provides a convenient way to describe a series of interrelated widgets that
--   can send data to, invoke, and replace one another. Useful for modeling user interface
--   "workflows".
module Reflex.Workflow (
    Workflow (..)
  , workflow
  , workflowView
  , mapWorkflow
  , mapWorkflowCheap
  , parallelWorkflows
  , zipWorkflows
  , zipWorkflowsWith
  , zipNEListWithWorkflow
  ) where

import Control.Arrow (first, (***))
import Control.Monad.Fix (MonadFix)
import Control.Lens (FunctorWithIndex(..))

import Data.Align
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Functor.Bind
import Data.Functor.Plus
import Data.These

import Prelude hiding (zip)

import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.Network
import Reflex.NotReady.Class
import Reflex.PostBuild.Class

-- | A widget in a workflow
-- When the 'Event' returned by a 'Workflow' fires, the current 'Workflow' is replaced by the one inside the firing 'Event'. A series of 'Workflow's must share the same return type.
newtype Workflow t m a = Workflow { unWorkflow :: m (a, Event t (Workflow t m a)) } deriving Functor

zip :: Apply f => (f a, f b) -> f (a,b)
zip (a,b) = liftF2 (,) a b

-- Prevent `Monad m` constraint from `liftF2` from infecting uses of `pure`
pureW :: (Applicative m, Reflex t) => a -> Workflow t m a
pureW a = Workflow $ pure (a, never)

-- | Create a workflow that's replaced when either input workflow is replaced.
-- The value of the output workflow is obtained by applying the provided function to the values of the input workflows
instance (Apply m, Monad m, Reflex t) => Apply (Workflow t m) where
  liftF2 f wa wb = join $ ffor wa $ \a -> ffor wb $ \b -> f a b

instance (Apply m, Monad m, Reflex t) => Applicative (Workflow t m) where
  pure a = Workflow $ pure (a, never)
  (<*>) = (<.>)

instance (Apply m, Reflex t, Semigroup a) => Semigroup (Workflow t m a) where
  (<>) = zipWorkflowsWith (<>)

instance (Apply m, Applicative m, Reflex t, Monoid a) => Monoid (Workflow t m a) where
  mempty = pureW mempty

-- | Create a workflow that's replaced when either input workflow is replaced.
-- The value of the output workflow is taken from the most-recently replaced input workflow (leftmost wins when simultaneous).
instance (Apply m, Reflex t) => Alt (Workflow t m) where
  (<!>) = parallelWorkflows fst snd fst zip

#if MIN_VERSION_these(0, 8, 0)
instance (Apply m, Reflex t) => Semialign (Workflow t m) where
  align = parallelWorkflows (This . fst) (That . snd) (uncurry These) zip
#endif

zipWorkflows :: (Apply m, Reflex t) => Workflow t m a -> Workflow t m b -> Workflow t m (a,b)
zipWorkflows = zipWorkflowsWith (,)

zipWorkflowsWith :: (Apply m, Reflex t) => (a -> b -> c) -> Workflow t m a -> Workflow t m b -> Workflow t m c
zipWorkflowsWith f = parallelWorkflows f' f' f' zip
  where f' = uncurry f

-- | Combine two independent workflows. The output workflow is replaced when either input is replaced
parallelWorkflows :: (Apply m, Reflex t)
                  => ((a,b) -> c) -- ^ Payload combining function when left workflow is replaced
                  -> ((a,b) -> c) -- ^ Payload combining function when right workflow is replaced
                  -> ((a,b) -> c) -- ^ Payload combining function when both workflows are replaced
                  -> (forall x y. (m x, m y) -> m (x,y)) -- ^ Widget combining function
                  -> Workflow t m a
                  -> Workflow t m b
                  -> Workflow t m c
parallelWorkflows fL fR fLR combineW = go fLR
  where
    go f0 wl wr = Workflow $ ffor (combineW (unWorkflow wl, unWorkflow wr)) $ \((l0, wlEv), (r0, wrEv)) ->
      (f0 (l0, r0), ffor (align wlEv wrEv) $ \case
          This wl' -> go fL wl' wr
          That wr' -> go fR wl wr'
          These wl' wr' -> go fLR wl' wr'
      )

-- | Collapse a workflow of workflows into one level
-- Whenever both outer and inner workflows are replaced at the same time, the inner one is ignored
instance (Apply m, Monad m, Reflex t) => Bind (Workflow t m) where
  join wwa = Workflow $ do
    let replaceInitial a wa = Workflow $ first (const a) <$> unWorkflow wa
    (wa0, wwaEv) <- unWorkflow wwa
    (a0, waEv) <- unWorkflow wa0
    pure (a0, join <$> leftmost [wwaEv, flip replaceInitial wwa <$> waEv])

instance (Apply m, Monad m, Reflex t) => Monad (Workflow t m) where
  (>>=) = (>>-)

zipNEListWithWorkflow :: (Monad m, Reflex t) => NonEmpty k -> Workflow t m a -> Workflow t m (k, a)
zipNEListWithWorkflow (k :| ks) w = Workflow $ do
  (a0, wEv) <- unWorkflow w
  pure ((k, a0), case nonEmpty ks of
           Nothing -> never
           Just nel -> zipNEListWithWorkflow nel <$> wEv)

instance (Monad m, Reflex t) => FunctorWithIndex Int (Workflow t m) where
  imap f w = uncurry f <$> zipNEListWithWorkflow (0 :| [1..]) w

-- | Runs a 'Workflow' and returns the 'Dynamic' result of the 'Workflow' (i.e., a 'Dynamic' of the value produced by the current 'Workflow' node, and whose update 'Event' fires whenever one 'Workflow' is replaced by another).
workflow :: forall t m a. (Reflex t, Adjustable t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (Dynamic t a)
workflow w0 = do
  rec eResult <- networkHold (unWorkflow w0) $ fmap unWorkflow $ switch $ snd <$> current eResult
  return $ fmap fst eResult

-- | Similar to 'workflow', but outputs an 'Event' that fires whenever the current 'Workflow' is replaced by the next 'Workflow'.
workflowView :: forall t m a. (Reflex t, NotReady t m, Adjustable t m, MonadFix m, MonadHold t m, PostBuild t m) => Workflow t m a -> m (Event t a)
workflowView w0 = do
  rec eResult <- networkView . fmap unWorkflow =<< holdDyn w0 eReplace
      eReplace <- fmap switch $ hold never $ fmap snd eResult
  return $ fmap fst eResult

{-# DEPRECATED mapWorkflow "Use 'fmap' instead" #-}
-- | Map a function over a 'Workflow', possibly changing the return type.
mapWorkflow :: (Reflex t, Functor m) => (a -> b) -> Workflow t m a -> Workflow t m b
mapWorkflow = fmap

-- | Map a "cheap" function over a 'Workflow'. Refer to the documentation for 'pushCheap' for more information and performance considerations.
mapWorkflowCheap :: (Reflex t, Functor m) => (a -> b) -> Workflow t m a -> Workflow t m b
mapWorkflowCheap f (Workflow x) = Workflow (fmap (f *** fmapCheap (mapWorkflowCheap f)) x)
