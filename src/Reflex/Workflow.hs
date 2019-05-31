{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
  , independentWorkflows
  , zipWorkflows
  , zipWorkflowsWith
  , zipNEListWithWorkflow
  ) where

import Control.Arrow (first, (***))
import Control.Monad.Fix (MonadFix)
import Control.Lens (FunctorWithIndex(..), set, (&), (.~))

import Data.Align
import Data.Align.Indexed (AlignWithIndex)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Functor.Bind
import Data.Functor.Plus
import Data.These
#if MIN_VERSION_these(0, 8, 0)
import Data.These.Lens (here, there)
#endif
import Data.Tuple (swap)

import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.Network
import Reflex.NotReady.Class
import Reflex.PostBuild.Class

-- | A widget in a workflow
-- When the 'Event' returned by a 'Workflow' fires, the current 'Workflow' is replaced by the one inside the firing 'Event'. A series of 'Workflow's must share the same return type.
newtype Workflow t m a = Workflow { unWorkflow :: m (a, Event t (Workflow t m a)) } deriving Functor

--------------------------------------------------------------------------------
-- Running workflows
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- Transforming workflows
--------------------------------------------------------------------------------
{-# DEPRECATED mapWorkflow "Use 'fmap' instead" #-}
-- | Map a function over a 'Workflow', possibly changing the return type.
mapWorkflow :: (Reflex t, Functor m) => (a -> b) -> Workflow t m a -> Workflow t m b
mapWorkflow = fmap

-- | Map a "cheap" function over a 'Workflow'. Refer to the documentation for 'pushCheap' for more information and performance considerations.
mapWorkflowCheap :: (Reflex t, Functor m) => (a -> b) -> Workflow t m a -> Workflow t m b
mapWorkflowCheap f (Workflow x) = Workflow (fmap (f *** fmapCheap (mapWorkflowCheap f)) x)

zipNEListWithWorkflow :: (Functor m, Reflex t) => (a -> b -> c) -> NonEmpty a -> Workflow t m b -> Workflow t m c
zipNEListWithWorkflow f (a :| as) w = Workflow $ ffor (unWorkflow w) $ \(b0, wEv) ->
  (f a b0, case nonEmpty as of
      Nothing -> never
      Just t -> zipNEListWithWorkflow f t <$> wEv)

instance (Functor m, Reflex t) => FunctorWithIndex Int (Workflow t m) where
  imap f = zipNEListWithWorkflow f (0 :| [1..])

--------------------------------------------------------------------------------
-- Combining occurrences
--------------------------------------------------------------------------------
withLastOccurrences :: (a -> b -> c) -> (a, b) -> These () () -> c
withLastOccurrences f (a,b) _ = f a b

theseOccurrence :: (a,b) -> These () () -> These a b
theseOccurrence (a,b) = set here a . set there b

leftmostOccurrence :: (a,a) -> These () () -> a
leftmostOccurrence (a,b) = \case
  This () -> a
  That () -> b
  These () () -> a

--------------------------------------------------------------------------------
-- Combining widgets
--------------------------------------------------------------------------------
forwardRender :: Apply f => (f a, f b) -> f (a,b)
forwardRender (a,b) = liftF2 (,) a b

backwardRender :: Apply f => (f a, f b) -> f (a,b)
backwardRender = fmap swap . forwardRender . swap

intercalateWidgets :: Apply f => f c -> (f a, f b) -> f (a,b)
intercalateWidgets c (a,b) = (,) <$> a <. c <.> b

--------------------------------------------------------------------------------
-- Combining workflows
--------------------------------------------------------------------------------
instance (Apply m, Reflex t, Semigroup a) => Semigroup (Workflow t m a) where
  (<>) = zipWorkflowsWith (<>)

instance (Apply m, Applicative m, Reflex t, Monoid a) => Monoid (Workflow t m a) where
  mempty = pure mempty

-- | Create a workflow that's replaced when either input workflow is replaced.
-- The value of the output workflow is taken from the most-recently replaced input workflow (leftmost wins when simultaneous).
instance (Apply m, Reflex t) => Alt (Workflow t m) where
  (<!>) = independentWorkflows forwardRender leftmostOccurrence

#if MIN_VERSION_these(0, 8, 0)
instance (Apply m, Reflex t) => Semialign (Workflow t m) where
  align = independentWorkflows forwardRender theseOccurrence
instance (Apply m, Reflex t) => AlignWithIndex Int (Workflow t m)
#endif

-- | Create a workflow that's replaced when either input workflow is replaced.
-- Occurrences of the first workflow cause the second workflow to be reset
instance (Apply m, Reflex t) => Apply (Workflow t m) where
  liftF2 f = hierarchicalWorkflows forwardRender (withLastOccurrences f)

instance (Apply m, Applicative m, Reflex t) => Applicative (Workflow t m) where
  pure a = Workflow $ pure (a, never)
  (<*>) = (<.>)

-- | Combine two workflows via `combineWorkflows`. Triggers of the first workflow reset the second one.
hierarchicalWorkflows
  :: (Functor m, Reflex t)
  => (forall x y. (m x, m y) -> m (x,y))
  -- ^ Compute resulting widget
  -> ((a,b) -> These () () -> c)
  -- ^ Compute resulting payload based on current payloads and ocurrences
  -> Workflow t m a
  -> Workflow t m b
  -> Workflow t m c
hierarchicalWorkflows = combineWorkflows $ \(_, wb0) (wa, _) -> \case
  This wa' -> (wa', wb0)
  That wb' -> (wa, wb')
  These wa' _ -> (wa', wb0)

zipWorkflows :: (Apply m, Reflex t) => Workflow t m a -> Workflow t m b -> Workflow t m (a,b)
zipWorkflows = zipWorkflowsWith (,)

-- | Create a workflow that's replaced when either input workflow is replaced.
-- The value of the output workflow is obtained by applying the provided function to the values of the input workflows
zipWorkflowsWith :: (Apply m, Reflex t) => (a -> b -> c) -> Workflow t m a -> Workflow t m b -> Workflow t m c
zipWorkflowsWith f = independentWorkflows forwardRender (withLastOccurrences f)

-- | Combine two workflows via `combineWorkflows`. Triggers of one workflow do not affect the other one.
independentWorkflows
  :: (Functor m, Reflex t)
  => (forall x y. (m x, m y) -> m (x,y))
  -- ^ Compute resulting widget
  -> ((a,b) -> These () () -> c)
  -- ^ Compute resulting payload based on current payloads and ocurrences
  -> Workflow t m a
  -> Workflow t m b
  -> Workflow t m c
independentWorkflows = combineWorkflows $ \(_, _) (wa, wb) -> fromThese wa wb

-- | Combine two workflows. The output workflow triggers when either input triggers
combineWorkflows
  :: (Functor m, Reflex t, w ~ Workflow t m)
  => (forall x y. (w x, w y) -> (w x, w y) -> These (w x) (w y) -> (w x, w y))
  -- ^ Choose the resulting workflows among original, current, and ocurring workflows
  -> (forall x y. (m x, m y) -> m (x,y))
  -- ^ Compute resulting widget
  -> ((a,b) -> These () () -> c)
  -- ^ Compute resulting payload based on current payloads and ocurrences
  -> w a
  -> w b
  -> w c
combineWorkflows triggerWorflows combineWidgets combineOccurrence wa0 wb0 = go (These () ()) (wa0, wb0)
  where
    go occurring (wa, wb) = Workflow $ ffor (combineWidgets (unWorkflow wa, unWorkflow wb)) $ \((a0, waEv), (b0, wbEv)) ->
      ( combineOccurrence (a0, b0) occurring
      , ffor (align waEv wbEv) $ \tw ->
          go (tw & here .~ () & there .~ ()) (triggerWorflows (wa0, wb0) (wa, wb) tw)
      )

--------------------------------------------------------------------------------
-- Flattening workflows
--------------------------------------------------------------------------------
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
