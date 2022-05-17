{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
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
  , workflow
  , workflowView
  , mapWorkflow
  , mapWorkflowCheap
  , runWorkflow

  -- Combinators
  , step
  , stop
  , label
  , label_

  -- Runners
  , stack
  , wizard

  ) where

import Control.Arrow ((***))
import Control.Monad ((<=<))
import Control.Monad.Cont (MonadCont, callCC)
import Control.Monad.Fix (MonadFix, fix)
import Control.Monad.Free.Church
import Data.Bifunctor (bimap)
import Data.Functor.Compose (Compose(..))
import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.PostBuild.Class

--------------------------------------------------------------------------------
-- Workflow
--------------------------------------------------------------------------------
-- | A widget in a workflow
--
-- When the 'Event' returned by a 'Workflow' fires, the current 'Workflow' is replaced by the one inside the firing 'Event'. A series of 'Workflow's must share the same return type.
newtype Workflow t m a = Workflow { unWorkflow :: m (a, Event t (Workflow t m a)) }

-- | Runs a 'Workflow' and returns the initial value together with an 'Event' that fires whenever one 'Workflow' is replaced by another.
runWorkflow :: (Adjustable t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (a, Event t a)
runWorkflow w0 = mdo
  ((a, e0), eResult) <- runWithReplace (unWorkflow w0) (fmap unWorkflow eReplace)
  eReplace <- switchHold e0 $ fmap snd eResult
  return (a, fmap fst eResult)

-- | Similar to 'runWorkflow' but combines the result into a 'Dynamic'.
workflow :: (Adjustable t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (Dynamic t a)
workflow = uncurry holdDyn <=< runWorkflow

-- | Similar to 'runWorkflow', but also puts the initial value in the 'Event'.
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

--------------------------------------------------------------------------------
-- Internal utils
--------------------------------------------------------------------------------
nowOrLater :: PostBuild t m => Either (Event t a) a -> m (Event t a)
nowOrLater = \case
  Left l -> pure l
  Right n -> (n <$) <$> getPostBuild

lateOrLater :: (MonadHold t m, Reflex t) => Event t (Either (Event t a) a) -> m (Event t a)
lateOrLater ev = mdo
  let (ltrEv, lt) = fanEither ev
  ltr <- switchHold never ltrEv
  pure $ leftmost [lt, ltr]

--------------------------------------------------------------------------------
-- Replacements layer
--------------------------------------------------------------------------------
newtype Step t m a = Step { unStep :: m (Either (Event t a) a) }
instance (Reflex t, Functor m) => Functor (Step t m) where
  fmap f = Step . fmap (bimap (fmap f) f) . unStep

runStep :: PostBuild t m => Step t m a -> m (Event t a)
runStep = nowOrLater <=< unStep

newtype Machine t m a = Machine { unMachine :: F (Compose m (Event t)) a } deriving (Functor, Applicative, Monad)

bottomUp
  :: forall t m a. PostBuild t m
  => (forall x. Step t m (Step t m x) -> Step t m x)
  -> Machine t m a -> m (Event t a)
bottomUp f mm = runStep $ runF root leaf branch
  where
    root :: F (Compose m (Event t)) a
    root = unMachine mm

    leaf :: a -> Step t m a
    leaf = Step . pure . Right

    branch :: Compose m (Event t) (Step t m a) -> Step t m a
    branch = f . Step . fmap Left . getCompose

--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------
-- | Machine with a single step
step :: (Functor m, Reflex t) => m (Event t a) -> Machine t m a
step = Machine . wrap . fmap pure . Compose

-- | Machine with a single step and no transitions
stop :: (Reflex t, Applicative m) => Machine t m a
stop = step $ pure never

-- TODO: Use upstream when https://github.com/haskell/mtl/pull/87 is merged
{- | Introduces a recursive binding to the continuation.
Due to the use of @callCC@, calling the continuation will interrupt execution
of the current block creating an effect similar to goto/setjmp in C.
-}
label :: MonadCont m  => a -> m (a -> m b, a)
label a = callCC $ \k -> let go b = k (go, b) in return (go, a)

{- | Simplified version of `label` without arguments -}
label_ :: MonadCont m => m (m a)
label_ = callCC $ return . fix

--------------------------------------------------------------------------------
-- Runners
--------------------------------------------------------------------------------
-- | A wizard only has a single step active at any given point, and any new step replaces its predecessor
wizard :: forall t m a. (Adjustable t m, MonadFix m, MonadHold t m, PostBuild t m) => Machine t m a -> m (Event t a)
wizard = bottomUp $ \m -> Step $ mdo
  (nl, ll) <- runWithReplace (unStep m) (unStep <$> replacement)
  replacement <- nowOrLater nl
  Left <$> lateOrLater ll

-- | A stack can have all steps active at a time, and the first one is always active.
-- When a step triggers, it replaces the (possibly empty) pile on top of itself with a single new step
stack :: forall t m a. (Adjustable t m, MonadHold t m, PostBuild t m) => Machine t m a -> m (Event t a)
stack = bottomUp $ \m -> Step $ do
  replacement <- runStep m
  ((), ll) <- runWithReplace (pure ()) (unStep <$> replacement)
  Left <$> lateOrLater ll
