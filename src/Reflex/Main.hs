{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Reflex.Main (runHostWithIO) where

import Data.Dependent.Sum (DSum (..) )
import Control.Concurrent (Chan, readChan, forkIO, ThreadId)
import Reflex.Host.Class
import Reflex.PerformEvent.Base
import Reflex.PostBuild.Base
import Reflex.TriggerEvent.Base
import Data.IORef (readIORef)
import Control.Monad.Ref (Ref, MonadRef)
import Data.Maybe (catMaybes)
import Control.Monad.IO.Class
import Control.Monad (forever)
import Data.Traversable (for)
import Data.Foldable (for_)
import Data.Functor.Identity

type EventChannel t = Chan [DSum (EventTriggerRef t) TriggerInvocation]

-- | Run the common PostBuild / PerformEvent stack, spawning a
-- background thread consuming events. Takes two interpreters; one for
-- the main action, and one for the background events, which may be
-- important for applications where OS threads matter.
--
-- A FireCommand is returned that can be used to fire events manually,
-- which will properly cascade for PerformEvent.
runHostWithIO
  :: forall m t a
   . ( Monad m
     , MonadSubscribeEvent t m
     , MonadReflexHost t m
     , MonadRef (HostFrame t)
     , MonadRef m
     , Ref (HostFrame t) ~ Ref IO
     , Ref m ~ Ref IO
     , MonadIO m
     )
  => EventChannel t -- ^ Events to process in the background
  -> (forall x. m x -> IO x) -- ^ Interpreter for the main action
  -> (forall x. m x -> IO x) -- ^ Interpreter for background events
  -> PostBuildT t (PerformEventT t m) a -- ^ Main action
  -> IO (a, FireCommand t m, ThreadId)
runHostWithIO events runMain runAsync a = do
  (a', fc) <- runMain $ do
    ((a', postBuildTriggerRef), fc@(FireCommand fire)) <- hostPerformEventT $ do
      (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
      a' <- runPostBuildT a postBuild
      return (a', postBuildTriggerRef)

    mPostBuildTrigger <- liftIO $ readIORef postBuildTriggerRef
    for_ mPostBuildTrigger $ \postBuildTrigger ->
      fire [postBuildTrigger :=> Identity ()] $ return ()
    return (a', fc)

  tid <- processAsyncEvents events runAsync fc
  return (a', fc, tid)

processAsyncEvents
  :: (MonadSubscribeEvent t m, MonadReflexHost t m, MonadIO m)
  => EventChannel t
  -> (forall x. m x -> IO x)
  -> FireCommand t m
  -> IO ThreadId
processAsyncEvents events run (FireCommand fire) = forkIO $ forever $ do
  ers <- readChan events
  _ <- run $ do
    mes <-
      liftIO $ for ers $ \(EventTriggerRef er :=> TriggerInvocation a _) ->
        fmap (\ e -> e :=> Identity a) <$> readIORef er
    _ <- fire (catMaybes mes) $ return ()
    liftIO $ for_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
  return ()
