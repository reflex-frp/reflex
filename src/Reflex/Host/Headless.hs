{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Reflex.Host.Headless where

import Control.Concurrent.Chan (newChan, readChan)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Fix (MonadFix, fix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Ref (MonadRef, Ref)
import Data.Dependent.Sum (DSum (..), (==>))
import Data.Foldable (for_)
import Data.IORef (IORef, readIORef)
import Data.Maybe (catMaybes)
import Data.Traversable (for)

import Reflex
import Reflex.Host.Class

type MonadHeadlessApp t m =
  ( Reflex t
  , Adjustable t m
  , MonadCatch m
  , MonadFix (Performable m)
  , MonadFix m
  , MonadHold t (Performable m)
  , MonadHold t m
  , MonadIO (HostFrame t)
  , MonadIO (Performable m)
  , MonadIO m
  , MonadMask m
  , MonadRef (HostFrame t)
  , MonadSample t (Performable m)
  , MonadSample t m
  , MonadThrow m
  , MonadThrow (Performable m)
  , NotReady t m
  , PerformEvent t m
  , PostBuild t m
  , PrimMonad (HostFrame t)
  , Ref (HostFrame t) ~ IORef
  , Ref m ~ IORef
  , ReflexHost t
  , TriggerEvent t m
  )

-- | Run a headless FRP network. Inside the action, you will most probably use
-- the capabilities provided by the 'TriggerEvent' and 'PerformEvent' type
-- classes to interface the FRP network with the outside world. Useful for
-- testing. Each headless network runs on its own spider timeline.
runHeadlessApp
  :: forall a
  .  (forall t m. MonadHeadlessApp t m => m (Event t a))
  -- ^ The action to be run in the headless FRP network. The FRP network is
  -- closed at the first occurrence of the resulting 'Event'.
  -> IO a
runHeadlessApp guest =
  withSpiderTimeline $ runSpiderHostForTimeline $ do
    -- Queue of externally-triggered events awaiting processing.
    events <- liftIO newChan
    -- Fold used both for the initial settle and for every event frame: stop as
    -- soon as the guest's shutdown 'Event' occurs, otherwise keep draining.
    let untilShutdown handle () = do
          mExit <- sequence =<< readEvent handle
          pure $ maybe (Right ()) Left mExit
    (_, shutdownEventHandle, shutdownImmediately, fc) <- hostPerformEventTAndRead
          ( runPostBuildT   -- guest can access the post-build 'Event'
          . flip runTriggerEventT events   -- guest can create triggers, queued to 'events'
          $ guest )
          subscribeEvent
          ()
          untilShutdown
    case shutdownImmediately of
      Left exitResult -> pure exitResult
      -- Main loop: block for the next external event, fire it, perform the effects
      -- aborting the moment the shutdown 'Event' occurs.
      Right () -> fix $ \loop -> do
        ers <- liftIO $ readChan events
        mes <- liftIO $
          for ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
            me <- readIORef er
            pure $ fmap (==> a) me
        mExit <- runFireCommandAndRead fc (catMaybes mes) () (untilShutdown shutdownEventHandle)
        liftIO $ for_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
        either pure (const loop) mExit
