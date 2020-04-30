{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Reflex.Host.Headless where

import Data.Maybe
import Reflex
import Control.Monad.Fix
import Control.Monad.Primitive
import Reflex.Host.Class
import Control.Monad.IO.Class
import Control.Monad.Ref
import Data.IORef
import Data.Dependent.Sum (DSum (..))
import Control.Concurrent.Chan (newChan, readChan)
import Control.Monad (forM, forM_)
import Control.Monad.Identity (Identity(..))

type MonadHeadlessApp t m =
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  , PrimMonad (HostFrame t)
  , ReflexHost t
  , MonadIO (HostFrame t)
  , Ref m ~ IORef
  , Ref (HostFrame t) ~ IORef
  , MonadRef (HostFrame t)
  , NotReady t m
  , TriggerEvent t m
  , PostBuild t m
  , PerformEvent t m
  , MonadIO m
  , MonadIO (Performable m)
  , Adjustable t m
  )

-- | Run a headless FRP network. Inside the action, you will most probably use
-- the capabilities provided by the 'TriggerEvent' and 'PerformEvent' type
-- classes to interface the FRP network with the outside world. Useful for
-- testing.
runHeadlessApp
  :: (forall t m. MonadHeadlessApp t m => m (Event t ()))
  -- ^ The action to be run in the headless FRP network. The FRP network is
  -- closed at the first occurrence of the resulting 'Event'.
  -> IO ()
runHeadlessApp guest =
  -- We are using the 'Spider' implementation of reflex. Running the host
  -- allows us to take actions on the FRP timeline. The scoped type signature
  -- specifies that our host runs on the Global timeline.
  -- For more information, see 'Reflex.Spider.Internal.runSpiderHost'.
  (runSpiderHost :: SpiderHost Global a -> IO a) $ do
    -- Create the "post-build" event and associated trigger. This event fires
    -- once, when the application starts.
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    -- Create a queue to which we will write 'Event's that need to be
    -- processed.
    events <- liftIO newChan
    -- Run the "guest" application, providing the appropriate context. We'll
    -- return the result of the action, and a 'FireCommand' that will be used to
    -- trigger events.
    (result, fc@(FireCommand fire)) <- do
      hostPerformEventT $                 -- Allows the guest app to run
                                          -- 'performEvent', so that actions
                                          -- (e.g., IO actions) can be run when
                                          -- 'Event's fire.

        flip runPostBuildT postBuild $    -- Allows the guest app to access to
                                          -- a "post-build" 'Event'

          flip runTriggerEventT events $  -- Allows the guest app to create new
                                          -- events and triggers and write
                                          -- those triggers to a channel from
                                          -- which they will be read and
                                          -- processed.
            guest

    -- Read the trigger reference for the post-build event. This will be
    -- 'Nothing' if the guest application hasn't subscribed to this event.
    mPostBuildTrigger <- readRef postBuildTriggerRef

    -- When there is a subscriber to the post-build event, fire the event.
    forM_ mPostBuildTrigger $ \postBuildTrigger ->
      fire [postBuildTrigger :=> Identity ()] $ return ()

    -- Subscribe to an 'Event' of that the guest application can use to
    -- request application shutdown. We'll check whether this 'Event' is firing
    -- to determine whether to terminate.
    shutdown <- subscribeEvent result

    -- The main application loop. We wait for new events and fire those that
    -- have subscribers. If we detect a shutdown request, the application
    -- terminates.
    fix $ \loop -> do
      -- Read the next event (blocking).
      ers <- liftIO $ readChan events
      stop <- do
        -- Fire events that have subscribers.
        fireEventTriggerRefs fc ers $
          -- Check if the shutdown 'Event' is firing.
          readEvent shutdown >>= \case
            Nothing -> return False
            Just _ -> return True
      if or stop
        then return ()
        else loop
  where
    -- Use the given 'FireCommand' to fire events that have subscribers
    -- and call the callback for the 'TriggerInvocation' of each.
    fireEventTriggerRefs
      :: (MonadIO m)
      => FireCommand t m
      -> [DSum (EventTriggerRef t) TriggerInvocation]
      -> ReadPhase m a
      -> m [a]
    fireEventTriggerRefs (FireCommand fire) ers rcb = do
      mes <- liftIO $
        forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
          me <- readIORef er
          return $ fmap (\e -> e :=> Identity a) me
      a <- fire (catMaybes mes) rcb
      liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
      return a
