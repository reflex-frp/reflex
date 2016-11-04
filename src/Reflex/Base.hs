module Reflex.Base where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Ref
import Data.Dependent.Sum
import Data.Foldable
import Data.Functor.Identity
import Data.Maybe
import Data.Traversable

import Reflex.Host.Class
import Reflex.PerformEvent.Base
import Reflex.PostBuild.Class
import Reflex.Spider.Internal
import Reflex.TriggerEvent.Base

type ReflexBase x = PostBuildT (SpiderTimeline x) (TriggerEventT (SpiderTimeline x) (PerformEventT (SpiderTimeline x) (SpiderHost x)))

-- | Runs a program in a given timeline. Returns the result of the
-- program, a firing command for firing events in the program, and the
-- thread id for the event firing thread.
runReflexBaseForTimeline'
  :: HasSpiderTimeline x
  => ReflexBase x a
  -> SpiderTimelineEnv x
  -> IO (a, FireCommand (SpiderTimeline x) (SpiderHost x), ThreadId)
runReflexBaseForTimeline' basic t = do
  events <- newChan
  (result, FireCommand fire) <-
    flip runSpiderHostForTimeline t $ do
      (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
      -- Run the various layers of event logic that exist over 'SpiderHost'
      results@(_, FireCommand fire) <-
        hostPerformEventT
          (runTriggerEventT (runPostBuildT basic postBuild) events)
      -- Read the trigger's ref, traverse into its 'Maybe', and fire the event
      readRef postBuildTriggerRef >>=
        traverse_
          (\postBuildTrigger ->
             fire [postBuildTrigger :=> Identity ()] $ return ())
      return results
  -- Start a thread that fires events forever
  eventThreadId <-
    forkIO . forever $ do
      triggers <- readChan events
      void . flip runSpiderHostForTimeline t $ do
        triggersToFire <-
          liftIO . for triggers $ \(EventTriggerRef tref :=> TriggerInvocation a _) ->
            fmap (\trigger -> trigger :=> Identity a) <$> readRef tref
        void . fire (catMaybes triggersToFire) $ return ()
        liftIO . for_ triggers $ \(_ :=> TriggerInvocation _ cb) -> cb
      return ()
  return (result, FireCommand fire, eventThreadId)

-- | Like 'runReflexBasic'', but discards the less relevant outputs.
runReflexBaseForTimeline
  :: HasSpiderTimeline x
  => ReflexBase x a -> SpiderTimelineEnv x -> IO a
runReflexBaseForTimeline basic =
  fmap (\(a, _, _) -> a) . runReflexBaseForTimeline' basic

runReflexBase'
  :: ReflexBase Global a
  -> IO (a, FireCommand (SpiderTimeline Global) (SpiderHost Global), ThreadId)
runReflexBase' basic = runReflexBaseForTimeline' basic globalSpiderTimelineEnv

runReflexBase :: ReflexBase Global a -> IO a
runReflexBase basic = runReflexBaseForTimeline basic globalSpiderTimelineEnv
