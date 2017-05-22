{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Snapshot where

import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.Ref
import Control.Monad.Trans
import Data.Coerce
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Monoid
import Data.Proxy
import Data.Unique.Tag
import System.IO.Unsafe

import Reflex.Class
import Reflex.Host.Class

data SS t
type M = DMap (Tag RealWorld) Identity

newtype SSPushM t a = SSPushM { unSSPushM :: PushM t a }

deriving instance Functor (PushM t) => Functor (SSPushM t)
deriving instance Applicative (PushM t) => Applicative (SSPushM t)
deriving instance Monad (PushM t) => Monad (SSPushM t)
deriving instance MonadFix (PushM t) => MonadFix (SSPushM t)

{-# NOINLINE newTagFor #-}
newTagFor :: a -> b -> Tag RealWorld c
newTagFor _ _ = unsafePerformIO newTag

ssHold :: (MonadHold t m, Reflex t) => a -> Event (SS t) a -> m (Behavior (SS t) a)
ssHold a fullE@(SSE e reset state) = do
  let tag = newTagFor a fullE
  b <- hold a $ leftmost [e, fmapMaybe (fmap runIdentity . DMap.lookup tag) reset]
  let state' = pull $
        DMap.insert tag <$> (Identity <$> sample b) <*> sample state
  return $ SSB b state'

instance ( MonadHold t (PushM t)
         , Reflex t
         ) => MonadHold (SS t) (SSPushM t) where
  hold a e = SSPushM $ ssHold a e 

instance MonadSample t (PushM t) => MonadSample (SS t) (SSPushM t) where
  sample (SSB b _) = SSPushM $ sample b

newtype SSPullM t a = SSPullM { unSSPullM :: PullM t a }

deriving instance Functor (PullM t) => Functor (SSPullM t)
deriving instance Applicative (PullM t) => Applicative (SSPullM t)
deriving instance Monad (PullM t) => Monad (SSPullM t)

instance MonadSample t (PullM t) => MonadSample (SS t) (SSPullM t) where

instance ( Reflex t
         ) => Reflex (SS t) where
  data Behavior (SS t) a = SSB { _ssb_behavior :: Behavior t a
                               , _ssb_state :: Behavior t M
                               }
  data Event (SS t) a = SSE { _sse_event :: Event t a
                            , _sse_reset :: Event t M
                            , _sse_state :: Behavior t M
                            }
  data Dynamic (SS t) a = SSD { _ssd_dynamic :: Dynamic t a
                              , _ssd_reset :: Event t M
                              , _ssd_state :: Behavior t M
                              }
  type PushM (SS t) = SSPushM t
  type PullM (SS t) = SSPullM t
  push f (SSE e reset state) = SSE (push (unSSPushM . f) e) reset state
  -- switch :: Behavior (SS t) (Event (SS t) a) -> Event (SS t) a
  switch (SSB b state) = SSE
    { _sse_event = switch $ _sse_event <$> b
    -- TODO: Is this the right semantics for switch?
    , _sse_reset = switch $ _sse_reset <$> b
    , _sse_state = pull $ do
        s0 <- sample state
        s1 <- sample . _sse_state =<< sample b
        return $ s0 <> s1
    }

instance Functor (Dynamic t) => Functor (Dynamic (SS t)) where
  fmap f (SSD d e b) = SSD (fmap f d) e b

instance Applicative (Dynamic t) => Applicative (Dynamic (SS t)) where

instance Monad (Dynamic t) => Monad (Dynamic (SS t)) where


newtype SSHost m a = SSHost { unSSHost :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

newtype SSEventTrigger t a = SSEventTrigger { unSSEventTrigger :: EventTrigger t a }
newtype SSHostFrame t a = SSHostFrame { unSSHostFrame :: HostFrame t a }
deriving instance Functor (HostFrame t) => Functor (SSHostFrame t)
deriving instance Applicative (HostFrame t) => Applicative (SSHostFrame t)
deriving instance Monad (HostFrame t) => Monad (SSHostFrame t)
deriving instance MonadFix (HostFrame t) => MonadFix (SSHostFrame t)

newtype SSReadPhase m a = SSReadPhase { unSSReadPhase :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadRef m => MonadRef (SSHost m) where
  type Ref (SSHost m) = Ref m

instance MonadTrans SSHost where
  lift = SSHost

instance ( ReflexHost t
         ) => ReflexHost (SS t) where
  type EventTrigger (SS t) = SSEventTrigger t
  type HostFrame (SS t) = SSHostFrame t

instance MonadReflexCreateTrigger t (HostFrame t) => MonadReflexCreateTrigger (SS t) (SSHostFrame t) where

instance MonadSample t (HostFrame t) => MonadSample (SS t) (SSHostFrame t) where
  sample (SSB b _) = SSHostFrame $ sample b

instance (Reflex t, MonadHold t (HostFrame t)) => MonadHold (SS t) (SSHostFrame t) where
  hold v0 e = SSHostFrame $ ssHold v0 e

instance MonadReadEvent t m => MonadReadEvent (SS t) (SSReadPhase m) where

instance MonadHold t m => MonadHold (SS t) (SSReadPhase m) where

instance MonadSample t m => MonadSample (SS t) (SSReadPhase m) where

instance MonadSubscribeEvent t (HostFrame t) => MonadSubscribeEvent (SS t) (SSHostFrame t) where

instance ( MonadReadEvent t (ReadPhase m)
         , MonadSample t (ReadPhase m)
         , MonadHold t (ReadPhase m)
         , MonadReflexCreateTrigger t m
         , MonadSubscribeEvent t m
         , MonadReflexHost t m
         ) => MonadReflexHost (SS t) (SSHost m) where
  type ReadPhase (SSHost m) = SSReadPhase (ReadPhase m)
  runHostFrame = SSHost . runHostFrame . unSSHostFrame
  fireEventsAndRead triggers (SSReadPhase r) = SSHost $ fireEventsAndRead (coerce triggers) r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger (SS t) (SSHost m) where

instance MonadSubscribeEvent t m => MonadSubscribeEvent (SS t) (SSHost m) where

-- instance (MonadReflexCreateTrigger t m) => MonadReflexCreateTrigger (SS t) (SSHost m) where
--   newEventWithTrigger trigger = do
--     e <- lift $ newEventWithTrigger trigger
--     return $ SSE e never mempty

--   newFanEventWithTrigger = 


-- instance ReflexHost t => ReflexHost (SS t) where
--   type EventTrigger (SS t) = EventTrigger t
