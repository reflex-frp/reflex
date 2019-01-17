{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.Network
  ( networkView
  , networkHold
  , untilReady
  ) where

import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.NotReady.Class
import Reflex.PostBuild.Class

-- | Given a Dynamic of network-creating actions, create a network that is recreated whenever the Dynamic updates.
--   The returned Event of network results occurs when the Dynamic does.
--   Note:  Often, the type 'a' is an Event, in which case the return value is an Event-of-Events that would typically be flattened (via 'switchPromptly').
networkView :: (NotReady t m, Adjustable t m, PostBuild t m) => Dynamic t (m a) -> m (Event t a)
networkView child = do
  postBuild <- getPostBuild
  let newChild = leftmost [updated child, tagCheap (current child) postBuild]
  snd <$> runWithReplace notReady newChild

-- | Given an initial network and an Event of network-creating actions, create a network that is recreated whenever the Event fires.
--   The returned Dynamic of network results occurs when the Event does.
--   Note:  Often, the type 'a' is an Event, in which case the return value is a Dynamic-of-Events that would typically be flattened.
networkHold :: (Adjustable t m, MonadHold t m) => m a -> Event t (m a) -> m (Dynamic t a)
networkHold child0 newChild = do
  (result0, newResult) <- runWithReplace child0 newChild
  holdDyn result0 newResult

-- | Render a placeholder network to be shown while another network is not yet
-- done building
untilReady :: (Adjustable t m, PostBuild t m) => m a -> m b -> m (a, Event t b)
untilReady a b = do
  postBuild <- getPostBuild
  runWithReplace a $ b <$ postBuild
