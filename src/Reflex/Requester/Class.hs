-- | This module defines 'Requester', which indicates that an action can make
-- requests and receive responses to them.  Typically, this is used for things
-- like a WebSocket, where it's desirable to collect many potential sources of
-- events and send them over a single channel, then distribute the results back
-- out efficiently to their original request sites.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.Requester.Class
 ( Requester (..)
 , requesting
 , requesting_
 , requestingIdentity
 ) where

import Control.Monad.Identity
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict
import Reflex.Class

-- | A 'Requester' action can trigger requests of type @Request m a@ based on
-- 'Event's, and receive responses of type @Response m a@ in return.  Note that
-- the @a@ type can vary within the 'Requester' action, but will be linked for a
-- given request.  For example, if @Request m@ is 'IO' and @Response m@ is
-- 'Identity', then 'requestingIdentity' has the same type as
-- 'Reflex.PerformEvent.Class.performEvent'.
class (Reflex t, Monad m) => Requester t m | m -> t where
  -- | The type of requests that this 'Requester' can emit
  type Request m :: * -> *
  -- | The type of responses that this 'Requester' can receive
  type Response m :: * -> *
  -- | Usually, end-user code should use 'requesting', 'requesting_', or
  -- 'requestingIdentity'.  For implementers of 'Requester', this function
  -- provides a convenient way to define instances of the typeclass that
  -- efficiently support the underlying operations without needing to use
  -- 'Control.Monad.Fix.MonadFix'.
  withRequesting :: (Event t (Response m a) -> m (Event t (Request m a), r)) -> m r

instance Requester t m => Requester t (ReaderT r m) where
  type Request (ReaderT r m) = Request m
  type Response (ReaderT r m) = Response m
  withRequesting f = do
    r <- ask
    lift $ withRequesting $ (`runReaderT` r) . f

instance Requester t m => Requester t (StateT s m) where
  type Request (StateT s m) = Request m
  type Response (StateT s m) = Response m
  withRequesting f = do
    old <- get
    (result, new) <- lift $ withRequesting $ \x -> do
      ((request, result), new) <- runStateT (f x) old
      return (request, (result, new))
    put new
    return result

instance Requester t m => Requester t (Lazy.StateT s m) where
  type Request (Lazy.StateT s m) = Request m
  type Response (Lazy.StateT s m) = Response m
  withRequesting f = do
    old <- Lazy.get
    (result, new) <- lift $ withRequesting $ \x -> do
      ((request, result), new) <- Lazy.runStateT (f x) old
      return (request, (result, new))
    Lazy.put new
    return result

-- | Emit a request whenever the given 'Event' fires, and return responses in
-- the resulting 'Event'.
requesting :: Requester t m => Event t (Request m a) -> m (Event t (Response m a))
requesting req = withRequesting $ return . (,) req

-- | Emit a request whenever the given 'Event' fires, and ignore all responses.
requesting_ :: Requester t m => Event t (Request m a) -> m ()
requesting_ req = withRequesting $ \_ -> return (req, ())

-- | Emit a request whenever the given 'Event' fires, and unwrap the responses
-- before returning them.  @Response m@ must be 'Identity'.
requestingIdentity :: (Requester t m, Response m ~ Identity) => Event t (Request m a) -> m (Event t a)
requestingIdentity = fmap (fmap runIdentity) . requesting
