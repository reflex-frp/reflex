{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif
-- | This module defines the 'FastWeakBag' type, which represents a mutable
-- collection of items that does not cause the items to be retained in memory.
-- This is useful for situations where a value needs to be inspected or modified
-- if it is still alive, but can be ignored if it is dead.
module Data.FastWeakBag
  ( FastWeakBag
  , FastWeakBagTicket
  , empty
  , isEmpty
  , insert
  , traverse
  , remove
  -- * Internal functions
  -- These will not always be available.
#ifndef ghcjs_HOST_OS
  , _weakBag_children --TODO: Don't export this
#endif
  ) where

import Control.Monad hiding (forM_, mapM_)
import Control.Monad.IO.Class

#ifdef ghcjs_HOST_OS
import GHCJS.Types
import Reflex.FastWeak (unsafeFromRawJSVal, unsafeToRawJSVal, js_isNull)
#else
import Control.Exception
import Data.Foldable (forM_, mapM_)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import System.Mem.Weak
#endif

import Prelude hiding (mapM_, traverse)

-- | A @FastWeakBag a@ holds a set of values of type @a@, but does not retain them -
-- that is, they can still be garbage-collected.  As long as the @a@s remain
-- alive, the 'FastWeakBag' will continue to refer to them.
#ifdef ghcjs_HOST_OS
newtype FastWeakBag a = FastWeakBag JSVal
#else
data FastWeakBag a = FastWeakBag
  { _weakBag_nextId :: {-# UNPACK #-} !(IORef Int) --TODO: what if this wraps around?
  , _weakBag_children :: {-# UNPACK #-} !(IORef (IntMap (Weak a)))
  }
#endif

-- | When inserting an item into a 'FastWeakBag', a 'FastWeakBagTicket' is returned.  If
-- the caller retains the ticket, the item is guranteed to stay in memory (and
-- thus in the 'FastWeakBag').  The ticket can also be used to remove the item from
-- the 'FastWeakBag' prematurely (i.e. while it is still alive), using 'remove'.
#ifdef ghcjs_HOST_OS
newtype FastWeakBagTicket a = FastWeakBagTicket JSVal
#else
data FastWeakBagTicket a = FastWeakBagTicket
  { _weakBagTicket_weakItem :: {-# UNPACK #-} !(Weak a)
  , _weakBagTicket_item :: {-# NOUNPACK #-} !a
  }
#endif

-- | Insert an item into a 'FastWeakBag'.
{-# INLINE insert #-}
insert :: a -- ^ The item
       -> FastWeakBag a -- ^ The 'FastWeakBag' to insert into
       -> IO (FastWeakBagTicket a) -- ^ Returns a 'FastWeakBagTicket' that ensures the item
                           -- is retained and allows the item to be removed.
#ifdef ghcjs_HOST_OS
insert a wb = js_insert (unsafeToRawJSVal a) wb
foreign import javascript unsafe "$r = new h$FastWeakBagTicket($2, $1);" js_insert :: JSVal -> FastWeakBag a -> IO (FastWeakBagTicket a)
#else
insert a (FastWeakBag nextId children) = {-# SCC "insert" #-} do
  a' <- evaluate a
  myId <- atomicModifyIORef' nextId $ \n -> (succ n, n)
  let cleanup = atomicModifyIORef' children $ \cs -> (IntMap.delete myId cs, ())
  wa <- mkWeakPtr a' $ Just cleanup
  atomicModifyIORef' children $ \cs -> (IntMap.insert myId wa cs, ())
  return $ FastWeakBagTicket
    { _weakBagTicket_weakItem = wa
    , _weakBagTicket_item = a'
    }
#endif

-- | Create an empty 'FastWeakBag'.
{-# INLINE empty #-}
empty :: IO (FastWeakBag a)
#ifdef ghcjs_HOST_OS
empty = js_empty
foreign import javascript unsafe "$r = new h$FastWeakBag();" js_empty :: IO (FastWeakBag a)
#else
empty = {-# SCC "empty" #-} do
  nextId <- newIORef 1
  children <- newIORef IntMap.empty
  let bag = FastWeakBag
        { _weakBag_nextId = nextId
        , _weakBag_children = children
        }
  return bag
#endif

-- | Check whether a 'FastWeakBag' is empty.
{-# INLINE isEmpty #-}
isEmpty :: FastWeakBag a -> IO Bool
#ifdef ghcjs_HOST_OS
isEmpty = js_isEmpty
foreign import javascript unsafe "(function(){ for(var i = 0; i < $1.tickets.length; i++) { if($1.tickets[i] !== null) { return false; } }; return true; })()" js_isEmpty :: FastWeakBag a -> IO Bool --TODO: Clean up as we go along so this isn't O(n) every time
#else
isEmpty bag = {-# SCC "isEmpty" #-} IntMap.null <$> readIORef (_weakBag_children bag)
#endif

{-# INLINE traverse #-}
-- | Visit every node in the given list.  If new nodes are appended during the
-- traversal, they will not be visited.  Every live node that was in the list
-- when the traversal began will be visited exactly once; however, no guarantee
-- is made about the order of the traversal.
traverse :: forall a m. MonadIO m => FastWeakBag a -> (a -> m ()) -> m ()
#ifdef ghcjs_HOST_OS
traverse wb f = do
  let go cursor = when (not $ js_isNull cursor) $ do
        val <- liftIO $ js_getTicketValue cursor
        f $ unsafeFromRawJSVal val
        go =<< liftIO (js_getNext (FastWeakBagTicket cursor))
  go =<< liftIO (js_getInitial wb)
foreign import javascript unsafe "(function(){ for(var i = $1.tickets.length - 1; i >= 0; i--) { if($1.tickets[i] !== null) { return $1.tickets[i]; } }; return null; })()" js_getInitial :: FastWeakBag a -> IO JSVal --TODO: Clean up as we go along so this isn't O(n) every time -- Result can be null or a FastWeakBagTicket a
foreign import javascript unsafe "$r = $1.val;" js_getTicketValue :: JSVal -> IO JSVal
--TODO: Fix the race condition where if a cursor is deleted (presumably using 'remove', below) while we're holding it, it can't find its way back to the correct bag
foreign import javascript unsafe "(function(){ for(var i = $1.pos - 1; i >= 0; i--) { if($1.bag.tickets[i] !== null) { return $1.bag.tickets[i]; } }; return null; })()" js_getNext :: FastWeakBagTicket a -> IO JSVal --TODO: Clean up as we go along so this isn't O(n) every time -- Result can be null or a FastWeakBagTicket a
#else
traverse (FastWeakBag _ children) f = {-# SCC "traverse" #-} do
  cs <- liftIO $ readIORef children
  forM_ cs $ \c -> do
    ma <- liftIO $ deRefWeak c
    mapM_ f ma
#endif

-- | Remove an item from the 'FastWeakBag'; does nothing if invoked multiple times
-- on the same 'FastWeakBagTicket'.
{-# INLINE remove #-}
remove :: FastWeakBagTicket a -> IO ()
#ifdef ghcjs_HOST_OS
remove = js_remove
foreign import javascript unsafe "$1.bag.tickets[$1.pos] = null; $1.bag = new h$FastWeakBag(); $1.bag.tickets.push($1); $1.pos = 0;" js_remove :: FastWeakBagTicket a -> IO () --TODO: Don't bother with the new surrogate FastWeakBag; instead, make the GC check for bag === null, and then null it out here
#else
remove (FastWeakBagTicket w _) = {-# SCC "remove" #-} finalize w
#endif
--TODO: Should 'remove' also drop the reference to the item?

--TODO: can/should we provide a null FastWeakBagTicket?
