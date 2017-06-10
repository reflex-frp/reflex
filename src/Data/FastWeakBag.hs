{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
-- | This module defines the 'FastWeakBag' type, which represents a mutable
-- collection of items that does not cause the items to be retained in memory.
-- This is useful for situations where a value needs to be inspected or modified
-- if it is still alive, but can be ignored if it is dead.
module Data.FastWeakBag
  ( FastWeakBag
  , FastWeakBagTicket
  , empty
  , singleton
  , insert
  , traverse
  , remove
  -- * Internal functions
  -- These will not always be available.
  , _weakBag_children --TODO: Don't export this
  ) where

import Control.Exception
import Control.Monad hiding (forM_, mapM_)
import Control.Monad.IO.Class
import Data.Foldable (forM_, mapM_)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Reflex.FastWeak
import System.Mem.Weak

import Prelude hiding (mapM_, traverse)

-- | A @FastWeakBag a@ holds a set of values of type @a@, but does not retain them -
-- that is, they can still be garbage-collected.  As long as the @a@s remain
-- alive, the 'FastWeakBag' will continue to refer to them.
data FastWeakBag a = FastWeakBag
  { _weakBag_nextId :: {-# UNPACK #-} !(IORef Int) --TODO: what if this wraps around?
  , _weakBag_children :: {-# UNPACK #-} !(IORef (IntMap (Weak a)))
  }

-- | When inserting an item into a 'FastWeakBag', a 'FastWeakBagTicket' is returned.  If
-- the caller retains the ticket, the item is guranteed to stay in memory (and
-- thus in the 'FastWeakBag').  The ticket can also be used to remove the item from
-- the 'FastWeakBag' prematurely (i.e. while it is still alive), using 'remove'.
data FastWeakBagTicket = forall a. FastWeakBagTicket
  { _weakBagTicket_weakItem :: {-# UNPACK #-} !(Weak a)
  , _weakBagTicket_item :: {-# NOUNPACK #-} !a
  }

-- | Insert an item into a 'FastWeakBag'.
{-# INLINE insert #-}
insert :: a -- ^ The item
       -> FastWeakBag a -- ^ The 'FastWeakBag' to insert into
       -> FastWeak b -- ^ An arbitrary value to be used in the following
                             -- callback
       -> (b -> IO ()) -- ^ A callback to be invoked when the item is removed
                       -- (whether automatically by the item being garbage
                       -- collected or manually via 'remove')
       -> IO FastWeakBagTicket -- ^ Returns a 'FastWeakBagTicket' that ensures the item
                           -- is retained and allows the item to be removed.
insert a (FastWeakBag nextId children) wb finalizer = {-# SCC "insert" #-} do
  a' <- evaluate a
  myId <- atomicModifyIORef' nextId $ \n -> (succ n, n)
  let cleanup = do
        mb <- getFastWeakValue wb
        forM_ mb $ \b -> do
          csWithoutMe <- atomicModifyIORef children $ \cs ->
            let !csWithoutMe = IntMap.delete myId cs
            in (csWithoutMe, csWithoutMe)
          when (IntMap.null csWithoutMe) $ finalizer b
          return ()
        return ()
  wa <- mkWeakPtr a' $ Just cleanup
  atomicModifyIORef' children $ \cs -> (IntMap.insert myId wa cs, ())
  return $ FastWeakBagTicket
    { _weakBagTicket_weakItem = wa
    , _weakBagTicket_item = a'
    }

-- | Create an empty 'FastWeakBag'.
{-# INLINE empty #-}
empty :: IO (FastWeakBag a)
empty = {-# SCC "empty" #-} do
  nextId <- newIORef 1
  children <- newIORef IntMap.empty
  let bag = FastWeakBag
        { _weakBag_nextId = nextId
        , _weakBag_children = children
        }
  return bag

-- | Create a 'FastWeakBag' with one item; equivalent to creating the 'FastWeakBag' with
-- 'empty', then using 'insert'.
{-# INLINE singleton #-}
singleton :: a -> FastWeak b -> (b -> IO ()) -> IO (FastWeakBag a, FastWeakBagTicket)
singleton a wb finalizer = {-# SCC "singleton" #-} do
  bag <- empty
  ticket <- insert a bag wb finalizer
  return (bag, ticket)

{-# INLINE traverse #-}
-- | Visit every node in the given list.  If new nodes are appended during the
-- traversal, they will not be visited.  Every live node that was in the list
-- when the traversal began will be visited exactly once; however, no guarantee
-- is made about the order of the traversal.
traverse :: MonadIO m => FastWeakBag a -> (a -> m ()) -> m ()
traverse (FastWeakBag _ children) f = {-# SCC "traverse" #-} do
  cs <- liftIO $ readIORef children
  forM_ cs $ \c -> do
    ma <- liftIO $ deRefWeak c
    mapM_ f ma

-- | Remove an item from the 'FastWeakBag'; does nothing if invoked multiple times
-- on the same 'FastWeakBagTicket'.
{-# INLINE remove #-}
remove :: FastWeakBagTicket -> IO ()
remove (FastWeakBagTicket w _) = {-# SCC "remove" #-} finalize w
--TODO: Should 'remove' also drop the reference to the item?

--TODO: can/should we provide a null FastWeakBagTicket?
