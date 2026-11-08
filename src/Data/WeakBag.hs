{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines the 'WeakBag' type, which represents a mutable
-- collection of items that does not cause the items to be retained in memory.
-- This is useful for situations where a value needs to be inspected or modified
-- if it is still alive, but can be ignored if it is dead.
module Data.WeakBag
  ( WeakBag
  , WeakBagTicket
  , empty
  , insert
  , isEmpty
  , traverse
  , traverse_
  , traverseWeakIntMap_
  , remove
  ) where

import Prelude hiding (traverse)

import Control.Exception
import Control.Monad.IO.Class
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Internal (IntMap (Nil, Tip))
import qualified Data.IntMap.Internal as IntMap.Internal
import Data.Functor (void)
import Data.IORef
#if MIN_VERSION_base(4,13,0)
import GHC.IORef (atomicModifyIORef'_)
#endif
import System.Mem.Weak

-- | A 'WeakBag' holds a set of values of type @/a/@, but does not retain them -
-- that is, they can still be garbage-collected.  As long as the @/a/@ values remain
-- alive, the 'WeakBag' will continue to refer to them.
data WeakBag a = WeakBag
  { _weakBag_nextId :: {-# UNPACK #-} !(IORef Int) --TODO: what if this wraps around?
  , _weakBag_children :: {-# UNPACK #-} !(IORef (IntMap (Weak a))) -- ^ The items referenced by the WeakBag
  }

-- | When inserting an item into a 'WeakBag', a 'WeakBagTicket' is returned.  If
-- the caller retains the ticket, the item is guranteed to stay in memory (and
-- thus in the 'WeakBag').  The ticket can also be used to remove the item from
-- the 'WeakBag' prematurely (i.e. while it is still alive), using 'remove'.
data WeakBagTicket = forall a. WeakBagTicket
  { _weakBagTicket_weakItem :: {-# UNPACK #-} !(Weak a)
  , _weakBagTicket_item :: {-# NOUNPACK #-} !a
  }

-- | Insert an item into a 'WeakBag'. The removal of the item, by garbage
-- collection or by 'remove', deletes it from the bag.
{-# INLINE insert #-}
insert :: a -> WeakBag a -> IO WeakBagTicket
insert a (WeakBag nextId children) = {-# SCC "insert" #-} do
  a' <- evaluate a
  myId <- atomicModifyIORef' nextId $ \n -> (succ n, n)
  wa <- mkWeakPtr a' $ Just $ void $ atomicModifyIORef'_ children $ IntMap.delete myId
  void $ atomicModifyIORef'_ children $ IntMap.insert myId wa
  return $ WeakBagTicket
    { _weakBagTicket_weakItem = wa
    , _weakBagTicket_item = a'
    }

#if !MIN_VERSION_base(4,13,0)
-- | 'GHC.IORef.atomicModifyIORef'_', which base < 4.13 does not have.
atomicModifyIORef'_ :: IORef a -> (a -> a) -> IO (a, a)
atomicModifyIORef'_ ref f = atomicModifyIORef' ref $ \old -> let !new = f old in (new, (old, new))
#endif

-- | Create an empty 'WeakBag'.
{-# INLINE empty #-}
empty :: IO (WeakBag a)
empty = {-# SCC "empty" #-} do
  nextId <- newIORef 1
  children <- newIORef IntMap.empty
  let bag = WeakBag
        { _weakBag_nextId = nextId
        , _weakBag_children = children
        }
  return bag

-- | Whether the bag has no entry. The entry of an item that the garbage
-- collector removed is deleted by the finalizer of the item's weak pointer, so
-- the bag can be non-empty for a while after its last live item is gone.
{-# INLINE isEmpty #-}
isEmpty :: WeakBag a -> IO Bool
isEmpty (WeakBag _ children) = {-# SCC "isEmpty" #-} IntMap.null <$> readIORef children

{-# INLINE traverse_ #-}
-- | Visit every node in the given list.  If new nodes are appended during the
-- traversal, they will not be visited.  Every live node that was in the list
-- when the traversal began will be visited exactly once; however, no guarantee
-- is made about the order of the traversal.
traverse_ :: forall a m. MonadIO m => WeakBag a -> (a -> m ()) -> m ()
traverse_ (WeakBag _ children) f = {-# SCC "traverse" #-} do
  cs <- liftIO $ readIORef children
  traverseWeakIntMap_ cs f

{-# INLINE traverseWeakIntMap_ #-}
traverseWeakIntMap_ :: forall a m. MonadIO m => IntMap (Weak a) -> (a -> m ()) -> m ()
traverseWeakIntMap_ items f = go items
  where
    -- This is a very hot function in Reflex and directly recursing over the
    -- internal data structure is a noticable improvement over using forM_:
    go :: IntMap (Weak a) -> m ()
    go (IntMapBin l r) = go l >> go r
    go (Tip _ c) = mapM_ f =<< liftIO (deRefWeak c)
    go Nil = pure ()

pattern IntMapBin :: IntMap a -> IntMap a -> IntMap a
#if MIN_VERSION_containers(0,8,0)
pattern IntMapBin l r <- IntMap.Internal.Bin _ l r
#else
pattern IntMapBin l r <- IntMap.Internal.Bin _ _ l r
#endif

{-# COMPLETE IntMapBin, Tip, Nil #-}

{-# DEPRECATED traverse "Use 'traverse_' instead" #-}
traverse :: MonadIO m => WeakBag a -> (a -> m ()) -> m ()
traverse = traverse_

-- | Remove an item from the 'WeakBag'; does nothing if invoked multiple times
-- on the same 'WeakBagTicket'.
{-# INLINE remove #-}
remove :: WeakBagTicket -> IO ()
remove (WeakBagTicket w _) = {-# SCC "remove" #-} finalize w
--TODO: Should 'remove' also drop the reference to the item?

--TODO: can/should we provide a null WeakBagTicket?
