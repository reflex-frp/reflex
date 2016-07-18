module Data.WeakBag (WeakBag, WeakBagTicket, empty, singleton, insert, Data.WeakBag.traverse, remove) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad hiding (forM_, mapM_)
import Control.Monad.IO.Class
import Data.Foldable (forM_, mapM_)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import System.Mem.Weak

import Prelude hiding (mapM_)

data WeakBag a = WeakBag
  { _weakBag_nextId :: {-# UNPACK #-} !(TVar Int) --TODO: what if this wraps around?
  , _weakBag_children :: {-# UNPACK #-} !(TVar (IntMap (Weak a)))
  }

data WeakBagTicket a = WeakBagTicket
  { _weakBagTicket_weakItem :: {-# UNPACK #-} !(Weak a)
  , _weakBagTicket_item :: {-# NOUNPACK #-} !a
  }

{-# INLINE insert #-}
insert :: a -> WeakBag a -> IORef (Weak b) -> (b -> IO ()) -> IO (WeakBagTicket a)
insert a (WeakBag nextId children) wbRef finalizer = do
  a' <- evaluate a
  wbRef' <- evaluate wbRef
  myId <- atomically $ do
    myId <- readTVar nextId
    writeTVar nextId $! succ myId
    return myId
  let cleanup = do
        wb <- readIORef wbRef'
        mb <- deRefWeak wb
        forM_ mb $ \b -> do
          isLastNode <- atomically $ do --TODO: Should this run even when mb is Nothing?
            cs <- readTVar children
            let csWithoutMe = IntMap.delete myId cs
            writeTVar children $! csWithoutMe
            return $ IntMap.size csWithoutMe == 0
          when isLastNode $ finalizer b
          return ()
        return ()
  wa <- mkWeakPtr a' $ Just cleanup
  atomically $ modifyTVar' children $ IntMap.insert myId wa
  return $ WeakBagTicket
    { _weakBagTicket_weakItem = wa
    , _weakBagTicket_item = a'
    }

{-# INLINE empty #-}
empty :: IO (WeakBag a)
empty = do
  nextId <- newTVarIO 1
  children <- newTVarIO IntMap.empty
  let bag = WeakBag
        { _weakBag_nextId = nextId
        , _weakBag_children = children
        }
  return bag

{-# INLINE singleton #-}
singleton :: a -> IORef (Weak b) -> (b -> IO ()) -> IO (WeakBag a, WeakBagTicket a)
singleton a wbRef finalizer = do
  bag <- empty
  ticket <- insert a bag wbRef finalizer
  return (bag, ticket)

{-# INLINE traverse #-}
-- | Visit every node in the given list.  If new nodes are appended during the traversal, they will not be visited.
-- Every live node that was in the list when the traversal began will be visited exactly once; however, no guarantee is made about the order of the traversal.
traverse :: MonadIO m => WeakBag a -> (a -> m ()) -> m ()
traverse (WeakBag _ children) f = do
  cs <- liftIO $ readTVarIO children
  forM_ cs $ \c -> do
    ma <- liftIO $ deRefWeak c
    mapM_ f ma

{-# INLINE remove #-}
remove :: WeakBagTicket a -> IO ()
remove = finalize . _weakBagTicket_weakItem
