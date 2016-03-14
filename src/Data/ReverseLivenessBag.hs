module Data.ReverseLivenessBag where

import Data.IORef
import Data.Bits
import Data.Atomics
import Data.Maybe
import System.Mem.Weak
import Control.Monad
import Control.Monad.Primitive
import Control.Exception

import System.IO.Unsafe

data ReverseLivenessBagMember e a
   = ReverseLivenessBagMember { _reverseLivenessBagMember_bags :: !(IORef [ReverseLivenessBag e a])
                              , _reverseLivenessBagMember_weakValue :: !(IORef (Weak a))
                              , _reverseLivenessBagMember_value :: {-# NOUNPACK #-} a
                              }

data ReverseLivenessBag e a
   = ReverseLivenessBag { _reverseLivenessBag_children :: !(IORef [Weak a])
                        , _reverseLivenessBag_totalChildren :: !(IORef Int) -- ^ The total number of children added to the bag minus the number known to have been expunged; this can be larger than the number actually in the bag, e.g. if the bag is cleaned before all dead children increment the deadChildren counter
                        , _reverseLivenessBag_deadChildren :: !(IORef Int)
                        , _reverseLivenessBag_extra :: {-# NOUNPACK #-} e
                        }

newEmptyReverseLivenessBag :: e -> IO (ReverseLivenessBag e a)
newEmptyReverseLivenessBag e = ReverseLivenessBag <$> newIORef [] <*> newIORef 0 <*> newIORef 0 <*> pure e

newReverseLivenessBag :: e -> [ReverseLivenessBagMember e a] -> IO (ReverseLivenessBag e a)
newReverseLivenessBag e members = do
  weaks <- mapM (readIORef . _reverseLivenessBagMember_weakValue) members
  b <- ReverseLivenessBag
       <$> newIORef weaks
       <*> newIORef (length weaks)
       <*> newIORef 0 -- None of the things we're dealing with can be dead, because we touch them right after this
       <*> pure e
  forM_ members $ \m -> modifyIORef' (_reverseLivenessBagMember_bags m) (b:)
  touch members
  return b

--readReverseLivenessBag :: ReverseLivenessBag e a -> IO [Weak a]
--readReverseLivenessBag = readIORef . _reverseLivenessBag_children

-- | Simultaneously retrieve everything from a bag, clear its contents, and reset all its Members (i.e. remove them from all *other* bags of which they are members)
dumpAndResetReverseLivenessBag :: ReverseLivenessBag e a -> IO [a]
dumpAndResetReverseLivenessBag b = do
  children <- atomicModifyIORefCAS (_reverseLivenessBag_children b) $ \c -> ([], c)
  writeIORef (_reverseLivenessBag_totalChildren b) 0 --TODO: This isn't quite right: if a child is added after the last line executes, this number should be higher
  writeIORef (_reverseLivenessBag_deadChildren b) 0 --TODO: This one isn't quite right either
  fmap catMaybes $ forM children $ \wc -> do
    mc <- deRefWeak wc
    finalize wc
    return mc

insertReverseLivenessBag :: ReverseLivenessBag e a -> ReverseLivenessBagMember e a -> IO ()
insertReverseLivenessBag b m = do
  w <- readIORef $ _reverseLivenessBagMember_weakValue m
  modifyIORef' (_reverseLivenessBag_children b) (w:)
  modifyIORef' (_reverseLivenessBag_totalChildren b) (+1)
  modifyIORef' (_reverseLivenessBagMember_bags m) (b:)

--TODO: Is it faster to clean up every time, or to occasionally go through and clean up as needed?
traverseAndCleanWeakList_ :: Monad m => (wa -> m (Maybe a)) -> [wa] -> (a -> m ()) -> m [wa]
traverseAndCleanWeakList_ deRef ws f = go ws
  where go [] = return []
        go (h:t) = do
          ma <- deRef h
          case ma of
            Just a -> do
              f a
              t' <- go t
              return $ h : t'
            Nothing -> go t

unsafeCleanWeakList :: [Weak a] -> [Weak a]
unsafeCleanWeakList l = case l of
  [] -> []
  h : t -> if isJust $ unsafeDupablePerformIO $ deRefWeak h
           then h : unsafeCleanWeakList t
           else unsafeCleanWeakList t

-- | A simpler version that modifies the state but does not return anything.
atomicModifyIORefCASLazy_ :: IORef t -> (t -> t) -> IO ()
atomicModifyIORefCASLazy_ ref fn = do
   tick <- readForCAS ref
   loop tick effort
  where
   effort = 30 :: Int -- TODO: Tune this.
   loop _   0     = atomicModifyIORef ref (\ x -> (fn x, ()))
   loop old tries = do
     let new = fn $ peekTicket old
     (b,val) <- casIORef ref old new
     if b
      then return ()
      else loop val (tries-1)

bagMemberFinalizer :: IORef [ReverseLivenessBag e a] -> IO ()
bagMemberFinalizer bagsRef = do
  bags <- atomicModifyIORefCAS bagsRef $ \b -> ([], b)
  forM_ bags $ \b -> do
    dead <- atomicModifyIORefCAS (_reverseLivenessBag_deadChildren b) $ \oldDead -> let newDead = oldDead + 1 in (newDead, newDead)
    total <- readIORef $ _reverseLivenessBag_totalChildren b
    let maxDead = total `shift` (-1)
    let cleanup = do
          atomicModifyIORefCASLazy_ (_reverseLivenessBag_children b) unsafeCleanWeakList
          _ <- evaluate . length =<< readIORef (_reverseLivenessBag_children b)
          atomicModifyIORefCAS_ (_reverseLivenessBag_totalChildren b) $ subtract dead
          writeIORef (_reverseLivenessBag_deadChildren b) 0
    when (dead >= maxDead) cleanup

newReverseLivenessBagMember :: a -> IO (ReverseLivenessBagMember e a)
newReverseLivenessBagMember a = do
  bagsRef <- newIORef []
  w <- newIORef <=< mkWeakPtr a $ Just $ bagMemberFinalizer bagsRef
  return $ ReverseLivenessBagMember bagsRef w a

-- | Remove this member from all bags that contain it
resetReverseLivenessBagMember :: ReverseLivenessBagMember e a -> IO ()
resetReverseLivenessBagMember m = do
  finalize =<< readIORef (_reverseLivenessBagMember_weakValue m)
  writeIORef (_reverseLivenessBagMember_weakValue m) <=< mkWeakPtr (_reverseLivenessBagMember_value m) $ Just $ bagMemberFinalizer $ _reverseLivenessBagMember_bags m -- We only need to do this in the case of explicit finalization; if the weak reference is *implicitly* finalized, that means that the ReverseLivenessBagMember must also be dead, since the Member contains a reference to the value
