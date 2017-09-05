{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
#undef ghcjs_HOST_OS
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
#endif
module Data.FastMutableIntMap
  ( FastMutableIntMap
  , new
  , newEmpty
  , insert
  , insertLookup
  , deleteLookup
  , isEmpty
  , getFrozenAndClear
  , size
  , applyPatch
  , PatchIntMap (..)
  , traverseIntMapPatchWithKey
  , lookup
  , forIntersectionWithImmutable_
  , for_
  , patchIntMapNewElements
  , patchIntMapNewElementsMap
  , getDeletions
  ) where

--TODO: Pure JS version
--TODO: Fast copy to FastIntMap
--TODO: Fast patch type

import Prelude hiding (lookup)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
import Reflex.Patch.Class
import Control.Monad.IO.Class

#ifdef ghcjs_HOST_OS
import Control.Lens (imapM_, iforM_, iforM)
import Control.Monad
import GHCJS.Types
import GHCJS.Marshal
import JavaScript.Array (JSArray)
import qualified JavaScript.Array as JSArray
import GHCJS.Prim (JSVal (..))
import GHC.Exts (ByteArray#)
import GHC.Prim (unsafeCoerce#)
#else
import Data.IORef
import Data.Foldable (traverse_)
#endif

newtype FastMutableIntMap a =
#ifdef ghcjs_HOST_OS
  FastMutableIntMap JSVal
#else
  FastMutableIntMap (IORef (IntMap a))
#endif

new :: IntMap a -> IO (FastMutableIntMap a)
#ifdef ghcjs_HOST_OS
new m = do
  result <- newEmpty
  imapM_ (insert result) m
  return result
#else
new m = FastMutableIntMap <$> newIORef m
#endif

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "var a = []; a.__ghcjsArray = true; $r = { f: { n: 'FastMutableIntMap', size: 2 }, d1: a, d2: { d1: 0 }, m: 0 }" newEmpty :: IO (FastMutableIntMap a)
#else
newEmpty :: IO (FastMutableIntMap a)
newEmpty = FastMutableIntMap <$> newIORef IntMap.empty
#endif

insert :: forall a. FastMutableIntMap a -> Int -> a -> IO ()
#ifdef ghcjs_HOST_OS
insert m k !v = insert_ m k ((unsafeCoerce# :: a -> ByteArray#) v)
foreign import javascript unsafe "if($1.d1[$2 & 0x7FFFFFFF] === undefined) { ++$1.d2.d1; } $1.d1[$2 & 0x7FFFFFFF] = $3;" insert_ :: FastMutableIntMap a -> Int -> ByteArray# -> IO ()
#else
insert (FastMutableIntMap r) k v = modifyIORef' r $ IntMap.insert k v
#endif

insertLookup :: forall a. FastMutableIntMap a -> Int -> a -> IO (Maybe a)
#ifdef ghcjs_HOST_OS
insertLookup m k !newVal = do
  oldVal <- insertLookup_ m k ((unsafeCoerce# :: a -> ByteArray#) newVal)
  return $ elemFromJSVal oldVal
foreign import javascript unsafe "$r = $1.d1[$2 & 0x7FFFFFFF]; if($r === undefined) { ++$1.d2.d1; } $1.d1[$2 & 0x7FFFFFFF] = $3;" insertLookup_ :: FastMutableIntMap a -> Int -> ByteArray# -> IO JSVal
#else
insertLookup (FastMutableIntMap r) k v = do
  oldMap <- readIORef r
  let (oldVal, newMap) = IntMap.updateLookupWithKey (\_ _ -> Just v) k oldMap
  writeIORef r $! newMap
  return oldVal
#endif

deleteLookup :: FastMutableIntMap a -> Int -> IO (Maybe a)
#ifdef ghcjs_HOST_OS
deleteLookup m k = do
  v <- deleteLookup_ m k
  return $ elemFromJSVal v
foreign import javascript unsafe "$r = $1.d1[$2 & 0x7FFFFFFF]; if($r !== undefined) { --$1.d2.d1; } delete $1.d1[$2 & 0x7FFFFFFF];" deleteLookup_ :: FastMutableIntMap a -> Int -> IO JSVal
#else
deleteLookup (FastMutableIntMap r) k = do
  oldMap <- readIORef r
  let (oldVal, newMap) = IntMap.updateLookupWithKey (\_ _ -> Nothing) k oldMap
  writeIORef r $! newMap
  return oldVal
#endif

lookup :: forall a. FastMutableIntMap a -> Int -> IO (Maybe a)
#ifdef ghcjs_HOST_OS
lookup m k = do
  v <- lookup_ m k
  return $ elemFromJSVal v
foreign import javascript unsafe "$r = $1.d1[$2 & 0x7FFFFFFF];" lookup_ :: FastMutableIntMap a -> Int -> IO JS

--TODO: Find out whether it's possible for Haskell objects to be represented as 'undefined'; if so, we need to do something slightly better to represent missing values

elemFromJSVal :: JSVal -> Maybe a
elemFromJSVal v = case isUndefined v of
  True -> Nothing
  False -> Just $ (unsafeCoerce# :: ByteArray# -> a) (case v of JSVal raw -> raw)
Val
#else
lookup (FastMutableIntMap r) k = IntMap.lookup k <$> readIORef r
#endif

--TODO: Do something faster than trying every one
{-# INLINE forIntersectionWithImmutable_ #-}
forIntersectionWithImmutable_ :: MonadIO m => FastMutableIntMap a -> IntMap b -> (a -> b -> m ()) -> m ()
#ifdef ghcjs_HOST_OS
forIntersectionWithImmutable_ ma mb f = iforM_ mb $ \k b -> do
  liftIO (lookup ma k) >>= \case
    Nothing -> return ()
    Just a -> f a b
#else
forIntersectionWithImmutable_ (FastMutableIntMap r) b f = do
  a <- liftIO $ readIORef r
  traverse_ (uncurry f) $ IntMap.intersectionWith (,) a b
#endif

{-# INLINE for_ #-}
for_ :: MonadIO m => FastMutableIntMap a -> (a -> m ()) -> m ()
#ifdef ghcjs_HOST_OS
for_ m f = do
  raw <- liftIO $ packFastMutableIntMap m --TODO: Don't pack the keys, since we don't need them
  forM_ (JSArray.toList raw) $ \p -> do
    (_ :: Int, JSVal v) <- liftIO $ fromJSValUnchecked p
    f $ (unsafeCoerce# :: ByteArray# -> a) v
#else
for_ (FastMutableIntMap r) f = do
  a <- liftIO $ readIORef r
  traverse_ f a
#endif

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "$r = $1.d2.d1 === 0;" isEmpty :: FastMutableIntMap a -> IO Bool
#else
isEmpty :: FastMutableIntMap a -> IO Bool
isEmpty (FastMutableIntMap r) = IntMap.null <$> readIORef r
#endif

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "$r = $1.d2.d1;" size :: FastMutableIntMap a -> IO Int
#else
size :: FastMutableIntMap a -> IO Int
size (FastMutableIntMap r) = IntMap.size <$> readIORef r
#endif

-- | Make an immutable snapshot of the datastructure and clear it
getFrozenAndClear :: FastMutableIntMap a -> IO (IntMap a)
#ifdef ghcjs_HOST_OS
getFrozenAndClear m = do
  raw <- packFastMutableIntMap m
  clear m
  pairs <- forM (JSArray.toList raw) $ \p -> do
    (k, JSVal v) <- fromJSValUnchecked p
    return (k, (unsafeCoerce# :: ByteArray# -> a) v)
  return $ IntMap.fromDistinctAscList pairs
-- | Make a copy of a FastMutableIntMap's contents, as an array of key/value pairs
foreign import javascript unsafe "$r = new Array($1.d2.d1); $r.__ghcjsArray = true; var i = 0; for(var k in $1.d1) { if(k !== '__ghcjsArray') { var p = [k, $1.d1[k]]; p.__ghcjsArray = true; $r[i++] = p; } } $r.sort(function(a, b) { return a[0] - b[0]; });" packFastMutableIntMap :: FastMutableIntMap a -> IO JSArray
foreign import javascript unsafe "$1.d1 = []; $1.d1.__ghcjsArray = true; $1.d2.d1 = 0;" clear :: FastMutableIntMap a -> IO ()
--NOTE: We would like to use Array.forEach here, but, at least on Chrome 59, forEach appears to be linear in the maximum key in the array.  Since we sometimes use very large indexes, this is unusable.  for(... in ...), on the other hand, appears to actually operate sparsely - but this requires us to sort the array in a second step.
#else
getFrozenAndClear (FastMutableIntMap r) = do
  result <- readIORef r
  writeIORef r IntMap.empty
  return result
#endif

applyPatch :: FastMutableIntMap a -> PatchIntMap a -> IO (IntMap a)
#ifdef ghcjs_HOST_OS
applyPatch f (PatchIntMap m) = fmap (IntMap.mapMaybe id) $ iforM m $ \k mv -> case mv of
  Nothing -> deleteLookup f k
  Just v -> insertLookup f k v
#else
applyPatch (FastMutableIntMap r) p@(PatchIntMap m) = do
  v <- readIORef r
  writeIORef r $! applyAlways p v
  return $ IntMap.intersection v m
#endif

newtype PatchIntMap a = PatchIntMap (IntMap (Maybe a)) deriving (Functor, Foldable, Traversable, Monoid)

instance Patch (PatchIntMap a) where
  type PatchTarget (PatchIntMap a) = IntMap a
  apply (PatchIntMap p) v = if IntMap.null p then Nothing else Just $
    let removes = IntMap.filter isNothing p
        adds = IntMap.mapMaybe id p
    in IntMap.union adds $ v `IntMap.difference` removes

traverseIntMapPatchWithKey :: Applicative t => (Int -> a -> t b) -> PatchIntMap a -> t (PatchIntMap b)
traverseIntMapPatchWithKey f (PatchIntMap m) = PatchIntMap <$> IntMap.traverseWithKey (\k mv -> traverse (f k) mv) m

patchIntMapNewElements :: PatchIntMap a -> [a]
patchIntMapNewElements (PatchIntMap m) = catMaybes $ IntMap.elems m

patchIntMapNewElementsMap :: PatchIntMap a -> IntMap a
patchIntMapNewElementsMap (PatchIntMap m) = IntMap.mapMaybe id m

getDeletions :: PatchIntMap v -> IntMap v' -> IntMap v'
getDeletions (PatchIntMap m) v = IntMap.intersection v m
