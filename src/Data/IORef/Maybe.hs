{-# LANGUAGE MagicHash, BangPatterns #-}
module Data.IORef.Maybe where

import Data.IORef

import Unsafe.Coerce
import GHC.Prim

import Data.Maybe

newtype IORefMaybe a = IORefMaybe { unIORefMaybe :: IORef Any }

-- | nothingSurrogate stands in for the value Nothing; we distinguish it by pointer
{-# NOINLINE nothingSurrogate #-}
nothingSurrogate :: Any
nothingSurrogate = error "Data.IORef.Maybe.nothingSurrogate evaluated"

{-# INLINE newIORefMaybe #-}
newIORefMaybe :: Maybe a -> IO (IORefMaybe a)
newIORefMaybe ma = case ma of
  Just a -> coerce $ newIORef $ unsafeCoerce a
  Nothing -> coerce $ newIORef nothingSurrogate

{-# INLINE readIORefMaybe #-}
readIORefMaybe :: IORefMaybe a -> IO (Maybe a)
readIORefMaybe (IORefMaybe r) = do
  x <- readIORef r
  return $ toMaybe x

{-# INLINE toMaybe #-}
toMaybe :: Any -> Maybe a
toMaybe x = case reallyUnsafePtrEquality# x nothingSurrogate of
  0# -> Just $ unsafeCoerce x
  _ -> Nothing

{-# INLINE writeIORefMaybe #-}
writeIORefMaybe :: IORefMaybe a -> Maybe a -> IO ()
writeIORefMaybe (IORefMaybe r) ma = case ma of
  Just a -> writeIORef r $ unsafeCoerce a
  Nothing -> writeIORef r nothingSurrogate

{-
--IMPORTANT: The following code does not work; it appears to put an unevaluated thunk into the IORef, which evaluates to nothingSurrogate, but is not reallyUnsafePtrEquality#-equal to it
-- This operation probably does not make sense, since it requires evaluating the input function "atomically", which probably isn't possible
atomicModifyIORefMaybe :: IORefMaybe a -> (Maybe a -> (Maybe a, b)) -> IO b
atomicModifyIORefMaybe (IORefMaybe r) f = atomicModifyIORef r $ \x -> case f (toMaybe x) of
  (Just a', b) -> (unsafeCoerce a', b)
  (Nothing, b) -> (nothingSurrogate, b)
-}

test :: IO ()
test = do
  r1 <- newIORefMaybe (Nothing :: Maybe Int)
  print =<< readIORefMaybe r1
  print =<< readIORefMaybe r1
  writeIORefMaybe r1 (Just 3)
  print =<< readIORefMaybe r1
  writeIORefMaybe r1 (Just undefined)
  print . isJust =<< readIORefMaybe r1
  r2 <- newIORefMaybe (Nothing :: Maybe Int)
  print =<< readIORefMaybe r2
  print =<< readIORefMaybe r2
  r3 <- newIORefMaybe (Nothing :: Maybe Int)
  print =<< readIORefMaybe r3
  print =<< readIORefMaybe r3
  r4 <- newIORefMaybe (Just 5 :: Maybe Int)
  print =<< readIORefMaybe r4
  print =<< readIORefMaybe r4
