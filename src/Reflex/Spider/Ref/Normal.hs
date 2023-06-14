{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- We need the redundant constraints in this module to stay consistent with the debug version
module Reflex.Spider.Ref.Normal where

import Reflex.Spider.Ref

import Data.IORef

newtype Ref ctx a = Ref { unRef :: IORef a }

{-# INLINE newRefN #-}
newRefN :: RefName ctx -> a -> IO (Ref ctx a)
newRefN _ a = Ref <$> newIORef a

{-# INLINE toIORef #-}
toIORef :: Ref ctx a -> IORef a
toIORef = unRef

{-# INLINE readRef #-}
readRef :: Ref ctx a -> IO a
readRef = readIORef . unRef

{-# INLINE writeRef #-}
writeRef :: RefCtx ctx => Ref ctx a -> a -> IO ()
writeRef (Ref r) = writeIORef r

{-# INLINE modifyRef' #-}
modifyRef' :: RefCtx ctx => Ref ctx a -> (a -> a) -> IO ()
modifyRef' (Ref r) = modifyIORef' r

{-# INLINE modifyRef #-}
modifyRef :: RefCtx ctx => Ref ctx a -> (a -> a) -> IO ()
modifyRef (Ref r) = modifyIORef r
