module Reflex.Spider.Ref.Debug where

import Data.IORef

import Reflex.Spider.Ref.Types

data Ref ctx a = Ref
  { _ref_name :: RefName ctx
  , _ref_r :: {-# UNPACK #-} !(IORef a)
  }

{-# INLINE newRefN #-}
newRefN :: RefName ctx -> a -> IO (Ref ctx a)
newRefN name v = do
  r <- newIORef v
  pure $ Ref
    { _ref_name = name
    , _ref_r = r
    }

{-# INLINE toIORef #-}
toIORef :: Ref ctx a -> IORef a
toIORef = _ref_r

{-# INLINE readRef #-}
readRef :: Ref ctx a -> IO a
readRef = readIORef . _ref_r

{-# INLINE writeRef #-}
writeRef :: RefCtx ctx => Ref ctx a -> a -> IO ()
writeRef r v = do
  traceRef (_ref_name r) RefAction_Write
  writeIORef (_ref_r r) v

{-# INLINE modifyRef' #-}
modifyRef' :: RefCtx ctx => Ref ctx a -> (a -> a) -> IO ()
modifyRef' r f = do
  traceRef (_ref_name r) RefAction_Modify'
  modifyIORef' (_ref_r r) f

{-# INLINE modifyRef #-}
modifyRef :: RefCtx ctx => Ref ctx a -> (a -> a) -> IO ()
modifyRef r f = do
  traceRef (_ref_name r) RefAction_Modify
  modifyIORef (_ref_r r) f
