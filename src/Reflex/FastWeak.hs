{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif
module Reflex.FastWeak
  ( FastWeakTicket
  , FastWeak
  , mkFastWeakTicket
  , getFastWeakTicketValue
  , getFastWeakTicketWeak
  , getFastWeakValue
  , getFastWeakTicket
  , emptyFastWeak
#ifdef ghcjs_HOST_OS
  --TODO: Move these elsewhere
  , unsafeFromRawJSVal
  , unsafeToRawJSVal
  , js_isNull
#endif
  ) where

import GHC.Exts (Any)
import Unsafe.Coerce

#ifdef ghcjs_HOST_OS
import GHCJS.Types
#else
import Control.Exception (evaluate)
import System.IO.Unsafe
import System.Mem.Weak
#endif


#ifdef ghcjs_HOST_OS
newtype FastWeakTicket a = FastWeakTicket JSVal

newtype FastWeak a = FastWeak JSVal

-- Just designed to mirror JSVal, so that we can unsafeCoerce between the two
data Val a = Val { unVal :: a }

-- | Coerce a JSVal that represents the heap object of a value of type 'a' into a value of type 'a'
unsafeFromRawJSVal :: JSVal -> a
unsafeFromRawJSVal v = unVal (unsafeCoerce v)

unsafeToRawJSVal :: a -> JSVal
unsafeToRawJSVal v = unsafeCoerce (Val v)
#else
data FastWeakTicket a = FastWeakTicket
  { _fastWeakTicket_val :: !a
  , _fastWeakTicket_weak :: {-# UNPACK #-} !(Weak a)
  }

type FastWeak a = Weak a
#endif

-- This needs to be in IO so we know that we've relinquished the ticket
getFastWeakTicketValue :: FastWeakTicket a -> IO a
#ifdef ghcjs_HOST_OS
getFastWeakTicketValue t = do
  v <- js_ticketVal t
  return $ unsafeFromRawJSVal v

foreign import javascript unsafe "$r = $1.val;" js_ticketVal :: FastWeakTicket a -> IO JSVal
#else
getFastWeakTicketValue = return . _fastWeakTicket_val
#endif

getFastWeakValue :: FastWeak a -> IO (Maybe a)
#ifdef ghcjs_HOST_OS
getFastWeakValue w = do
  r <- js_weakVal w
  case js_isNull r of
    True -> return Nothing
    False -> return $ Just $ unsafeFromRawJSVal r

foreign import javascript unsafe "$1 === null" js_isNull :: JSVal -> Bool

foreign import javascript unsafe "$r = ($1.ticket === null) ? null : $1.ticket.val;" js_weakVal :: FastWeak a -> IO JSVal
#else
getFastWeakValue = deRefWeak
#endif

getFastWeakTicket :: forall a. FastWeak a -> IO (Maybe (FastWeakTicket a))
#ifdef ghcjs_HOST_OS
getFastWeakTicket w = do
  r <- js_weakTicket w
  case js_isNull r of
    True -> return Nothing
    False -> return $ Just $ FastWeakTicket r

foreign import javascript unsafe "$r = $1.ticket;" js_weakTicket :: FastWeak a -> IO JSVal
#else
getFastWeakTicket w = do
  deRefWeak w >>= \case
    Nothing -> return Nothing
    Just v -> return $ Just $ FastWeakTicket
      { _fastWeakTicket_val = v
      , _fastWeakTicket_weak = w
      }
#endif

-- I think it's fine if this is lazy - it'll retain the 'a', but so would the output; we just need to make sure it's forced before we start relying on the associated FastWeak to actually be weak
mkFastWeakTicket :: a -> IO (FastWeakTicket a)
#ifdef ghcjs_HOST_OS
mkFastWeakTicket v = js_fastWeakTicket (unsafeToRawJSVal v)

foreign import javascript unsafe "$r = new h$FastWeakTicket($1);" js_fastWeakTicket :: JSVal -> IO (FastWeakTicket a)
#else
mkFastWeakTicket v = do
  v' <- evaluate v
  w <- mkWeakPtr v' Nothing
  return $ FastWeakTicket
    { _fastWeakTicket_val = v'
    , _fastWeakTicket_weak = w
    }
#endif

-- Needs IO so that it can force the value - otherwise, could end up with a reference to the Ticket, which would retain the value
#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "$r = $1.weak;" getFastWeakTicketWeak :: FastWeakTicket a -> IO (FastWeak a)
#else
getFastWeakTicketWeak :: FastWeakTicket a -> IO (FastWeak a)
getFastWeakTicketWeak = return . _fastWeakTicket_weak
#endif

-- | A weak reference that is always empty
emptyFastWeak :: FastWeak a
emptyFastWeak = unsafeCoerce w
  where w :: FastWeak Any
#ifdef ghcjs_HOST_OS
        w = js_emptyWeak
foreign import javascript unsafe "$r = new h$FastWeak(null);" js_emptyWeak :: FastWeak Any
#else
        w = unsafePerformIO $ do
          w' <- mkWeakPtr undefined Nothing
          finalize w'
          return w'
#endif
{-# NOINLINE emptyFastWeak #-}
