{-# LANGUAGE CPP #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif
module Reflex.FastWeak
  ( FastWeakTicket
  , FastWeak
  , mkFastWeakTicket
  , fastWeakTicketValue
  , getFastWeakTicketWeak
  , getFastWeakValue
  ) where

#ifdef ghcjs_HOST_OS
import GHCJS.Types
import Unsafe.Coerce
#else
import Control.Exception (evaluate)
import System.Mem.Weak
#endif


#ifdef ghcjs_HOST_OS
newtype FastWeakTicket a = FastWeakTicket JSVal

newtype FastWeak a = FastWeak JSVal

-- Just designed to mirror JSVal, so that we can unsafeCoerce between the two
data Val a = Val { unVal :: a }
#else
data FastWeakTicket a = FastWeakTicket
  { _fastWeakTicket_val :: !a
  , _fastWeakTicket_weak :: {-# UNPACK #-} !(Weak a)
  }

type FastWeak a = Weak a
#endif

-- Do we have a concern about accidentally retaining the ticket here?
fastWeakTicketValue :: FastWeakTicket a -> a
#ifdef ghcjs_HOST_OS
fastWeakTicketValue t = unVal (unsafeCoerce (js_ticketVal t))

foreign import javascript unsafe "$r = $1.val;" js_ticketVal :: FastWeakTicket a -> JSVal
#else
fastWeakTicketValue = _fastWeakTicket_val
#endif

getFastWeakValue :: FastWeak a -> IO (Maybe a)
#ifdef ghcjs_HOST_OS
getFastWeakValue w = do
  r <- js_weakVal w
  case js_isNull r of
    True -> return Nothing
    False -> return (unsafeCoerce r)

foreign import javascript unsafe "$1 === null" js_isNull :: JSVal -> Bool

foreign import javascript unsafe "$r = ($1.ticket === null) ? null : $1.ticket.val;" js_weakVal :: FastWeak a -> IO JSVal
#else
getFastWeakValue = deRefWeak
#endif

-- I think it's fine if this is lazy - it'll retain the 'a', but so would the output; we just need to make sure it's forced before we start relying on the associated FastWeak to actually be weak
mkFastWeakTicket :: a -> IO (FastWeakTicket a)
#ifdef ghcjs_HOST_OS
mkFastWeakTicket v = js_fastWeakTicket (unsafeCoerce (Val v))

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
