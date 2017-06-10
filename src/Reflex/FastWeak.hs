{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE MagicHash #-}
module Reflex.FastWeak
  ( FastWeakTicket
  , FastWeak
  , fastWeakTicket
  , fastWeakTicketValue
  , getFastWeakTicketWeak
  , getFastWeakValue
  ) where

#ifdef ghcjs_HOST_OS
import GHCJS.Types
import Unsafe.Coerce
#else
#endif


#ifdef ghcjs_HOST_OS
type Ref = JSVal
#else
type Ref = ()
#endif

newtype FastWeakTicket a = FastWeakTicket Ref

newtype FastWeak a = FastWeak Ref

-- Just designed to mirror JSVal, so that we can unsafeCoerce between the two
data Val a = Val { unVal :: a }

-- Do we have a concern about accidentally retaining the ticket here?
fastWeakTicketValue :: FastWeakTicket a -> a
#ifdef ghcjs_HOST_OS
fastWeakTicketValue t = unVal (unsafeCoerce (js_ticketVal t))

foreign import javascript unsafe "$r = $1.val;" js_ticketVal :: FastWeakTicket a -> JSVal
#else
fastWeakTicketValue = error "fastWeakTicketValue not implemented"
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
getFastWeakValue = error "getFastWeakValue not implemented"
#endif

-- I think it's fine if this is lazy - it'll retain the 'a', but so would the output; we just need to make sure it's forced before we start relying on the associated FastWeak to actually be weak
fastWeakTicket :: a -> FastWeakTicket a
#ifdef ghcjs_HOST_OS
fastWeakTicket v = js_fastWeakTicket (unsafeCoerce (Val v))

foreign import javascript unsafe "$r = new h$FastWeakTicket($1);" js_fastWeakTicket :: JSVal -> FastWeakTicket a
#else
fastWeakTicket = error "fastWeakTicket not implemented"
#endif

-- Needs IO so that it can force the value - otherwise, could end up with a reference to the Ticket, which would retain the value
#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "$r = $1.weak;" getFastWeakTicketWeak :: FastWeakTicket a -> IO (FastWeak a)
#else
getFastWeakTicketWeak :: FastWeakTicket a -> IO (FastWeak a)
getFastWeakTicketWeak = error "fastWeakTicketWeak not implemented"
#endif
