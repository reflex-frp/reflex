{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if GHCJS_FAST_WEAK
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif

-- |Contains 'FastWeak', a weak pointer to some value, and 'FastWeakTicket' which ensures the value referred to by a 'FastWeak' stays live while the ticket is
-- held (live).
--
-- On GHC or GHCJS when not built with the @fast-weak@ cabal flag, 'FastWeak' is a wrapper around the simple version of 'System.Mem.Weak.Weak' where the key
-- and value are the same.
--
-- On GHCJS when built with the @fast-weak@ cabal flag, 'FastWeak' is implemented directly in JS using @h$FastWeak@ and @h$FastWeakTicket@ which are a
-- nonstandard part of the GHCJS RTS. __FIXME__ 
module Reflex.FastWeak
  ( FastWeakTicket
  , FastWeak
  , mkFastWeakTicket
  , getFastWeakTicketValue
  , getFastWeakTicketWeak
  , getFastWeakValue
  , getFastWeakTicket
  , emptyFastWeak
#ifdef GHCJS_FAST_WEAK
  --TODO: Move these elsewhere
  , unsafeFromRawJSVal
  , unsafeToRawJSVal
  , js_isNull
#endif
  ) where

import GHC.Exts (Any)
import Unsafe.Coerce

#ifdef GHCJS_FAST_WEAK
import GHCJS.Types
#else
import Control.Exception (evaluate)
import System.IO.Unsafe
import System.Mem.Weak
#endif


#ifdef GHCJS_FAST_WEAK
-- |A 'FastWeak' which has been promoted to a strong reference. 'getFastWeakTicketValue' can be used to get the referred to value without fear of @Nothing,
-- and 'getFastWeakTicketWeak' can be used to get the weak version.
--
-- Implemented by way of special support in the GHCJS RTS, @h$FastWeakTicket@.
newtype FastWeakTicket a = FastWeakTicket JSVal

-- |A reference to some value which can be garbage collected if there are only weak references to the value left.
--
-- 'getFastWeakValue' can be used to try and obtain a strong reference to the value.
--
-- The value in a @FastWeak@ can also be kept alive by obtaining a 'FastWeakTicket' using 'getFastWeakTicket' if the value hasn't been collected yet.
--
-- Implemented by way of special support in the GHCJS RTS, @h$FastWeak@.
newtype FastWeak a = FastWeak JSVal

-- Just designed to mirror JSVal, so that we can unsafeCoerce between the two
data Val a = Val { unVal :: a }

-- | Coerce a JSVal that represents the heap object of a value of type @a@ into a value of type @a@
unsafeFromRawJSVal :: JSVal -> a
unsafeFromRawJSVal v = unVal (unsafeCoerce v)

-- | Coerce a heap object of type @a@ into a 'JSVal' which represents that object.
unsafeToRawJSVal :: a -> JSVal
unsafeToRawJSVal v = unsafeCoerce (Val v)
#else
-- |A 'FastWeak' which has been promoted to a strong reference. 'getFastWeakTicketValue' can be used to get the referred to value without fear of @Nothing,
-- and 'getFastWeakTicketWeak' can be used to get the weak version.
data FastWeakTicket a = FastWeakTicket
  { _fastWeakTicket_val :: !a
  , _fastWeakTicket_weak :: {-# UNPACK #-} !(Weak a)
  }

-- |A reference to some value which can be garbage collected if there are only weak references to the value left.
--
-- 'getFastWeakValue' can be used to try and obtain a strong reference to the value.
--
-- The value in a @FastWeak@ can also be kept alive by obtaining a 'FastWeakTicket' using 'getFastWeakTicket' if the value hasn't been collected yet.
--
-- Synonymous with 'Weak'.
type FastWeak a = Weak a
#endif

-- |Return the @a@ kept alive by the given 'FastWeakTicket'.
--
-- This needs to be in IO so we know that we've relinquished the ticket.
getFastWeakTicketValue :: FastWeakTicket a -> IO a
#ifdef GHCJS_FAST_WEAK
getFastWeakTicketValue t = do
  v <- js_ticketVal t
  return $ unsafeFromRawJSVal v

foreign import javascript unsafe "$r = $1.val;" js_ticketVal :: FastWeakTicket a -> IO JSVal
#else
getFastWeakTicketValue = return . _fastWeakTicket_val
#endif

-- |Get the value referred to by a 'FastWeak' if it hasn't yet been collected, or @Nothing@ if it has been collected.
getFastWeakValue :: FastWeak a -> IO (Maybe a)
#ifdef GHCJS_FAST_WEAK
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

-- |Try to create a 'FastWeakTicket' for the given 'FastWeak' which will ensure the referred to value remains alive. Returns @Just@ if the value hasn't been
-- collected and a ticket can therefore be obtained, @Nothing@ if it's been collected.
getFastWeakTicket :: forall a. FastWeak a -> IO (Maybe (FastWeakTicket a))
#ifdef GHCJS_FAST_WEAK
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

-- |Create a 'FastWeakTicket' directly from a value, creating a 'FastWeak' in the process which can be obtained with 'getFastWeakTicketValue'.
mkFastWeakTicket :: a -> IO (FastWeakTicket a)
-- I think it's fine if this is lazy - it'll retain the 'a', but so would the output; we just need to make sure it's forced before we start relying on the
-- associated FastWeak to actually be weak
#ifdef GHCJS_FAST_WEAK
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

-- |Demote a 'FastWeakTicket' which ensures the value is alive to a 'FastWeak' which doesn't. Note that unless the ticket or another for the same 'FastWeak' is
-- held some other way the value might be collected immediately.
getFastWeakTicketWeak :: FastWeakTicket a -> IO (FastWeak a)
-- Needs IO so that it can force the value - otherwise, could end up with a reference to the Ticket, which would retain the value
#ifdef GHCJS_FAST_WEAK
foreign import javascript unsafe "$r = $1.weak;" getFastWeakTicketWeak' :: FastWeakTicket a -> IO (FastWeak a)
{-# INLINE getFastWeakTicketWeak #-}
getFastWeakTicketWeak = getFastWeakTicketWeak'
#else
getFastWeakTicketWeak = return . _fastWeakTicket_weak
#endif

-- | A weak reference that is always empty
emptyFastWeak :: FastWeak a
emptyFastWeak = unsafeCoerce w
  where w :: FastWeak Any
#ifdef GHCJS_FAST_WEAK
        w = js_emptyWeak
foreign import javascript unsafe "$r = new h$FastWeak(null);" js_emptyWeak :: FastWeak Any
#else
        w = unsafePerformIO $ do
          w' <- mkWeakPtr undefined Nothing
          finalize w'
          return w'
#endif
{-# NOINLINE emptyFastWeak #-}
