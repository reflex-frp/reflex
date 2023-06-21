{-# LANGUAGE CPP #-}
module Reflex.Spider.Ref
  ( module X
  ) where

import Reflex.Spider.Ref.Types as X

#ifdef DEBUG_TRACE_REFS
import Reflex.Spider.Ref.Debug as X
#else
import Reflex.Spider.Ref.Normal as X
#endif
