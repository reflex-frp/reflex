{-# LANGUAGE CPP #-}
module Reflex.Spider.NodeInfo
  ( module X
  ) where

#ifdef DEBUG_NODEIDS
import Reflex.Spider.NodeInfo.Debug as X
#else
import Reflex.Spider.NodeInfo.Normal as X
#endif
