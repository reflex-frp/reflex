{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reflex.Spider.NodeInfo.Debug where

import Reflex.Spider.NodeInfo

import GHC.Stack
import GHC.Exts (Ptr)

newtype NodeId x = NodeId { unNodeId :: Int }
  deriving (Show, Eq, Ord, Enum)

data NodeInfo x = NodeInfo
  { _nodeInfo_id :: {-# UNPACK #-} !(NodeId x)
  , _nodeInfo_ccs :: {-# UNPACK #-} !(Ptr CostCentreStack)
  }

