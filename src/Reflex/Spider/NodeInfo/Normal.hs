{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- We want to keep constraints here even if they are redundant, to match the Debug version of this module
module Reflex.Spider.NodeInfo.Normal where

import Control.Monad.IO.Class

newtype NodeId x = NodeId ()
  deriving (Show, Eq, Ord, Enum)

newtype NodeIdAllocator x = NodeIdAllocator ()

class HasNodeIds x where
  getNodeIdAllocator :: NodeIdAllocator x

newtype NodeInfo x = NodeInfo ()

{-# INLINE newNodeIdAllocator #-}
newNodeIdAllocator :: IO (NodeIdAllocator x)
newNodeIdAllocator = pure $ NodeIdAllocator ()

{-# INLINE neverNodeId #-}
neverNodeId :: NodeId x
neverNodeId = NodeId ()

{-# INLINE nowNodeId #-}
nowNodeId :: NodeId x
nowNodeId = NodeId ()

{-# INLINE newNodeId #-}
newNodeId :: forall x m. (HasNodeIds x, MonadIO m) => m (NodeId x)
newNodeId = pure $ NodeId ()

{-# INLINE showNodeId' #-}
showNodeId' :: NodeId x -> String
showNodeId' _ = ""

newtype StackInfo = StackInfo ()

{-# INLINE withStackInfo #-}
withStackInfo :: dummy -> (StackInfo -> a) -> a
withStackInfo _ k = k $ StackInfo ()

{-# INLINE stackInfoToStrings #-}
stackInfoToStrings :: StackInfo -> IO [String]
stackInfoToStrings _ = pure []
