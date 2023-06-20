{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Reflex.Spider.NodeInfo.Debug where

import Control.Monad.IO.Class
import Data.IORef
import GHC.Stack
import GHC.Exts (Ptr)
import System.IO.Unsafe

newtype NodeId x = NodeId { unNodeId :: Int }
  deriving (Show, Eq, Ord, Enum)

newtype NodeIdAllocator x = NodeIdAllocator { unNodeIdAllocator :: IORef (NodeId x) }

class HasNodeIds x where
  getNodeIdAllocator :: NodeIdAllocator x

{-# INLINE newNodeIdAllocator #-}
newNodeIdAllocator :: IO (NodeIdAllocator x)
newNodeIdAllocator = NodeIdAllocator <$> newIORef (NodeId 2)

{-# INLINE neverNodeId #-}
neverNodeId :: NodeId x
neverNodeId = NodeId 0

{-# INLINE nowNodeId #-}
nowNodeId :: NodeId x
nowNodeId = NodeId 1

{-# INLINE newNodeId #-}
newNodeId :: forall x m. (HasNodeIds x, MonadIO m) => m (NodeId x)
newNodeId = liftIO $ atomicModifyIORef' (unNodeIdAllocator (getNodeIdAllocator @x)) $ \n -> (succ n, n)

showNodeId' :: NodeId x -> String
showNodeId' = ("#"<>) . show . unNodeId

newtype StackInfo = StackInfo { unStackInfo :: Ptr CostCentreStack }

{-# INLINE withStackInfo #-}
withStackInfo :: dummy -> (StackInfo -> a) -> a
withStackInfo dummy k = unsafePerformIO $ do
  ccs <- getCurrentCCS dummy
  pure $ k $ StackInfo ccs

{-# INLINE stackInfoToStrings #-}
stackInfoToStrings :: StackInfo -> IO [String]
stackInfoToStrings = ccsToStrings . unStackInfo
