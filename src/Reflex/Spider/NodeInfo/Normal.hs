{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reflex.Spider.NodeInfo.Normal where

type NodeInfo = ()

newtype NodeId x = NodeId ()
  deriving (Show, Eq, Ord, Enum)
