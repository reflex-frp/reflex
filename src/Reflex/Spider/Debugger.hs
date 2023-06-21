{-# LANGUAGE FlexibleContexts #-}
module Reflex.Spider.Debugger where

import Prelude hiding (filter)

import Control.Monad.RWS
import Data.Foldable
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Trie (Trie (..))
import qualified Data.Trie as Trie
import Data.Sequence ((<|))
import qualified Data.Sequence as Seq
import Data.Witherable


indent :: MonadWriter [String] m => m a -> m a
indent = censor (fmap ("  " <>))

trieToDot :: Trie String (Map Int (Set Int)) -> RWS () [String] Int ()
trieToDot (Trie prefix leaves children) = do
  myId <- get
  put $! succ myId
  tell ["subgraph cluster_" <> show myId <> " {"]
  indent $ do
    tell ["label = " <> show (intercalate "\n" $ reverse $ toList prefix) <> ";"]
    forM_ (maybe [] Map.toList leaves) $ \(nodeId, _) -> do
      tell ["n" <> show nodeId <> " [label=" <> show ("#" <> show nodeId) <> "];"]
    forM_ (Map.toList children) $ \(discriminatorStackFrame, Trie childStackFrames childLeaves childChildren) -> do
      trieToDot $ Trie (discriminatorStackFrame <| childStackFrames) childLeaves childChildren
  tell ["}"]

showDot :: [([String], (Int, Set Int))] -> String
showDot nodes = unlines $ snd $ execRWS graph () 1
  where
    includedNodes = Set.fromList $ fmap (\(_, (nodeId, _)) -> nodeId) nodes
    t = Trie.fromList $ (\(stack, (nodeId, parents)) -> (Seq.fromList stack, Map.singleton nodeId $ Set.intersection includedNodes parents)) <$> filter (\(_, (nodeId, _)) -> nodeId `Set.member` includedNodes) nodes
    edges = fmap (Set.intersection includedNodes) $ Map.fromList $ fmap snd nodes
    graph = do
      tell ["digraph {"]
      indent $ do
        tell ["labelloc=b;"]
        trieToDot t
        forM_ (Map.toList edges) $ \(nodeId, parents) -> do
          when (nodeId `Set.member` includedNodes) $ do
            tell ["{" <> intercalate ";" ((\parentId -> "n" <> show parentId) <$> Set.toList (Set.intersection includedNodes parents)) <> "} -> n" <> show nodeId <> ";"]
      tell ["}"]
