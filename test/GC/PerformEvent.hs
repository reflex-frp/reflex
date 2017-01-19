{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, TypeFamilies, RecursiveDo, RankNTypes, ScopedTypeVariables, LambdaCase #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Ref
import Control.Monad.Fix
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Functor.Misc
import Reflex
import Reflex.Host.Class

import System.Mem
import System.Exit

main :: IO ()
main = do
  r <- newIORef Nothing
  runApp (app r)
  readIORef r >>= \case
    Nothing -> exitFailure
    Just x -> do
      putStrLn $ mconcat ["Result: ", show x]
      return ()
  return ()

runApp :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
       => (Event t () -> Event t () -> PerformEventT t m ())
       -> IO ()
runApp x = runSpiderHost $ do
  (egc, gcTriggerRef) <- newEventWithTriggerRef
  (eadd, addTriggerRef) <- newEventWithTriggerRef
  (_, FireCommand fire) <- hostPerformEventT $ x egc eadd
  mGcTrigger <- readRef gcTriggerRef
  mAddTrigger <- readRef addTriggerRef
  forM_ ((,) <$> mGcTrigger <*> mAddTrigger) $ \(gcTrigger, addTrigger) -> do
    _ <- fire [ gcTrigger :=> Identity () ] $ return ()
    _ <- fire [ addTrigger :=> Identity () ] $ return ()
    _ <- fire [ addTrigger :=> Identity () ] $ return ()
    return ()

app :: (MonadIO m, MonadAdjust t m, MonadHold t m, PerformEvent t m, MonadIO (Performable m), MonadFix m) => IORef (Maybe Int) -> Event t () -> Event t () -> m ()
app r egc eadd = do
  performEvent_ $ liftIO performMajorGC <$ egc
  rec let modMap = ffor (tag (current list) eadd) $ \m -> case Map.maxViewWithKey m of
            Nothing -> Map.singleton (1 :: Int) (Just ())
            Just ((k, _), _) -> Map.singleton (k+1) (Just ())
          listEl _ _ = performEvent_ $ liftIO (writeIORef r (Just 1)) <$ eadd
      list <- do
        let dm0 = DMap.empty
            dm' = fmap (PatchDMap . mapWithFunctorToDMap . Map.mapWithKey (\k v -> ComposeMaybe $ fmap (listEl k) v)) modMap
        (a0, a') <- sequenceDMapWithAdjust dm0 dm'
        fmap dmapToMap . incrementalToDynamic <$> holdIncremental a0 a'
  return ()
