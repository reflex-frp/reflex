{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Fix
import qualified Data.Dependent.Map as DMap
import Data.Functor.Misc
import qualified Data.Map as M
import Data.These

import Reflex
import Reflex.EventWriter.Base
import Test.Run

main :: IO ()
main = do
  os1@[[Just [10,9,8,7,6,5,4,3,2,1]]] <- runApp' (unwrapApp testOrdering) $
    [ Just ()
    ]
  print os1
  os2@[[Just [1,3,5,7,9]],[Nothing,Nothing],[Just [2,4,6,8,10]],[Just [2,4,6,8,10],Nothing]]
    <- runApp' (unwrapApp testSimultaneous) $ map Just $
         [ This ()
         , That ()
         , This ()
         , These () ()
         ]
  print os2
  os3@[[Nothing, Just [2]]] <- runApp' (unwrapApp testMoribundTellEvent) [Just ()]
  print os3
  os4@[[Nothing, Just [2]]] <- runApp' (unwrapApp testMoribundTellEventDMap) [Just ()]
  print os4
  os5@[[Nothing, Just [1, 2]]] <- runApp' (unwrapApp testLiveTellEventDMap) [Just ()]
  print os5
  return ()

unwrapApp :: (Reflex t, Monad m) => (a -> EventWriterT t [Int] m ()) -> a -> m (Event t [Int])
unwrapApp x appIn = do
  ((), e) <- runEventWriterT $ x appIn
  return e

testOrdering :: (Reflex t, Monad m) => Event t () -> EventWriterT t [Int] m ()
testOrdering pulse = do
  forM_ [10,9..1] $ \i -> tellEvent ([i] <$ pulse)
  return ()

testSimultaneous :: (Reflex t, Adjustable t m, MonadHold t m) => Event t (These () ()) -> EventWriterT t [Int] m ()
testSimultaneous pulse = do
  let e0 = fmapMaybe (^? here) pulse
      e1 = fmapMaybe (^? there) pulse
  forM_ [1,3..9] $ \i -> runWithReplace (tellEvent ([i] <$ e0)) $ ffor e1 $ \_ -> tellEvent ([i+1] <$ e0)
  return ()

-- | Test that a widget telling and event which fires at the same time it has been replaced
-- doesn't count along with the new widget.
testMoribundTellEvent
  :: forall t m
  .  ( Reflex t
     , Adjustable t m
     , MonadHold t m
     , MonadFix m
     )
  => Event t ()
  -> EventWriterT t [Int] m ()
testMoribundTellEvent pulse = do
  rec let tellIntOnReplace :: Int -> EventWriterT t [Int] m ()
          tellIntOnReplace x = tellEvent $ [x] <$ rwrFinished
      (_, rwrFinished) <- runWithReplace (tellIntOnReplace 1) $ tellIntOnReplace 2 <$ pulse
  return ()

-- | The equivalent of 'testMoribundTellEvent' for 'traverseDMapWithKeyWithAdjust'.
testMoribundTellEventDMap
  :: forall t m
  .  ( Reflex t
     , Adjustable t m
     , MonadHold t m
     , MonadFix m
     )
  => Event t ()
  -> EventWriterT t [Int] m ()
testMoribundTellEventDMap pulse = do
  rec let tellIntOnReplace :: Int -> EventWriterT t [Int] m ()
          tellIntOnReplace x = tellEvent $ [x] <$ rwrFinished
      (_, rwrFinished :: Event t (PatchDMap (Const2 () Int) Identity)) <-
        traverseDMapWithKeyWithAdjust
          (\(Const2 ()) (Identity v) -> Identity . const v <$> tellIntOnReplace v)
          (mapToDMap $ M.singleton () 1)
          ((PatchDMap $ DMap.map (ComposeMaybe . Just) $ mapToDMap $ M.singleton () 2) <$ pulse)
  return ()

-- | Ensures that elements which are _not_ removed can still fire 'tellEvent's
-- during the same frame as other elements are updated.
testLiveTellEventDMap
  :: forall t m
  .  ( Reflex t
     , Adjustable t m
     , MonadHold t m
     , MonadFix m
     )
  => Event t ()
  -> EventWriterT t [Int] m ()
testLiveTellEventDMap pulse = do
  rec let tellIntOnReplace :: Int -> EventWriterT t [Int] m ()
          tellIntOnReplace x = tellEvent $ [x] <$ rwrFinished
      (_, rwrFinished :: Event t (PatchDMap (Const2 Int ()) Identity)) <-
        traverseDMapWithKeyWithAdjust
          (\(Const2 k) (Identity ()) -> Identity <$> tellIntOnReplace k)
          (mapToDMap $ M.singleton 1 ())
          ((PatchDMap $ DMap.map (ComposeMaybe . Just) $ mapToDMap $ M.singleton 2 ()) <$ pulse)
  return ()
