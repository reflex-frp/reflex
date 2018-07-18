{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Fix
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
  rec let tellIntOnReplce :: Int -> EventWriterT t [Int] m ()
          tellIntOnReplce x = tellEvent $ [x] <$ rwrFinished
      (_, rwrFinished) <- runWithReplace (tellIntOnReplce 1) $ tellIntOnReplce 2 <$ pulse
  return ()
