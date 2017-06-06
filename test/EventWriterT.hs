{-# LANGUAGE FlexibleContexts, GADTs #-}
module Main where

import Control.Lens
import Control.Monad
import Data.These

import Reflex
import Reflex.EventWriter
import Test.Run

import System.Exit (exitFailure)

import Data.IORef
import Control.Monad.Ref
import Reflex.Host.Class
import Data.List

main :: IO ()
main = do
  os1@[[Just [10,9,8,7,6,5,4,3,2,1]]] <- runApp' (unwrapApp app1) $
    [ Just ()
    ]
  print os1
  os2@[[Just [1,3,5,7,9]],[Nothing,Nothing],[Just [2,4,6,8,10]],[Just [2,4,6,8,10],Nothing]]
    <- runApp' (unwrapApp app2) $ map Just $
         [ This ()
         , That ()
         , This ()
         , These () ()
         ]
  print os2
  return ()

unwrapApp :: (Reflex t, Monad m) => (a -> EventWriterT t [Int] m ()) -> a -> m (Event t [Int])
unwrapApp x appIn = do
  ((), e) <- runEventWriterT $ x appIn
  return e

app1 :: (Reflex t, Monad m) => Event t () -> EventWriterT t [Int] m ()
app1 pulse = do
  forM_ [10,9..1] $ \i -> tellEvent ([i] <$ pulse)
  return ()

app2 :: (Reflex t, MonadAdjust t m, MonadHold t m) => Event t (These () ()) -> EventWriterT t [Int] m ()
app2 pulse = do
  let e0 = fmapMaybe (^? here) pulse
      e1 = fmapMaybe (^? there) pulse
  forM_ [1,3..9] $ \i -> runWithReplace (tellEvent ([i] <$ e0)) $ ffor e1 $ \_ -> tellEvent ([i+1] <$ e0)
  return ()

