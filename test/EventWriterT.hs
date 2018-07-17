{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Main where

import Control.Lens
import Control.Monad
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
  x <- runApp' (unwrapApp testRunWithReplace) $ map Just $ concat $ replicate 3
    [ This (This ()) -- outer
    , This (That ()) -- inner
    , That ()        -- output
    ]
  print x
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

testRunWithReplace
  -- :: forall t m
  ::  ( Reflex t
     --, MonadFix m
     , MonadHold t m
     , Adjustable t m
     , EventWriter t [Int] m
     )
  => Event t (These (These () ()) ())
  -> m ()
testRunWithReplace pulse = do
  let outer = fmapMaybe (^? here . here) pulse
  let inner = fmapMaybe (^? here . there) pulse
  let output = fmapMaybe (^? there) pulse
  void $ runWithReplace (return ()) $ ffor outer $ const $ do
    void $ runWithReplace (return ()) $ ffor inner $ const $ do
      tellEvent $ [1] <$ output
