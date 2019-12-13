{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Foldable
import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Exception

import System.Timeout

import Data.Maybe (isJust)
import Data.Functor.Misc
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)


import Data.These
import Data.Align

import Reflex
import Reflex.EventWriter.Base
import Test.Run

import Reflex.Spider.Internal (EventLoopException)


import Data.Witherable (Filterable)

#if defined(MIN_VERSION_these_lens) || (MIN_VERSION_these(0,8,0) && !MIN_VERSION_these(0,9,0))
import Data.These.Lens
#endif

type Widget t m = (MonadHold t m, Reflex t, MonadFix m)


connectDyn ::Widget t m => Event t () -> (Dynamic t a, Dynamic t a) -> m (Dynamic t a)
connectDyn e (d, d') = do
  dd <- holdDyn d (d' <$ e)
  return $ join dd


dynLoop :: Widget t m => (Event t Int, Event t ()) -> m (Event t Int)
dynLoop (e1, e2) = do
  -- "heightBagRemove: Height 2 not present in bag HeightBag {_heightBag_size = 2, _heightBag_contents = fromList [(0,1)]}"
  rec
    d <- count e1
    d' <- connectDyn e2 (d, liftA2 (+) d d')
  return $ updated d'

connectOnCoincidence :: Widget t m => Event t () -> Event t a -> m (Event t a)
connectOnCoincidence click e = do
  d <- holdDyn never (e <$ click)
  return $ coincidence (updated d)

coincidenceLoop :: Widget t m => (Event t Int, Event t ()) -> m (Event t Int)
coincidenceLoop (e1, e2) = do
-- "heightBagRemove: Height 1 not present in bag HeightBag {_heightBag_size = 1, _heightBag_contents = fromList [(0,0)]}"
-- (simpler version of dynLoop)
  rec
    e' <- connectOnCoincidence e2 (updated d)
    d <- count (align e' e1)
  return $ updated d


addHeight :: Reflex t =>  Event t a -> Event t a
addHeight e = leftmost [e4, e4] where
  e1 = leftmost [e, e]
  e2 = leftmost [e1, e1]
  e3 = leftmost [e2, e2]
  e4 = leftmost [e3, e3]


-- Take an existing test and build it inside a
buildLoop :: Widget t m => (forall t m. Widget t m => (Event t Int, Event t ()) -> m (Event t Int)) -> (Event t Int, Event t ()) -> m (Event t Int)
buildLoop test (e1, e2) = switchHold never buildLoop
  where buildLoop = pushAlways (const $ test (e1, e2)) e2


connectButtonPromptly :: Widget t m => Event t () -> Event t a -> m (Event t a)
connectButtonPromptly click e = do
  d <- holdDyn never (e <$ click)
  return (switchDyn d)

connectButton :: Widget t m => Event t () -> Event t a -> m (Event t a)
connectButton click e = do
  d <- hold never (e <$ click)
  return (switch d)


switchLoop :: Widget t m => (Event t Int, Event t ()) -> m (Event t Int)
switchLoop (e1, e2) = do
  rec
    e' <- connectButton e2 (updated d)
    d <- count (align e' e1)
  return $ updated d

mergeLoop :: forall t m. (Adjustable t m, Widget t m) => (Event t Int, Event t ()) -> m (Event t Int)
mergeLoop (e1, e2) = do
  rec
    (_, e) <- runEventWriterT $
      runWithReplace w (leftmost [w <$ e1])

  return (sum <$> e)

  where
    w = do
      c <- count e1
      tellEvent (updated ((pure <$> c) :: Dynamic t [Int]))




switchLoop' :: Widget t m => (Event t Int, Event t ()) -> m (Event t Int)
switchLoop' (e1, e2) = do
  rec
    e' <- connectButton e2 (updated d)
    d <- count (leftmost [e', e1])
  return $ updated d


switchLoop2 :: Widget t m => (Event t Int, Event t ()) -> m (Event t Int)
switchLoop2 (e1, e2) = do
  rec
    e' <- connectButton e2 (addHeight $ updated d)
    d <- count (align e' e1)
  return $ updated d


staticLoop :: Widget t m => (Event t Int, Event t ()) -> m (Event t Int)
staticLoop (e1, e2) = do
  rec
    d <- foldDyn (+) (0 :: Int) (1 <$ align e1 (updated d))
  return $ updated d

staticLoop' :: Widget t m => (Event t Int, Event t ()) -> m (Event t Int)
staticLoop' (e1, e2) = do
  rec
    d <- foldDyn (+) (0 :: Int) (leftmost [e1, updated d])
  return $ updated d


buildStaticLoop :: Widget t m => (Event t Int, Event t ()) -> m (Event t Int)
buildStaticLoop (e1, e2) = switchHold never buildLoop
  where buildLoop = pushAlways (const $ staticLoop (e1, e2)) e2

splitThese :: Filterable f => f (These a b) -> (f a, f b)
splitThese f = (mapMaybe (preview here) f,  mapMaybe (preview there) f)

pattern RunTestCaseFlag = "--run-test"

runTest :: (String, TestCase) -> IO ()
runTest (name, TestCase test) = do
  putStrLn ("Test: " <> name)
  mError <- timeout (milliseconds 5) $
    run `catch` \(e :: EventLoopException) -> pure (show e)

  case mError of
    Just err -> putStrLn err
    Nothing  -> error "timed out (loop not detected)"

  where

      run = do
        r <- runApp' (test . splitThese) (Just <$> occs)
        error (name <> ": unexpected success " <> show r)

      occs = [  This 1, This 2, That (), This 3, That (), This 1 ]

      milliseconds = (*1000)


newtype TestCase = TestCase { unTest :: forall t m. (Widget t m, Adjustable t m) => (Event t Int, Event t ()) -> m (Event t Int)  }

tests :: [(String, TestCase)]
tests =
  [ ("switchLoop'", TestCase switchLoop')
  , ("switchLoop",  TestCase switchLoop)

  , ("mergeLoop",  TestCase mergeLoop)


  -- , ("switchLoop2",  TestCase switchLoop2)

  --  , ("staticLoop'",  TestCase staticLoop')
  --  , ("staticLoop",  TestCase staticLoop)
  -- ("buildStaticLoop", TestCase buildStaticLoop)
  -- , ("buildSwitchLoop", TestCase $ buildLoop switchLoop)

  --  ("coincidenceLoop", TestCase coincidenceLoop)
  -- , ("dynLoop",     TestCase dynLoop)
  -- , ("buildCoincidenceLoop", TestCase $ buildLoop coincidenceLoop)
  ]

main :: IO ()
main = traverse_ runTest tests






