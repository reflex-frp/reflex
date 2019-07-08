{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Fix
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.Foldable
import Data.These

import Reflex
import Reflex.Requester.Base
import Reflex.Requester.Class
import Test.Run

import Debug.Trace

data RequestInt a where
  RequestInt :: Int -> RequestInt Int

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
  return ()

unwrapRequest :: RequestItem t x RequestInt Identity -> Int
unwrapRequest (RequestItem { _requestItem_value = RequestInt i }) = i

unwrapApp
  :: ( Reflex t
     , MonadFix m
     , MonadHold t m
     )
  => (a -> RequesterT t RequestInt Identity m ())
  -> a
  -> m (Event t [Int])
unwrapApp x appIn = do
  runRequesterT' $ \reqs -> do
    () <- x appIn
    return (never, fmap (map unwrapRequest . toList) reqs)

testOrdering :: ( Response m ~ Identity
                , Request m ~ RequestInt
                , Requester t m
                , Adjustable t m)
             => Event t ()
             -> m ()
testOrdering pulse = forM_ [10,9..1] $ \i -> do
  traceM $ "testOrdering." <> show i
  requestingIdentity (RequestInt i <$ pulse)

testSimultaneous :: ( Response m ~ Identity
                    , Request m ~ RequestInt
                    , Requester t m
                    , Adjustable t m)
                 => Event t (These () ())
                 -> m ()
testSimultaneous pulse = do
  traceM $ "testSimultaneous.a"
  let tellE = fmapMaybe (^? here) pulse
      switchE = fmapMaybe (^? there) pulse
  forM_ [1,3..9] $ \i -> do
    traceM $ "testSimultaneous.b" <> show i
    runWithReplace (requestingIdentity (RequestInt i <$ tellE)) $ ffor switchE $ \_ -> do
      traceM $ "testSimultaneous.c"
      result <- requestingIdentity (RequestInt (i+1) <$ tellE)
      traceM $ "testSimultaneous.d"
      pure result
