{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Fix
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.Functor.Misc
import qualified Data.Map as M
import Data.These

import Reflex
import Reflex.Requester.Base
import Reflex.Requester.Class
import Test.Run

data RequestInt a where
  RequestInt :: Int -> RequestInt Int

main :: IO ()
main = do
  os1 <- runApp' (unwrapApp testOrdering) $
    [ Just ()
    ]
  print os1
  os2 <- runApp' (unwrapApp testSimultaneous) $ map Just $
    [ This ()
    , That ()
    , This ()
    , These () ()
    ]
  print os2
  os3 <- runApp' (unwrapApp testMoribundRequest) [Just ()]
  print os3
  os4 <- runApp' (unwrapApp testMoribundRequestDMap) [Just ()]
  print os4
  os5 <- runApp' (unwrapApp testLiveRequestDMap) [Just ()]
  print os5
  let ![[Just [10,9,8,7,6,5,4,3,2,1]]] = os1
  let ![[Just [1,3,5,7,9]],[Nothing,Nothing],[Just [2,4,6,8,10]],[Just [2,4,6,8,10],Nothing]] = os2
  let ![[Nothing, Just [2]]] = os3
  let ![[Nothing, Just [2]]] = os4
  let ![[Nothing, Just [1, 2]]] = os5
  return ()

unwrapRequest :: DSum tag RequestInt -> Int
unwrapRequest (_ :=> RequestInt i) = i

unwrapApp :: ( Reflex t, Monad m )
          => (a -> RequesterT t RequestInt Identity m ())
          -> a
          -> m (Event t [Int])
unwrapApp x appIn = do
  ((), e) <- runRequesterT (x appIn) never
  return $ fmap (map unwrapRequest . requesterDataToList) e

testOrdering :: ( Response m ~ Identity
                , Request m ~ RequestInt
                , Requester t m
                , Adjustable t m)
             => Event t ()
             -> m ()
testOrdering pulse = forM_ [10,9..1] $ \i ->
  requestingIdentity (RequestInt i <$ pulse)

testSimultaneous :: ( Response m ~ Identity
                    , Request m ~ RequestInt
                    , Requester t m
                    , Adjustable t m)
                 => Event t (These () ())
                 -> m ()
testSimultaneous pulse = do
  let tellE = fmapMaybe (^? here) pulse
      switchE = fmapMaybe (^? there) pulse
  forM_ [1,3..9] $ \i -> runWithReplace (requestingIdentity (RequestInt i <$ tellE)) $ ffor switchE $ \_ ->
    requestingIdentity (RequestInt (i+1) <$ tellE)

-- | Test that a widget requesting and event which fires at the same time it has been replaced
-- doesn't count along with the new widget.
testMoribundRequest
  :: forall t m
  .  ( Reflex t
     , Adjustable t m
     , MonadHold t m
     , MonadFix m
     , Response m ~ Identity
     , Request m ~ RequestInt
     , Requester t m
     )
  => Event t ()
  -> m ()
testMoribundRequest pulse = do
  rec let requestIntOnReplace x = requestingIdentity $ RequestInt x <$ rwrFinished
      (_, rwrFinished) <- runWithReplace (requestIntOnReplace 1) $ requestIntOnReplace 2 <$ pulse
  return ()

-- | The equivalent of 'testMoribundRequest' for 'traverseDMapWithKeyWithAdjust'.
testMoribundRequestDMap
  :: forall t m
  .  ( Reflex t
     , Adjustable t m
     , MonadHold t m
     , MonadFix m
     , Response m ~ Identity
     , Request m ~ RequestInt
     , Requester t m
     )
  => Event t ()
  -> m ()
testMoribundRequestDMap pulse = do
  rec let requestIntOnReplace :: Int -> m ()
          requestIntOnReplace x = void $ requestingIdentity $ RequestInt x <$ rwrFinished
      (_, rwrFinished :: Event t (PatchDMap (Const2 () Int) Identity)) <-
        traverseDMapWithKeyWithAdjust
          (\(Const2 ()) (Identity v) -> Identity . const v <$> requestIntOnReplace v)
          (mapToDMap $ M.singleton () 1)
          ((PatchDMap $ DMap.map (ComposeMaybe . Just) $ mapToDMap $ M.singleton () 2) <$ pulse)
  return ()

-- | Ensures that elements which are _not_ removed can still fire requests
-- during the same frame as other elements are updated.
testLiveRequestDMap
  :: forall t m
  .  ( Reflex t
     , Adjustable t m
     , MonadHold t m
     , MonadFix m
     , Response m ~ Identity
     , Request m ~ RequestInt
     , Requester t m
     )
  => Event t ()
  -> m ()
testLiveRequestDMap pulse = do
  rec let requestIntOnReplace :: Int -> m ()
          requestIntOnReplace x = void $ requestingIdentity $ RequestInt x <$ rwrFinished
      (_, rwrFinished :: Event t (PatchDMap (Const2 Int ()) Identity)) <-
        traverseDMapWithKeyWithAdjust
          (\(Const2 k) (Identity ()) -> Identity <$> requestIntOnReplace k)
          (mapToDMap $ M.singleton 1 ())
          ((PatchDMap $ DMap.map (ComposeMaybe . Just) $ mapToDMap $ M.singleton 2 ()) <$ pulse)
  return ()
