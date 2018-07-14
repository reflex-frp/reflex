{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens
import Control.Monad
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.These

import Reflex
import Reflex.Requester.Base
import Reflex.Requester.Class
import Test.Run

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

unwrapRequest :: DSum tag RequestInt -> Int
unwrapRequest (_ :=> RequestInt i) = i

unwrapApp :: ( Reflex t, Monad m )
          => (a -> RequesterT t RequestInt Identity m ())
          -> a
          -> m (Event t [Int])
unwrapApp x appIn = do
  ((), e) <- runRequesterT (x appIn) never
  return $ fmap (map unwrapRequest . DMap.toList) e

testOrdering :: ( Response m ~ Identity
                , Request m ~ RequestInt
                , Requester t m
                , Adjustable t m)
             => Event t ()
             -> m ()
testOrdering pulse = do
  forM_ [10,9..1] $ \i -> requestingIdentity (RequestInt i <$ pulse)
  return ()

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
