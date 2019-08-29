{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Functor.Misc
import qualified Data.Map as M
import Data.These


import Reflex
import Reflex.EventWriter.Base
import Test.Run

import Data.Witherable (Filterable)

#if defined(MIN_VERSION_these_lens) || (MIN_VERSION_these(0,8,0) && !MIN_VERSION_these(0,9,0))
import Data.These.Lens
#endif


connectDyn :: (MonadHold t m, Reflex t, MonadFix m) => Event t () -> (Dynamic t a, Dynamic t a) -> m (Dynamic t a)
connectDyn e (d, d') = do 
  dd <- holdDyn d (d' <$ e)
  return $ join dd


dynLoop :: (MonadHold t m, Reflex t, MonadFix m) => (Event t Int, Event t ()) -> m (Event t Int)
dynLoop (e1, e2) = do
  -- "heightBagRemove: Height 2 not present in bag HeightBag {_heightBag_size = 2, _heightBag_contents = fromList [(0,1)]}"
  rec
    d <- count e1
    d' <- connectDyn e2 (d, (liftA2 (+) d d'))
  return $ updated d'


splitThese :: Filterable f => f (These a b) -> (f a, f b)
splitThese f = (mapMaybe (preview here) f,  mapMaybe (preview there) f)

main :: IO ()
main = void $ do
  runApp' (dynLoop . splitThese) (Just <$> occs)
    
  where 
    occs =  [ This 1, This 2, That (), This 3 ]


