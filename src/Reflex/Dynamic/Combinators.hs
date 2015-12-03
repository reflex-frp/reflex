{-# LANGUAGE QuasiQuotes, GADTs #-}
module Reflex.Dynamic.Combinators ( combineDyn3
                                  , combineDyn4
                                  ) where

import Reflex.Class
import Reflex.Dynamic
import Reflex.Dynamic.TH

-- | combineDyn for three 'Dynamic's.
combineDyn3 :: (Reflex t, MonadHold t m)
  => (a -> b -> c -> d)
  -> Dynamic t a
  -> Dynamic t b
  -> Dynamic t c
  -> m (Dynamic t d)
combineDyn3 f a b c = [mkDyn|f $a $b $c|]

-- | combineDyn for four 'Dynamic's.
combineDyn4 :: (Reflex t, MonadHold t m)
  => (a -> b -> c -> d -> e)
  -> Dynamic t a
  -> Dynamic t b
  -> Dynamic t c
  -> Dynamic t d
  -> m (Dynamic t e)
combineDyn4 f a b c d = [mkDyn|f $a $b $c $d|]


