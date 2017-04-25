{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs       #-}
module Reflex.Test.TH (testCases) where

import Reflex
import Reflex.Dynamic.TH

import Reflex.TestPlan

testCases :: [(String, TestCase)]
testCases =
  [
    testE "mkDynPure" $ do
      dyn1 <- holdDyn 0 =<< events1
      dyn2 <- holdDyn 0 =<< events2
      let dynResult = [mkDynPure|($dyn1, $dyn2)|]
      return $ updated dynResult
  ] where

  events1, events2 ::  TestPlan t m => m (Event t Int)
  events1 = plan [(1, 1), (11, 2), (21, 3), (31, 4), (41, 5)]
  events2 = plan [(5, 10), (15, 20), (25, 30), (35, 40), (45, 50)]




