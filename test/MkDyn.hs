{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.IntMap
import Reflex
import Reflex.Dynamic.TH
import Reflex.Plan.Pure
import Reflex.TestPlan
import System.Exit

main::IO ()
main = do
  let setup = do
        pEv1 <- planList [1.0,2.0,3.0]
        pEv2 <- planList [-1.0,-2.0,-3.0]
        dyn1 <- holdDyn 0 pEv1
        dyn2 <- holdDyn 0 pEv2
        return (dyn1, dyn2)
      resMkDyn :: IntMap (Maybe Double)
      resMkDyn  = testEvent $ runPure $ do
        (dyn1, dyn2) <- setup
        let dynFromMkDyn = [mkDynPure|$dyn1 + $dyn2|]
        return $ updated dynFromMkDyn
      resZipDyn :: IntMap (Maybe Double)
      resZipDyn  = testEvent $ runPure $ do
        (dyn1,dyn2) <- setup
        let dynFromZipDyn = zipDynWith (+) dyn1 dyn2
        return $ updated dynFromZipDyn
      testPassed = resMkDyn == resZipDyn
  putStrLn $ "MkDynPure: " ++ show resMkDyn
  putStrLn $ "zipDynWith: " ++ show resZipDyn
  putStr "MkDyn: "
  if testPassed then putStrLn "Succeeded" else putStrLn "Failed"
  exitWith $ if testPassed then ExitSuccess else ExitFailure 1


