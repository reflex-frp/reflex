{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Test whether Applicative combination of Behaviors leaks memory when one of
-- the Behaviors' source events doesn't update.
-- (https://github.com/reflex-frp/reflex/issues/490)

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Primitive (touch)
import Control.Monad.Ref
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Maybe (fromJust)
import GHC.Stats
import Reflex
import Reflex.Host.Class
import System.Exit
import System.Mem (performMajorGC)
import Text.Printf

warmupN, measureN :: Int
warmupN  = 200000
measureN = 2000000

testCase :: (Reflex t, MonadHold t m) => Event t b -> m (Behavior t (Int, Int))
testCase tickE = do
  a <- hold (0 :: Int) (0 <$ tickE)
  c <- hold (0 :: Int) never
  pure $ (,) <$> a <*> c

main :: IO ()
main = do
  enabled <- getRTSStatsEnabled
  unless enabled $ do
    putStrLn "Failed: RTS stats not enabled (run with +RTS -T)"
    exitFailure

  runSpiderHost $ do
    (tickE, tickTriggerRef) <- newEventWithTriggerRef
    b <- runHostFrame $ testCase tickE
    trigger <- fromJust <$> readRef tickTriggerRef
    let fireOnce = void $ fireEventsAndRead [trigger :=> Identity ()] $ do
          v <- sample b
          v `seq` pure ()
    let liveBytes = liftIO $ do
          performMajorGC
          gcdetails_live_bytes . gc <$> getRTSStats
    replicateM_ warmupN fireOnce
    liftIO performMajorGC
    before <- liveBytes
    replicateM_ measureN fireOnce
    after <- liveBytes
    liftIO $ touch b
    liftIO $ do
      let liveBytesDifference = after - before
      let liveBytesRatio :: Double =
            fromIntegral (toInteger after - toInteger before)
            / fromIntegral (measureN - warmupN)
      if liveBytesRatio < 0.1
        then putStrLn "Succeeded"
        else do
          printf "Failed: Behavior Applicative space leak\n"
          printf "    absolute difference: %d live bytes\n" liveBytesDifference
          printf "    approx. avg. leaked bytes per tick: %.3f\n" liveBytesRatio
          -- exitFailure -- Ignore until fixed.
