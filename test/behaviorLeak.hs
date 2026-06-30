{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Test whether Applicative combination of Behaviors leaks memory when one of
-- the Behaviors' source events never occurs.
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

warmupTicks :: Int
warmupTicks = 200000
windowTicks :: Int
windowTicks = 200000
samplesPerWindow :: Int
samplesPerWindow = 20
ticksBetweenWindows :: Int
ticksBetweenWindows = 9 * windowTicks
allowedGrowthBytes :: Integer
allowedGrowthBytes = 512 * 1024

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
        liveBytes = liftIO $ do
          performMajorGC
          fromIntegral . gcdetails_live_bytes . gc <$> getRTSStats :: IO Integer
        measureWindow = do
          let chunk = windowTicks `div` samplesPerWindow
          samples <- replicateM samplesPerWindow $ do
            replicateM_ chunk fireOnce
            liveBytes
          pure $ sum samples `div` fromIntegral samplesPerWindow

    replicateM_ warmupTicks fireOnce
    firstWindowAvg <- measureWindow
    replicateM_ ticksBetweenWindows fireOnce
    secondWindowAvg <- measureWindow
    liftIO $ touch b
    liftIO $ do
      let growth = secondWindowAvg - firstWindowAvg
      if growth <= allowedGrowthBytes
        then putStrLn "Succeeded"
        else do
          printf "Failed: Behavior Applicative space leak\n"
          printf "    first-window avg live bytes:  %d\n" firstWindowAvg
          printf "    second-window avg live bytes: %d\n" secondWindowAvg
          printf "    growth over %d intervening ticks: %d bytes (allowed %d)\n"
            ticksBetweenWindows growth allowedGrowthBytes
          exitFailure
