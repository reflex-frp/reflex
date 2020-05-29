{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Reflex
import Reflex.Host.Headless
import Reflex.Network
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.These (These (..))
import Data.Either (Either (..), partitionEithers)
import Control.Monad.Fix (MonadFix)
import Control.Monad (void)
import Control.Concurrent
import Control.Monad.IO.Class
import Data.IORef
import System.IO.Unsafe
import Debug.Trace

main :: IO ()
main = runHeadlessApp mainWidget

mainWidget
  :: forall m t
   . ( MonadFix m
     , MonadHold t m
     , PostBuild t m
     , MonadIO m
     , TriggerEvent t m
     , NotReady t m
     , Adjustable t m
     )
  => m (Event t ())
mainWidget = do
  ref                   <- liftIO $ newIORef 0
  (e    , eTrigger    ) <- newTriggerEvent
  (close, closeTrigger) <- newTriggerEvent
  void $ liftIO $ forkIO $ do
    threadDelay 1000000
    eTrigger ()
    threadDelay 1000000
    count <- readIORef ref
    if count == 1
      then do
        putStrLn
          "It appears there is no bug here! Trigger was counted exactly once!"
        closeTrigger ()
      else do
        putStrLn $ "Bug! Triggers counted: " <> show count
        closeTrigger ()
  dynamic <- countTriggers ref <$> holdDyn () e
  void $ simpleList (constDyn [0 .. bugFactor]) $ const
    (void $ networkView $ pure () <$ dynamic)
  pure close

bugFactor :: Int
bugFactor = 1000

{-# NOINLINE incrementRef #-}
incrementRef :: (Show a) => IORef Int -> a -> String
incrementRef ref x = unsafePerformIO
  (atomicModifyIORef
    ref
    (\a -> (a + 1, show x <> " triggered time: " <> show (a + 1)))
  )

countTriggers
  :: (Reflex t, Show a) => IORef Int -> Dynamic t a -> Dynamic t a
countTriggers ref d =
  let e'    = traceEventWith (incrementRef ref) $ updated d
      getV0 = sample $ current d
  in  trace "Registering counter" $ unsafeBuildDynamic getV0 e'
