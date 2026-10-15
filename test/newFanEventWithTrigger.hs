{-# LANGUAGE GADTs #-}
-- | Regression test: 'fireEventsAndRead' fires all of its triggers in one
-- frame. This must also hold when two of the triggers are keys of one root
-- created by 'newFanEventWithTrigger'.
import Control.Monad.IO.Class (liftIO)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Misc (Const2 (..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map as Map
import Reflex
import Reflex.Host.Class
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "newFanEventWithTrigger"
  [ testCase "two keys fired in one frame both occur" $ do
      occurrences <- runSpiderHost $ do
        triggersRef <- liftIO $ newIORef (Map.empty :: Map.Map Int (EventTrigger Spider Int))
        selector <- newFanEventWithTrigger $ \(Const2 key) trigger -> do
          modifyIORef' triggersRef $ Map.insert key trigger
          pure $ modifyIORef' triggersRef $ Map.delete key
        (handle1, handle2) <- runHostFrame $ (,)
          <$> subscribeEvent (select selector (Const2 1 :: Const2 Int Int Int))
          <*> subscribeEvent (select selector (Const2 2 :: Const2 Int Int Int))
        triggers <- liftIO $ readIORef triggersRef
        let readHandle handle = readEvent handle >>= sequence
        fireEventsAndRead [triggers Map.! 1 :=> Identity 10, triggers Map.! 2 :=> Identity 20] $
          (,) <$> readHandle handle1 <*> readHandle handle2
      occurrences @?= (Just 10, Just 20)
  ]
