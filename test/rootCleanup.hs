import Control.Monad
import Control.Concurrent
import Reflex
import Reflex.Host.Class
import System.Mem
import Data.IORef
import System.Exit

main :: IO ()
main = do
  numSubscriptions <- newIORef 0
  replicateM_ 1000 $ do
    runSpiderHost $ do
      e <- newEventWithTrigger $ \_ -> do
        modifyIORef' numSubscriptions succ
        return $ modifyIORef' numSubscriptions pred
      _ <- hold () e
      return ()
    replicateM_ 100 $ do
      performMajorGC
      threadDelay 1
  n <- readIORef numSubscriptions
  if n == 0
    then putStrLn "Succeeded"
    else putStrLn "Failed" >> exitFailure
