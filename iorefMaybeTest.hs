{-# LANGUAGE CPP #-}
import Control.Monad
#if USE_IOREF_MAYBE
import Data.IORef.Maybe
#define new newIORefMaybe
#define read readIORefMaybe
#define write writeIORefMaybe
#else
import Data.IORef
#define new newIORef
#define read readIORef
#define write writeIORef
#endif

main :: IO ()
main = do
  iorefs <- forM [1 :: Int .. 1000000] $ \n -> new $ Just n
  replicateM_ 100 $ forM_ iorefs $ \r -> do
    Just n <- read r
    write r $! Just $! succ n
