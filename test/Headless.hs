module Main where

import Reflex
import Reflex.Host.Headless (runHeadlessApp)

main :: IO ()
main = do
  runHeadlessApp $ do
    pb <- getPostBuild
    performEvent (pure <$> pb)
