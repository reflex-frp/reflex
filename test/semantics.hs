{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Reflex.Test

import Data.Bifunctor
import Data.Functor
import Data.List
import qualified Reflex.Bench.Focused as Focused
import qualified Reflex.Test.Micro as Micro

import System.Environment
import System.Exit

import Prelude

matchPrefixes :: [String] -> (String -> Bool)
matchPrefixes []   = const True
matchPrefixes args = \name -> any (`isPrefixOf` name) args


main :: IO ()
main = do
  args <- getArgs

  case args of
    ["--list"] -> mapM_ putStrLn (fst <$> allTests) >> exitWith (ExitFailure 1)
    _          -> case filter (matchPrefixes args . fst) allTests of
                    []    -> putStrLn "filter did not match any tests" >> exitWith (ExitFailure 1)
                    tests -> runTests tests

  where
    allTests = concat
     [ makeGroup "micro" Micro.testCases
     , makeGroup "subscribing (100,40)" (Focused.subscribing 100 40)
     , makeGroup "firing 1000" (Focused.firing 1000)
     , makeGroup "merge 100" (Focused.merging 100)
     , makeGroup "fan 50" (Focused.fans 50)
     ]

    makeGroup name tests = first (\test -> intercalate "/" [name, test]) <$> tests


