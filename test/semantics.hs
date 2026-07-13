{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Reflex.Test

import Data.Bifunctor
import Data.List
import qualified Reflex.Bench.Focused as Focused
import qualified Reflex.Test.Micro as Micro

import System.Environment

import Prelude

matchPrefixes :: [String] -> (String -> Bool)
matchPrefixes []   = const True
matchPrefixes args = \name -> any (`isPrefixOf` name) args


main :: IO ()
main = do
  args <- getArgs

  case args of
    ("--dump-expected":prefixes) -> mapM_ dumpExpected (filter (matchPrefixes prefixes . fst) allTests)
    _                            -> warnMissingExpected microTests >> runTests allTests

  where
    microTests = makeGroup "micro" Micro.testCases

    allTests = concat
     [ microTests
     , makeGroup "subscribing (100,40)" (Focused.subscribing 100 40)
     , makeGroup "firing 1000" (Focused.firing 1000)
     , makeGroup "merge 100" (Focused.merging 100)
     , makeGroup "fan 50" (Focused.fans 50)
     ]

    makeGroup name tests = first (\test -> intercalate "/" [name, test]) <$> tests
