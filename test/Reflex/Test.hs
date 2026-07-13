{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Reflex.Test
  ( toTestTree
  , dumpExpected
  , warnMissingExpected
  , runTests

  , module Reflex.TestPlan

  ) where

import Reflex.Spider

import Reflex.TestPlan

import Reflex.Plan.Pure
import Reflex.Plan.Reflex

import Data.Maybe
import qualified Data.IntMap as IntMap

import System.IO (hPutStr, stderr)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure (expectFailBecause)

import Prelude


toTestTree :: (String, TestCase) -> TestTree
toTestTree (name, tc) = testGroup name (implementationLeaves tc)

implementationLeaves :: TestCase -> [TestTree]
implementationLeaves (TestE meta p) =
  [ leaf reference "pure"   (return (firings pure'))
  , leaf reference "spider" (firings <$> runSpiderHost (runTestE p))
  ]
  where
    firings   = IntMap.mapMaybe id
    pure'     = testEvent (runPure p)
    reference = firings (fromMaybe pure' (expectedOutput meta))
    leaf      = implementationLeaf (expectedFailures meta)
implementationLeaves (TestB meta p) =
  [ leaf reference "pure"   (return pure')
  , leaf reference "spider" (runSpiderHost (runTestB p))
  ]
  where
    pure'     = testBehavior (runPure p)
    reference = fromMaybe pure' (expectedOutput meta)
    leaf      = implementationLeaf (expectedFailures meta)

implementationLeaf :: (Eq a, Show a) => [String] -> a -> String -> IO a -> TestTree
implementationLeaf failures reference name getResult =
  markExpectedFailure $ testCase name $ do
    result <- getResult
    result @?= reference
  where
    markExpectedFailure
      | name `elem` failures =
          expectFailBecause (name <> " is expected to diverge from the reference")
      | otherwise = id


-- | Print a paste-ready 'testE''/'testB'' declaration using Pure as a reference.
dumpExpected :: (String, TestCase) -> IO ()
dumpExpected (name, TestE _ p) =
  putStrLn $ "  , testE' " <> show name <> " "
    <> show (IntMap.toList (IntMap.mapMaybe id (testEvent (runPure p))))
dumpExpected (name, TestB _ p) =
  putStrLn $ "  , testB' " <> show name <> " "
    <> show (IntMap.toList (testBehavior (runPure p)))

warnMissingExpected :: [(String, TestCase)] -> IO ()
warnMissingExpected tests = case length (filter (missingExpected . snd) tests) of
  0 -> return ()
  n -> hPutStr stderr $ unlines
    [ ""
    , "WARNING: " <> show n <> " test case(s) pin no expected output and are only"
    , "checked for agreement against the Pure implementation. To pin a fixed"
    , "expected output, generate the testE'/testB' declarations with"
    , ""
    , "    cabal run semantics -- --dump-expected [name-prefix]"
    , ""
    , "and paste the relevant lines into the test list."
    , ""
    ]
  where
    missingExpected (TestE meta _) = isNothing (expectedOutput meta)
    missingExpected (TestB meta _) = isNothing (expectedOutput meta)

runTests :: [(String, TestCase)] -> IO ()
runTests = defaultMain . testGroup "semantics" . map toTestTree
