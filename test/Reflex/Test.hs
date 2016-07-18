{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Reflex.Test
  ( testAgreement
  , compareResult
  , runTests

  , module Reflex.TestPlan

  ) where

import Reflex.Spider

import Reflex.TestPlan

import Reflex.Plan.Pure
import Reflex.Plan.Reflex

import Control.Monad
import Data.Monoid

import Data.IntMap (IntMap)

--import Data.Foldable
import System.Exit

import Prelude


testAgreement :: TestCase -> IO Bool
testAgreement (TestE p) = do
  spider <- runSpiderHost $ runTestE p
  let results = [("spider", spider)]

  compareResult results (testEvent $ runPure p)

testAgreement (TestB p) = do
  spider <- runSpiderHost $ runTestB p
  let results = [("spider", spider)]

  compareResult results (testBehavior $ runPure p)


compareResult :: (Show a, Eq a) => [(String, IntMap a)] -> IntMap a -> IO Bool
compareResult results expected = fmap and $ forM results $ \(name, r) -> do

  when (r /= expected) $ do
    putStrLn ("Got: " ++ show (name, r))
    putStrLn ("Expected: " ++ show expected)
  return (r == expected)


runTests :: [(String, TestCase)] -> IO ()
runTests testCases = do
   results <- forM testCases $ \(name, test) -> do
     putStrLn $ "Test: " <> name
     testAgreement test
   exitWith $ if and results
              then ExitSuccess
              else ExitFailure 1

