-- | 'runHostFrame' contains some optimizations which broke Spider when Reflex
-- gained `now`.
import qualified Data.IntMap as IntMap
import Reflex
import Reflex.Host.Class
import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "runHostFrame"
  [ testCase "hold on now" $ do
      value <- runSpiderHost $ do
        b <- runHostFrame $ do
          buildTime <- now
          hold "initial" ("fired" <$ buildTime)
        runHostFrame $ sample b
      value @?= "fired"
  , expectFailBecause "runHostFrame never drains the delayed-merge queue, so the merge's occurrence is lost" $
      testCase "hold on mergeInt of now" $ do
        value <- runSpiderHost $ do
          b <- runHostFrame $ do
            buildTime <- now
            hold "initial" ("fired" <$ mergeInt (IntMap.singleton 0 buildTime))
          runHostFrame $ sample b
        value @?= "fired"
  ]
