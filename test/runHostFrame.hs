-- | A frame run by 'runHostFrame' is a full instant: a build-time 'now'
-- occurrence must reach every hold in the built network, including through
-- combinators (such as merges) whose occurrences are computed by propagation
-- rather than read directly at subscribe time.
import qualified Data.IntMap as IntMap
import Reflex
import Reflex.Host.Class
import Test.Tasty
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
  , testCase "hold on mergeInt of now" $ do
        value <- runSpiderHost $ do
          b <- runHostFrame $ do
            buildTime <- now
            hold "initial" ("fired" <$ mergeInt (IntMap.singleton 0 buildTime))
          runHostFrame $ sample b
        value @?= "fired"
  ]
