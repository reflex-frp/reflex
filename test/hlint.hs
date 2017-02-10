import Language.Haskell.HLint3 (hlint)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  ideas <- hlint
    [ "."
    , "--ignore=Redundant do"
    , "--ignore=Use camelCase"
    , "--ignore=Redundant $"
    , "--ignore=Use &&"
    , "--ignore=Use &&&"
    , "--ignore=Use const"
    , "--ignore=Use >=>"
    , "--ignore=Use ."
    , "--cpp-define=USE_TEMPLATE_HASKELL"
    ]
  if null ideas then exitSuccess else exitFailure
