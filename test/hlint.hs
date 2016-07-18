import Language.Haskell.HLint3 (hlint)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  ideas <- hlint
    [ "."
    , "--ignore=Redundant do"
    , "--ignore=Use camelCase"
    , "--ignore=Redundant $"
    , "--ignore=Use &&&"
    , "--ignore=Use const"
    , "--ignore=Use >=>"
    , "--ignore=Use ."
    ]
  if null ideas then exitSuccess else exitFailure
