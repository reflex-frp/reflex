builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [
  "release.nix"
  ".git"
  "dist"
  "cabal.haskell-ci"
  "cabal.project"
  ".travis.yml"
])) ./.
