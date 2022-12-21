{ compiler ? "reflex-platform" # or "ghc943", "ghc924"
}:
let
  rp = import ./dep/reflex-platform { __useNewerCompiler = true; };
  pkgs = rp.nixpkgs;
  system = builtins.currentSystem;
  reflexEnv = if compiler == "reflex-platform"
    then (import ./release.nix {}).${system}.ghc.reflex.env
    else ((import ./nixpkgs {}).haskell.packages.${compiler}.callCabal2nix "reflex" (import ./src.nix) {}).env;
in
  pkgs.mkShell {
    name = "shell";
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
    ];
    inputsFrom = [
      reflexEnv
    ];
  }
