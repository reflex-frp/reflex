# Enter a shell for this project, with some choice of compiler. By default, we
# select the version of ghc provided by reflex-platform, but you can choose a
# later version from nixpkgs as well by doing:
# $ nix-shell --argstr compiler "ghc943"
{ compiler ? "reflex-platform" # or "ghc943", "ghc924"
}:
let
  rp = import ./dep/reflex-platform { __useNewerCompiler = true; };
  pkgs = rp.nixpkgs;
  haskellLib = pkgs.haskell.lib;
  system = builtins.currentSystem;
  nixpkgsGhc = ((import ./nixpkgs {}).haskell.packages.${compiler}).override {
    overrides = self: super: import ./overlay.nix { inherit self super haskellLib; };
  };
  reflexEnv = if compiler == "reflex-platform"
    then (import ./release.nix {}).${system}.ghc.reflex.env
    else (nixpkgsGhc.callCabal2nix "reflex" (import ./src.nix) {}).env;
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
