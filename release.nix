{ rp-src ? (import <nixpkgs> {}).fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "384cd850f3adf1d404bced2424b5f6efb0f415f2";
    sha256 = "1ws77prqx8khmp8j6br1ij4k2v4dlgv170r9fmg0p1jivfbn8y9d";
  }
}:
let
  rp = import rp-src {};
  inherit (rp.nixpkgs) lib;
  compilers = ["ghc8_4" "ghc8_0" "ghcjs8_4" "ghcjs8_0"];
in lib.genAttrs compilers (ghc: {
  reflex-useTemplateHaskell = rp.${ghc}.callPackage ./. { useTemplateHaskell = true; };
  reflex = rp.${ghc}.callPackage ./. { useTemplateHaskell = false; };
})
