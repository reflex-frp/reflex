{ rp ? import ./reflex-platform.nix {}
}:
let
  inherit (rp.nixpkgs) lib;
  compilers = ["ghc8_4" "ghc8_0" "ghcjs8_4" "ghcjs8_0"];
in lib.attrValues (lib.genAttrs compilers (ghc: {
  reflex-useTemplateHaskell = rp.${ghc}.callPackage ./. { useTemplateHaskell = true; splitThese = false; };
  reflex = rp.${ghc}.callPackage ./. { useTemplateHaskell = false; splitThese = false; };
}))
