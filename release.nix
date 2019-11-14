{ reflex-platform-fun ? import ./dep/reflex-platform
}:

let
  native-reflex-platform = reflex-platform-fun {};
  inherit (native-reflex-platform.nixpkgs) lib;
  systems = ["x86_64-linux" "x86_64-darwin"];

  perPlatform = lib.genAttrs systems (system: let
    reflex-platform = reflex-platform-fun { inherit system; };
    compilers = [
      "ghc"
      "ghcjs"
    ] ++ lib.optionals (reflex-platform.androidSupport) [
      "ghcAndroidAarch64"
      "ghcAndroidAarch32"
    ] ++ lib.optionals (reflex-platform.iosSupport) [
      "ghcIosAarch64"
    ];
    hsPkgs = lib.genAttrs compilers (ghc: let
      ghc' = reflex-platform.${ghc}.override {
        overrides = self: super: {
          reflex-useTemplateHaskell = self.callPackage ./. { useTemplateHaskell = true; splitThese = false; };
          reflex = self.callPackage ./. { useTemplateHaskell = false; splitThese = false; };
        };
      };
    in {
      inherit (ghc') reflex reflex-useTemplateHaskell;
    });
  in hsPkgs // {
    cache = reflex-platform.pinBuildInputs "reflex-${system}"
      (lib.concatLists (map builtins.attrValues (builtins.attrValues hsPkgs)));
  });

  metaCache = native-reflex-platform.pinBuildInputs "reflex-everywhere"
    (map (a: a.cache) (builtins.attrValues perPlatform));

in perPlatform // { inherit metaCache; }
