{ reflex-platform-fun ? import ./dep/reflex-platform
, supportedSystems ? ["x86_64-linux" "x86_64-darwin"]
}:

let
  native-reflex-platform = reflex-platform-fun { __useNewerCompiler = true; };
  inherit (native-reflex-platform.nixpkgs) lib;

  perPlatform = lib.genAttrs supportedSystems (system: let
    reflex-platform = reflex-platform-fun { inherit system;  __useNewerCompiler = true; };
    compilers = [
      "ghc"
      "ghcjs"
    ] ++ lib.optionals (reflex-platform.androidSupport) [
      "ghcAndroidAarch64"
      "ghcAndroidAarch32"
    ] ++ lib.optionals (reflex-platform.iosSupport) [
      "ghcIosAarch64"
    ];
    variations = map (v: "reflex" + v) [
      "-dontUseTemplateHaskell"
      ""
    ];
    compilerPkgs = lib.genAttrs compilers (ghc: let
      variationPkgs = lib.genAttrs variations (variation: let
        reflex-platform = reflex-platform-fun {
          inherit system;
          __useNewerCompiler = true;
          __useTemplateHaskell = variation == "reflex"; # TODO hack
          haskellOverlays = [
            (self: super: {
              commutative-semigroups = self.callHackageDirect {
                pkg = "commutative-semigroups";
                ver = "0.1.0.0";
                sha256 = "0xmv20n3iqjc64xi3c91bwqrg8x79sgipmflmk21zz4rj9jdkv8i";
              } {};
              patch = self.callHackageDirect {
                pkg = "patch";
                ver = "0.0.8.1";
                sha256 = "0q5rxnyilhbnfph48fnxbclggsbbhs0pkn0kfiadm0hmfr440cgk";
              } {};
            })
            # Use this package's source for reflex
            (self: super: {
              _dep = super._dep // {
                reflex = import ./src.nix;
              };
            })
          ];
        };
      in reflex-platform.${ghc}.reflex);
    in variationPkgs // {
      cache = reflex-platform.pinBuildInputs "reflex-${system}-${ghc}"
        (builtins.attrValues variationPkgs);
    });
  in compilerPkgs // {
    cache = reflex-platform.pinBuildInputs "reflex-${system}"
      (map (a: a.cache) (builtins.attrValues compilerPkgs));
  });

  metaCache = native-reflex-platform.pinBuildInputs "reflex-everywhere"
    (map (a: a.cache) (builtins.attrValues perPlatform));

in perPlatform // { inherit metaCache; }
