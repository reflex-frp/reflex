{ reflex-platform-fun ? import ./dep/reflex-platform
, supportedSystems ? ["x86_64-linux" "x86_64-darwin"]
}:

let
  native-reflex-platform = reflex-platform-fun {};
  inherit (native-reflex-platform.nixpkgs) lib;

  perPlatform = lib.genAttrs supportedSystems (system: let
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
    variations = map (v: "reflex" + v) [
      "-dontUseTemplateHaskell"
      ""
    ];
    compilerPkgs = lib.genAttrs compilers (ghc: let
      variationPkgs = lib.genAttrs variations (variation: let
        reflex-platform = reflex-platform-fun {
          inherit system;
          enableLibraryProfiling = true;
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
                ver = "0.0.8.0";
                sha256 = "1nnp7jn0vbx9zrnf57dxbknp6fbkqz7bca4i40aa6fabpwjw97kg";
              } {};
            })
            # Use this package's source for reflex
            (self: super: {
              _dep = super._dep // {
                reflex = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [
                  "release.nix"
                  ".git"
                  "dist"
                  "cabal.haskell-ci"
                  "cabal.project"
                  ".travis.yml"
                ])) ./.;
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
