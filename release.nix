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
    nixpkgsGhcs =
      let
        pkgs = import ./nixpkgs { inherit system; };
        nixGhc945 = pkgs.haskell.packages.ghc945.override {
        };
        nixGhc961 = pkgs.haskell.packages.ghc961.override {
          overrides = self: super: {
            exception-transformers = pkgs.haskell.lib.doJailbreak super.exception-transformers;
            these-lens = self.callHackageDirect {
              pkg = "these-lens";
              ver = "1.0.1.3";
              sha256 = "0n1vkr57jz5yvy4jm15v5cs42rp342ni0gisib7aqyhibpicqs5c";
            } {};
            these = self.callHackageDirect {
              pkg = "these";
              ver = "1.2";
              sha256 = "1iaaq1fsvg8c3l0czcicshkmbbr00hnwkdamjbkljsa1qvlilaf0";
            } {};
            lens = self.callHackageDirect {
              pkg = "lens";
              ver = "5.2.2";
              sha256 = "0c4a421sxfjm1cj3nvgwkr4glll23mqnsvs2iv5qh85931h2f3cy";
            } {};

            assoc = self.callHackageDirect {
              pkg = "assoc";
              ver = "1.1";
              sha256 = "1krvcafrbj98z5hv55gq4zb1in5yd71nmz9zdiqgnywjzbrvpf75";
            } {};

            strict = self.callHackageDirect {
              pkg = "strict";
              ver = "0.5";
              sha256 = "02iyvrr7nd7fnivz78lzdchy8zw1cghqj1qx2yzbbb9869h1mny7";
            } {};

            hlint = self.callHackageDirect {
              pkg = "hlint";
              ver = "3.5";
              sha256 = "1np43k54918v54saqqgnd82ccd6225njwxpg2031asi70jam80x9";
            } {};

            patch = self.callHackageDirect {
              pkg = "patch";
              ver = "0.0.8.2";
              sha256 = "160zqqhjg48fr3a33gffd82qm3728c8hwf8sn37pbpv82fw71rzg";
            } {};
          };
        };
      in
      {
        ghc945 = nixGhc945.callCabal2nix "reflex" (import ./src.nix) {};
        ghc961 = nixGhc961.callCabal2nix "reflex" (import ./src.nix) {};
      };
    compilerPkgs = lib.genAttrs compilers (ghc: let
      variationPkgs = lib.genAttrs variations (variation: let
        reflex-platform = reflex-platform-fun {
          inherit system;
          __useNewerCompiler = true;
          __useTemplateHaskell = variation == "reflex"; # TODO hack
          haskellOverlays = [
            (self: super: import ./overlay.nix { inherit self super; haskellLib = native-reflex-platform.nixpkgs.haskell.lib; })
            # Use this package's source for reflex
            (self: super: {
              _dep = super._dep // { reflex = import ./src.nix; };
            })
          ];
        };
      in reflex-platform.${ghc}.reflex);
    in variationPkgs // {
      cache = reflex-platform.pinBuildInputs "reflex-${system}-${ghc}"
        (builtins.attrValues variationPkgs);
    });
  in compilerPkgs // nixpkgsGhcs // {
    cache = reflex-platform.pinBuildInputs "reflex-${system}"
      (map (a: a.cache) (builtins.attrValues compilerPkgs));
  });

  metaCache = native-reflex-platform.pinBuildInputs "reflex-everywhere"
    (map (a: a.cache) (builtins.attrValues perPlatform));

in perPlatform // { inherit metaCache; }
