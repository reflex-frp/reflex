{ inputs ? (import ./inputs.nix)
, system ? builtins.currentSystem
, pkgs ? (import inputs.nixpkgs { inherit system; })
, fourmoluSrc ?
    if builtins.pathExists ./deps/fourmolu/thunk.nix
      then import ./deps/fourmolu/thunk.nix
      else ./deps/fourmolu
}:

let haskellPackages = pkgs.haskellPackages;

    # The GHC 9.10 set's defaults for these two are older than the fork accepts.
    fourmoluDeps = {
      ghc-lib-parser = haskellPackages.ghc-lib-parser_9_14_1_20251220;
      Cabal-syntax = haskellPackages.Cabal-syntax_3_16_1_0;
    };

    fourmoluPackage = pkgs.haskell.lib.compose.dontCheck
      (haskellPackages.callCabal2nix "fourmolu" fourmoluSrc fourmoluDeps);

    fourmolu = pkgs.haskell.lib.compose.justStaticExecutables fourmoluPackage;
in

pkgs.writeShellScriptBin "fourmolu" ''
  baseConfig=${./fourmolu.yaml}

  # Forward all arguments to fourmolu, but collapse repeated `--config` flags to
  # the last one (fourmolu itself rejects duplicates). With no `--config`, fall
  # back to the bundled config; `--config -` reads the config from stdin.
  args=()
  config=$baseConfig
  while [ "$#" -gt 0 ]; do
    case "$1" in
      --config)
        config=$2
        shift 2
        ;;
      --config=*)
        config=''${1#--config=}
        shift
        ;;
      *)
        args+=("$1")
        shift
        ;;
    esac
  done

  if [ "$config" = "-" ]; then
    config=$(${pkgs.coreutils}/bin/mktemp)
    trap '${pkgs.coreutils}/bin/rm -f "$config"' EXIT
    ${pkgs.coreutils}/bin/cat - > "$config"
    ${pkgs.lib.getExe fourmolu} --config "$config" "''${args[@]}"
  else
    exec ${pkgs.lib.getExe fourmolu} --config "$config" "''${args[@]}"
  fi
''
