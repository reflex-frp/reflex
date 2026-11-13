{ inputs ? (import ./inputs.nix)
, system ? builtins.currentSystem
, pkgs ? (import inputs.nixpkgs { inherit system; })
}:

pkgs.writeShellScriptBin "stylish-haskell" ''
  baseConfig=${./stylish-haskell.yaml}

  # Forward all arguments to stylish-haskell, but collapse repeated config
  # flags to the last one (stylish-haskell itself rejects duplicates). With no
  # config flag, fall back to the bundled config rather than letting
  # stylish-haskell search for a `.stylish-haskell.yaml`; `--config -` reads the
  # config from stdin. Note stylish-haskell has no `--mode check` the way
  # fourmolu does: to check without writing, run
  # `stylish-haskell -i -r src/ && git diff --exit-code`.
  #
  # The config flag has a short form, `-c`, which may appear at the end of a
  # bundled cluster like `-ic FILE` or `-ricFILE`. The last two cases below peel
  # the `c` off such a cluster and forward the remaining flags untouched.
  args=()
  config=$baseConfig
  while [ "$#" -gt 0 ]; do
    case "$1" in
      --config|-c)
        config=$2
        shift 2
        ;;
      --config=*)
        config=''${1#--config=}
        shift
        ;;
      -c?*)
        config=''${1#-c}
        shift
        ;;
      -[dirv]*c)
        args+=("''${1%c}")
        config=$2
        shift 2
        ;;
      -[dirv]*c?*)
        args+=("''${1%%c*}")
        config=''${1#*c}
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
    ${pkgs.lib.getExe pkgs.stylish-haskell} --config "$config" "''${args[@]}"
  else
    exec ${pkgs.lib.getExe pkgs.stylish-haskell} --config "$config" "''${args[@]}"
  fi
''
