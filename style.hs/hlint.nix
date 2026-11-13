{ inputs ? (import ./inputs.nix)
, system ? builtins.currentSystem
, pkgs ? (import inputs.nixpkgs { inherit system; })
  # `refactor`, which hlint's `--refactor` shells out to, comes from
  # apply-refact. Prefer the default package set, but fall back when that copy
  # is marked broken, as it is on GHC 9.10: it wants ghc-exactprint <1.13 and
  # that set has no such version. `refactor` is a separate executable that only
  # exchanges serialised hints with hlint, so it need not share hlint's
  # compiler; 9.12 is the nearest set where it is both unbroken and cached.
, applyRefact ?
    if !(pkgs.haskellPackages.apply-refact.meta.broken or false)
      then pkgs.haskellPackages.apply-refact
      else pkgs.haskell.packages.ghc912.apply-refact
}:

pkgs.writeShellScriptBin "hlint" ''
  baseConfig=${./hlint.yaml}
  refactor=${pkgs.lib.getExe' applyRefact "refactor"}

  # Forward all arguments to hlint, with the bundled config prepended. Unlike
  # fourmolu, hlint *accumulates* hint files rather than replacing, and later
  # files win, so we pass the bundled config first and leave the user's own
  # `--hint` flags after it in their original order. hlint also always loads
  # its own built-in hints before either.
  #
  # The config flag is `-h`/`--hint` (hlint spells help `-?`, and `-c` is
  # `--color`). `--hint -` reads the config from stdin, which hlint itself does
  # not support; we materialise it into a temporary file.
  #
  # Note that passing any `--hint` stops hlint searching for a project's own
  # `.hlint.yaml`, so a project that wants one must pass it explicitly.
  args=()
  hints=()
  stdinConfig=
  withRefactor=1
  while [ "$#" -gt 0 ]; do
    case "$1" in
      --hint|-h)
        if [ "$2" = "-" ]; then stdinConfig=1; else hints+=("$2"); args+=("--hint=$2"); fi
        shift 2
        ;;
      --hint=*)
        if [ "''${1#--hint=}" = "-" ]; then stdinConfig=1; else hints+=("''${1#--hint=}"); args+=("$1"); fi
        shift
        ;;
      -h?*)
        if [ "''${1#-h}" = "-" ]; then stdinConfig=1; else hints+=("''${1#-h}"); args+=("--hint=''${1#-h}"); fi
        shift
        ;;
      --with-refactor|--with-refactor=*)
        withRefactor=
        args+=("$1")
        shift
        ;;
      *)
        args+=("$1")
        shift
        ;;
    esac
  done

  # Make `--refactor` work out of the box, unless the caller pointed hlint at
  # its own refactor binary.
  if [ -n "$withRefactor" ]; then
    args=("--with-refactor=$refactor" "''${args[@]}")
  fi

  # Don't add the bundled config if the caller already passed a byte-identical
  # copy of it. Restrictions and `ignore`s survive being loaded twice, but a
  # custom `warn`/`hint` rule is reported once per copy, so `hlint.yaml` run as
  # an executable would otherwise report "Use <>" twice.
  base=("--hint=$baseConfig")
  for hint in "''${hints[@]}"; do
    if ${pkgs.diffutils}/bin/cmp -s "$hint" "$baseConfig"; then
      base=()
      break
    fi
  done

  if [ -n "$stdinConfig" ]; then
    # The `.yaml` suffix matters: hlint picks the config format from the file
    # extension, and treats anything else as a pre-2.3 Haskell config.
    config=$(${pkgs.coreutils}/bin/mktemp --suffix=.yaml)
    trap '${pkgs.coreutils}/bin/rm -f "$config"' EXIT
    ${pkgs.coreutils}/bin/cat - > "$config"
    ${pkgs.lib.getExe pkgs.hlint} "''${base[@]}" --hint="$config" "''${args[@]}"
  else
    exec ${pkgs.lib.getExe pkgs.hlint} "''${base[@]}" "''${args[@]}"
  fi
''
