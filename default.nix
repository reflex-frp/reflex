{ mkDerivation, dependent-map, dependent-sum
, mtl, ref-tf, semigroups, these
}:
mkDerivation {
  pname = "reflex";
  version = "0.2";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  buildDepends = [
    dependent-map dependent-sum mtl ref-tf semigroups these
  ];
  license = null;
}
