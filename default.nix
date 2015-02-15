{ mkDerivation, dependent-map, dependent-sum, lens
, mtl, semigroups, these
}:
mkDerivation {
  pname = "reflex";
  version = "0.1";
  src = ./.;
  buildDepends = [
    dependent-map dependent-sum lens mtl semigroups these
  ];
  license = null;
}
