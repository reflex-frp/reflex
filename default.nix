{ mkDerivation, dependent-map, dependent-sum
, mtl, ref-tf, semigroups, these, MemoTrie
}:
mkDerivation {
  pname = "reflex";
  version = "0.1";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  buildDepends = [
    dependent-map dependent-sum mtl ref-tf semigroups these
  ];
  testDepends = [
    MemoTrie
  ];
  license = null;
}
