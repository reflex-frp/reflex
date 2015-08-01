{ mkDerivation, dependent-map, dependent-sum
, mtl, ref-tf, semigroups, these, MemoTrie, exception-transformers
}:
mkDerivation {
  pname = "reflex";
  version = "0.3";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  buildDepends = [
    dependent-map dependent-sum mtl ref-tf semigroups these exception-transformers
  ];
  testDepends = [
    MemoTrie
  ];
  license = null;
}
