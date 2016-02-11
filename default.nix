{ mkDerivation, dependent-map, dependent-sum
, mtl, ref-tf, semigroups, these, MemoTrie, exception-transformers, filtrable
, haskell-src-exts, haskell-src-meta
}:
mkDerivation {
  pname = "reflex";
  version = "0.4.0";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  buildDepends = [
    dependent-map dependent-sum mtl ref-tf semigroups these exception-transformers filtrable
    haskell-src-exts haskell-src-meta
  ];
  testDepends = [
    MemoTrie
  ];
  license = null;
}
