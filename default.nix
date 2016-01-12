{ mkDerivation, dependent-map, dependent-sum
, mtl, ref-tf, semigroups, these, MemoTrie, exception-transformers
, haskell-src-exts, haskell-src-meta, criterion, split
}:
mkDerivation {
  pname = "reflex";
  version = "0.3.2";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  buildDepends = [
    dependent-map dependent-sum mtl ref-tf semigroups these exception-transformers
    haskell-src-exts haskell-src-meta
  ];
  testDepends = [
    MemoTrie criterion split
  ];
  license = null;
}
