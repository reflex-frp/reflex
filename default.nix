{ mkDerivation, dependent-map, dependent-sum
, mtl, ref-tf, semigroups, these, MemoTrie, exception-transformers
, haskell-src-exts, haskell-src-meta, criterion, deepseq, split, stm
, atomic-primops
}:
mkDerivation {
  pname = "reflex";
  version = "0.4.0";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  buildDepends = [
    atomic-primops
    dependent-map
    dependent-sum
    exception-transformers
    haskell-src-exts
    haskell-src-meta
    mtl
    ref-tf
    semigroups
    these
  ];
  testDepends = [
    MemoTrie
    criterion
    deepseq
    split
    stm
  ];
  license = null;
}
