{ mkDerivation, base, containers, dependent-map, dependent-sum
, exception-transformers, haskell-src-exts, haskell-src-meta
, MemoTrie, mtl, primitive, ref-tf, semigroups, stdenv, syb
, template-haskell, these, transformers, transformers-compat
, criterion, deepseq, split, stm, loch-th
}:
mkDerivation {
  pname = "reflex";
  version = "0.4.0";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  libraryHaskellDepends = [
    base containers dependent-map dependent-sum exception-transformers
    haskell-src-exts haskell-src-meta mtl primitive ref-tf semigroups
    syb template-haskell these transformers transformers-compat
  ];
  testHaskellDepends = [
    base containers dependent-map MemoTrie mtl ref-tf
    # Benchmark dependencies
    criterion deepseq split stm loch-th
  ];
  homepage = "https://github.com/reflex-frp/reflex";
  description = "Higher-order Functional Reactive Programming";
  license = stdenv.lib.licenses.bsd3;
}
