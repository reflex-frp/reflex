{ mkDerivation, base, bifunctors, containers, deepseq
, dependent-map, dependent-sum, exception-transformers
, haskell-src-exts, haskell-src-meta, hlint, MemoTrie, mtl
, primitive, ref-tf, semigroupoids, semigroups, split, stdenv, stm
, syb, template-haskell, these, transformers, transformers-compat
}:
mkDerivation {
  pname = "reflex";
  version = "0.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors containers dependent-map dependent-sum
    exception-transformers haskell-src-exts haskell-src-meta MemoTrie
    mtl primitive ref-tf semigroupoids semigroups stm syb
    template-haskell these transformers transformers-compat
  ];
  testHaskellDepends = [
    base bifunctors containers deepseq dependent-map dependent-sum
    hlint mtl ref-tf split transformers
  ];
  homepage = "https://github.com/reflex-frp/reflex";
  description = "Higher-order Functional Reactive Programming";
  license = stdenv.lib.licenses.bsd3;
}
