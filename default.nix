{ mkDerivation, base, bifunctors, containers, data-default, deepseq
, dependent-map, dependent-sum, exception-transformers, ghc
, haskell-src-exts, haskell-src-meta, lens, MemoTrie, monad-control
, mtl, prim-uniq, primitive, ref-tf, semigroupoids, semigroups
, split, stdenv, stm, syb, template-haskell, these, transformers
, transformers-compat
}:
mkDerivation {
  pname = "reflex";
  version = "0.5.0";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  libraryHaskellDepends = [
    base bifunctors containers data-default dependent-map dependent-sum
    exception-transformers ghc haskell-src-exts haskell-src-meta lens
    MemoTrie monad-control mtl prim-uniq primitive ref-tf semigroupoids
    semigroups stm syb template-haskell these transformers
    transformers-compat
  ];
  testHaskellDepends = [
    base bifunctors containers deepseq dependent-map dependent-sum mtl
    ref-tf split transformers
  ];
  homepage = "https://github.com/reflex-frp/reflex";
  description = "Higher-order Functional Reactive Programming";
  license = stdenv.lib.licenses.bsd3;
}
