{ mkDerivation, ghc, base, bifunctors, containers, deepseq
, dependent-map, dependent-sum, exception-transformers
, haskell-src-exts, haskell-src-meta, hlint, lens, MemoTrie
, monad-control, mtl, primitive, ref-tf, semigroupoids
, semigroups, split, stdenv, stm, syb, template-haskell
, these, transformers, transformers-compat, prim-uniq
, data-default
, useTemplateHaskell ? true
}:
mkDerivation {
  pname = "reflex";
  version = "0.5.0";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  libraryHaskellDepends = [
    base bifunctors containers dependent-map dependent-sum
    exception-transformers lens
    MemoTrie monad-control mtl primitive ref-tf semigroupoids
    semigroups stm syb template-haskell these transformers
    transformers-compat prim-uniq
    base bifunctors containers deepseq dependent-map dependent-sum
    mtl ref-tf split transformers data-default
  ] ++ (if !useTemplateHaskell then [] else [
    haskell-src-exts haskell-src-meta
  ]);
  testHaskellDepends = if ghc.isGhcjs or false then [] else [
    hlint
  ];
  configureFlags = if useTemplateHaskell then [] else [
    "-f-use-template-haskell"
  ];
  homepage = "https://github.com/reflex-frp/reflex";
  description = "Higher-order Functional Reactive Programming";
  license = stdenv.lib.licenses.bsd3;
}
