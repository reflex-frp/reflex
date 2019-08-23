{ mkDerivation, ghc, base, bifunctors, containers, deepseq
, dependent-map, dependent-sum, exception-transformers
, haskell-src-exts, haskell-src-meta, hlint, lens, MemoTrie
, monad-control, mtl, primitive, random, ref-tf
, semigroupoids , semigroups, split, stdenv, stm, syb
, template-haskell , these, time, transformers
, transformers-compat, unbounded-delays, prim-uniq
, data-default, filepath, directory, filemanip, ghcjs-base
, monoidal-containers, witherable, profunctors
, semialign ? null, splitThese ? (semialign != null)
, useTemplateHaskell ? true
}:
mkDerivation {
  pname = "reflex";
  version = "0.6.2.3";
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [ ".git" "dist" ])) ./.;
  libraryHaskellDepends = [
    base bifunctors containers dependent-map dependent-sum
    exception-transformers lens
    MemoTrie monad-control mtl primitive ref-tf semigroupoids
    semigroups stm syb template-haskell these transformers
    transformers-compat prim-uniq
    base bifunctors containers deepseq dependent-map dependent-sum
    mtl ref-tf split transformers data-default
    random time unbounded-delays monoidal-containers witherable
    profunctors
  ] ++ (if ghc.isGhcjs or false then [
    ghcjs-base
  ] else []) ++ (if !useTemplateHaskell then [] else [
    haskell-src-exts haskell-src-meta
  ]) ++ (if splitThese then [
    semialign
  ] else []);
  testHaskellDepends = if ghc.isGhcjs or false then [] else [
    hlint filepath directory filemanip
  ];
  configureFlags =
    stdenv.lib.optional (!useTemplateHaskell) [ "-f-use-template-haskell" ] ++
    stdenv.lib.optional (!splitThese) [ "-f-split-these" ];
  homepage = "https://github.com/reflex-frp/reflex";
  description = "Higher-order Functional Reactive Programming";
  license = stdenv.lib.licenses.bsd3;
}
