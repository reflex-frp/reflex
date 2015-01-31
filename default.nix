{ cabal, dependentMap, dependentSum, lens
, mtl, semigroups, these, HList
}:
cabal.mkDerivation (self: {
  pname = "reflex";
  version = "0.1";
  src = ./.;
  buildDepends = [
    dependentMap dependentSum lens mtl semigroups these HList
  ];
  meta = {
    description = "Glitch-free Functional Reactive Programming";
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
