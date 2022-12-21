{ haskellLib, self, super }:
{
  # jailbreak here because callHackageDirect doesn't give us a way to get the latest revision of a package
  # 0.1.0.0-r3 would work just fine
  commutative-semigroups = haskellLib.doJailbreak (self.callHackageDirect {
    pkg = "commutative-semigroups";
    ver = "0.1.0.0";
    sha256 = "0xmv20n3iqjc64xi3c91bwqrg8x79sgipmflmk21zz4rj9jdkv8i";
  } {});
  patch = self.callHackageDirect {
    pkg = "patch";
    ver = "0.0.8.1";
    sha256 = "0q5rxnyilhbnfph48fnxbclggsbbhs0pkn0kfiadm0hmfr440cgk";
  } {};
  hlint = self.callHackageDirect {
    pkg = "hlint";
    ver = "3.5";
    sha256 = "1np43k54918v54saqqgnd82ccd6225njwxpg2031asi70jam80x9";
  } {};
}
