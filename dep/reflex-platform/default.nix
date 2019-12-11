# DO NOT HAND-EDIT THIS FILE
import ((import <nixpkgs> {}).fetchFromGitHub (
  let json = builtins.fromJSON (builtins.readFile ./github.json);
  in { inherit (json) owner repo rev sha256;
       private = json.private or false;
     }
))
