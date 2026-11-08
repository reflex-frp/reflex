(import
  (let lock = builtins.fromJSON (builtins.readFile ./flake.lock);
   in fetchTarball {
     url = "https://github.com/${lock.nodes.flake-compat.locked.owner}/${lock.nodes.flake-compat.locked.repo}/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
     sha256 = lock.nodes.flake-compat.locked.narHash;
   }) { src = ./.; }
).outputs.inputs
