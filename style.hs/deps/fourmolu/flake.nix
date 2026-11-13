# DO NOT HAND-EDIT THIS FILE
{
  description = "nix-thunk packed thunk";
  inputs = {
    "src" = {
      flake = false;
      owner = "obsidiansystems";
      repo = "fourmolu";
      rev = "0d07f766a95bd3e84c7967407f424b4e1f517157";
      type = "github";
    };
  };
  outputs = { self, src }: { inherit src; };
}
