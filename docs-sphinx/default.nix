let
  env = import ./nix/devenv.nix;
  deps = env.deps;
  pkgs = env.pkgs;
in
pkgs.stdenv.mkDerivation {
  name = "dev";
  buildInputs = deps;
  shellHook = ''
  '';
}
