
let
  opts = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          # No overrides
        };
      };
    };
  };

  pkgs = import (builtins.fetchTarball {
    url = https://github.com/nixos/nixpkgs/archive/4c810e70efa185b814b843119a9b332ce3bb55d4.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1x4ivhrdfp93hhrns32gw9ns83pfzj80wqv4sadn4wz891hl78ww";
  }) { config = opts; };

  # TODO try adding HIE or ghcide
  # https://github.com/hercules-ci/ghcide-nix/tarball/master"
  # https://github.com/Infinisil/all-hies
in

pkgs.stdenv.mkDerivation {
  name = "build1";
  buildInputs = [

    pkgs.lilypond
    (
    pkgs.haskellPackages.ghcWithPackages (pkgs:
        [ pkgs.cabal-install
          pkgs.pandoc
        ])
    )
   ];
  shellHook = ''
    export PS1="build1> "
  '';
}
