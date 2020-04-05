
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
    url = https://github.com/nixos/nixpkgs/archive/f74f2f354866c828248a419ef9a2cbddc793b7f9.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1jxb2kb83mrmzg06l7c1zw9pikk2l1lpg8dl0rvni65bgmlxf7xy";
  }) { config = opts; };

  # TODO try adding HIE or ghcide
  # https://github.com/hercules-ci/ghcide-nix/tarball/master"
  # https://github.com/Infinisil/all-hies
in

pkgs.stdenv.mkDerivation {
  name = "music-suite";
  buildInputs = [

    pkgs.lilypond
    pkgs.timidity
    (
    pkgs.haskellPackages.ghcWithPackages (pkgs:
        [ pkgs.cabal-install
          pkgs.pandoc
        ])
    )
   ];
  shellHook = ''
  export PS1="music-suite-build> "
  '';
}
