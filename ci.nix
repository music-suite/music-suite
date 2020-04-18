
# CI config: same as default but without Lilypond/Pandoc/Timidity (so no doc generation!)
#
# TODO unify this expression with default.nix

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
in

pkgs.stdenv.mkDerivation {
  name = "music-suite";
  buildInputs = [
    (
    pkgs.haskellPackages.ghcWithPackages (pkgs:
        [ pkgs.cabal-install
        ])
    )
   ];
  shellHook = ''
  export PS1="music-suite-build> "
  '';
}

