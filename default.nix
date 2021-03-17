
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

  addOrmulo =  (
    # This overlay adds Ormolu straight from GitHub.
    self: super:

    let source = super.fetchFromGitHub {
          owner = "tweag";
          repo = "ormolu";
          rev = "de279d80122b287374d4ed87c7b630db1f157642"; # update as necessary
          sha256 = "0qrxfk62ww6b60ha9sqcgl4nb2n5fhf66a65wszjngwkybwlzmrv"; # as well
        };
        ormolu = import source { pkgs = self; };
    in {
      haskell = super.haskell // {
        packages = super.haskell.packages // {
          "${ormolu.ormoluCompiler}" = super.haskell.packages.${ormolu.ormoluCompiler}.override {
            overrides = ormolu.ormoluOverlay;
          };
        };
      };
    });

  pkgs = import (builtins.fetchTarball {
    url = https://github.com/nixos/nixpkgs/archive/8d05772134f17180fb2711d0660702dae2a67313.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0pnyg26c1yhnp3ymzglc71pd9j0567ymqy6il5ywc82bbm1zy25a";
  }) { config = opts; overlays = [ addOrmulo ]; };
in

pkgs.stdenv.mkDerivation {
  name = "music-suite";
  buildInputs = [

    pkgs.lilypond
    pkgs.timidity
    pkgs.cabal-install
    pkgs.haskell.packages.ghc883.ormolu
    # pkgs.haskell.packages.ghc865.cabal-install
    (
    pkgs.haskellPackages.ghcWithPackages (pkgs:
        [
          # pkgs.cabal-install
          pkgs.pandoc
          pkgs.ormolu
        ])
    )
   ];
  shellHook = ''
    function doctests {
        cabal exec doctester --package music-suite -- src/Control && \
        cabal exec doctester --package music-suite -- src/Data && \
        cabal exec doctester --package music-suite -- src/Music/Articulation && \
        cabal exec doctester --package music-suite -- src/Music/Dynamics && \
        cabal exec doctester --package music-suite -- src/Music/Parts && \
        cabal exec doctester --package music-suite -- src/Music/Pitch && \
        # cabal exec doctester --package music-suite -- src/Music/Prelude && \
        # cabal exec doctester --package music-suite -- src/Music/Score && \
        cabal exec doctester --package music-suite -- src/Music/Score/Dynamics && \
        cabal exec doctester --package music-suite -- src/Music/Score/Part && \
        cabal exec doctester --package music-suite -- src/Music/Score/Export && \
        cabal exec doctester --package music-suite -- src/Music/Score/Import && \
        cabal exec doctester --package music-suite -- src/Music/Score/Meta && \
        # cabal exec doctester --package music-suite -- src/Music/Time && \
        true;
    }
    function tests {
        cabal test --test-show-details=streaming --test-options=--color=always && \
            cabal build && \
            doctests && \
            true;
    }
    export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
    export LANG=en_US.UTF-8
    export PS1="# "
  '';
}
