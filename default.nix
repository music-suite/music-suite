
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
    url = https://github.com/nixos/nixpkgs/archive/1158f3463912d54cc981d61213839ec6c02570d3.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1v94p8mn3kw3yq79jhmrg0a7zam34v9pvx1sz534y737k2cwbx41";
  }) { config = opts; overlays = [ addOrmulo ]; };
in

pkgs.stdenv.mkDerivation {
  name = "music-suite";
  buildInputs = [

    pkgs.lilypond
    pkgs.timidity
    pkgs.cabal-install
    pkgs.haskellPackages.hasktags
    pkgs.haskell.packages.ghc884.ormolu
    (
    pkgs.haskellPackages.ghcWithPackages (pkgs:
        [
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
        cabal exec doctester --package music-suite -- src/Music/Notation && \
        cabal exec doctester --package music-suite -- src/Music/Parts && \
        cabal exec doctester --package music-suite -- src/Music/Pitch && \
        # cabal exec doctester --package music-suite -- src/Music/Prelude && \
        # cabal exec doctester --package music-suite -- src/Music/Score && \
        cabal exec doctester --package music-suite -- src/Music/Score/Dynamics && \
        cabal exec doctester --package music-suite -- src/Music/Score/Part && \
        # TODO: strange import error:
        #  cabal exec doctester --package music-suite -- src/Music/Score/Export && \
        cabal exec doctester --package music-suite -- src/Music/Score/Import && \
        cabal exec doctester --package music-suite -- src/Music/Score/Meta && \
        cabal exec doctester --package music-suite -- src/Music/Score/Meta.hs && \
        cabal exec doctester --package music-suite -- src/Music/Time && \
        true;
    }
    function tests {
        cabal test --test-show-details=streaming --test-options=--color=always && \
            cabal build && \
            doctests && \
            true;
    }
    function ci {
            tests && cabal build && cabal haddock
    }
    export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
    export LANG=en_US.UTF-8
    export PS1="# "
  '';
}
