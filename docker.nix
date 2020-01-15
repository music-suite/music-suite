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

# Build a Docker image containing Music Suite
# TODO consolidate this expression with default.nix
in

with pkgs;
let
    debian = dockerTools.pullImage
      { imageName = "nixos/nix";
        imageDigest = "sha256:af330838e838cedea2355e7ca267280fc9dd68615888f4e20972ec51beb101d8";
        sha256 = "sha256:0cpiwhk6iiwvl4f45brfrs63g2v1hglk5mpr5zhynx803005w1di";
      };

musicSuite = pkgs.stdenv.mkDerivation {
      name = "music-suite";
      buildInputs = [

        pkgs.lilypond
        (
        pkgs.haskellPackages.ghcWithPackages (pkgs:
            [ pkgs.cabal-install
              pkgs.pandoc
            ])
        )
       ];
      src = builtins.filterSource
                (path: type:
                        (baseNameOf path != ".git")
                        && (baseNameOf path != "dist-newstyle")
                        )
                ./.;
      buildPhase = ''
        export HOME=$TMP
        set -e
        cabal update
        cabal build
      '';
      installPhase = ''
        # TODO copy artefacts
        mkdir -p "$out"
        cp -r dist-newstyle "$out/"
      '';

      shellHook = '';
        export PS1="music-suite-build> "
      '';
    };
in
dockerTools.buildImage {
  name = "music-suite";

  fromImage = debian;

  contents = [  bashInteractive
                glibcLocales
                # TODO add Music Suite build env + products here
                # Later: Remove build env, just runtime dependencies
                musicSuite
                pkgs.lilypond
                (pkgs.haskellPackages.ghcWithPackages (p: []))
             ];

  config = {
    Env = ["LANG=en_US.UTF-8"
           "LOCALE_ARCHIVE=${glibcLocales}/lib/locale/locale-archive"];
    WorkingDir = "/programs";
    Cmd = "/bin/bash";
  };
}
