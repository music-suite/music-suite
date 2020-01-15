FROM nixos/nix:2.3@sha256:af330838e838cedea2355e7ca267280fc9dd68615888f4e20972ec51beb101d8
COPY src suite/src
COPY music-suite.cabal suite/music-suite.cabal
COPY cabal.project suite/cabal.project
COPY cabal.project.freeze suite/cabal.project.freeze
COPY default.nix suite/default.nix
RUN (cd suite && nix-shell --pure --command "cabal update && cabal build")
