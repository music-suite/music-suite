# Format /src with Ormulo <https://github.com/tweag/ormolu>

set -e

export LANG="C.UTF-8"

# Ormolu enables most extensions by default, but some have to be passed
# explicitly. If this list becomes long, think of a better way
# of coordinating extensions between Cabal, doctester and Ormulo.
OR=ormolu\ --ghc-opt=-XTypeApplications
MODE=${MODE:-"inplace"}

$OR --mode=$MODE $(find src       -type f -name "*.hs" | cut -d':' -f1)
$OR --mode=$MODE $(find doc-tools -type f -name "*.hs" | cut -d':' -f1)
