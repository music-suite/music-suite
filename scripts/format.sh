# Format /src with Ormulo <https://github.com/tweag/ormolu>

set -e

export LANG="C.UTF-8"

# Ormolu enables most extensions by default, but some have to be passed
# explicitly. If this list becomes long, think of a better way
# of coordinating extensions between Cabal, doctester and Ormulo.
OR=ormolu\ --ghc-opt=-XTypeApplications

$OR --mode inplace  $(find src -type f -name "*.hs" -exec grep -H -c 'CPP' {} \; | grep 0$ | cut -d':' -f1)
$OR --mode inplace  $(find doc-tools -type f -name "*.hs" -exec grep -H -c 'CPP' {} \; | grep 0$ | cut -d':' -f1)
