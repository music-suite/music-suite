# Format /src with Ormulo <https://github.com/tweag/ormolu>
#
# To use this, clone Ormulo 0.0.2.0 from source and build as per its README
#
# Replace the /nix/store/... path below with the build output, if necessary

set -e

export LANG="C.UTF-8"

OR=/nix/store/2598zamzhprq07xvxr8lj6020kms7scm-ormolu-0.0.2.0

$OR/bin/ormolu --mode inplace  $(find src -type f -name "*.hs" -exec grep -H -c 'CPP' {} \; | grep 0$ | cut -d':' -f1)
