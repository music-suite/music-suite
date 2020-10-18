#!/usr/bin/env bash
set -euo pipefail

channel='nixos-unstable'
nix-prefetch-git \
  https://github.com/nixos/nixpkgs-channels.git \
  --rev "refs/heads/${channel}" \
  > ./nix/nixpkgs.json
