let
  src = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  pkgs =
    import
      (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${src.rev}.tar.gz";
        sha256 = src.sha256;
      }) { };

  deps-client = [
    pkgs.curl
    pkgs.dhall
  ];

  deps = [
    pkgs.python38Packages.sphinx
    # pkgs.python38Packages.recommonmark
    # pkgs.sphinx_rtd_theme
    # pkgs.python38Packages.pip
    pkgs.python38Packages.sphinx_rtd_theme
  ];

in
{ deps = deps; deps-client = deps-client; pkgs = pkgs; }
