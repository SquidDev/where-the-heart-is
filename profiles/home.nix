{pkgs, ...}: {
  home.username = "squid";
  home.packages = [
    pkgs.advancecomp # https://github.com/amadvance/advancecomp
    pkgs.black       # https://black.readthedocs.io/en/stable/
    pkgs.bloaty      # https://github.com/google/bloaty
    pkgs.hotspot     # https://github.com/KDAB/hotspot
    pkgs.komikku     # https://valos.gitlab.io/Komikku/
    pkgs.poetry      # https://python-poetry.org/
    pkgs.reuse       # https://reuse.software/
    pkgs.spotify
    # TODO: pkgs.python311Packages.git-of-theseus # https://github.com/erikbern/git-of-theseus
  ];

  where-the-heart-is.window-decorations = false;

  imports = [
    ./_common.nix
    ../packages/albert
    ../packages/emacs
    ../packages/kitty
    ../packages/vscode.nix
  ];
}
