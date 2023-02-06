{pkgs, ...}: {
  home.username = "squid";
  home.packages = [
    pkgs.advancecomp # https://github.com/amadvance/advancecomp
    pkgs.bloaty      # https://github.com/google/bloaty
    pkgs.hotspot     # https://github.com/KDAB/hotspot
    pkgs.spotify
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
