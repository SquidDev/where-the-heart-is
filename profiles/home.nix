{pkgs, ...}: {
  home.username = "squid";
  home.packages = [
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
