{pkgs, ...}: {
  home.username = "squid";
  home.packages = [
    pkgs.spotify
    pkgs.wabt
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
