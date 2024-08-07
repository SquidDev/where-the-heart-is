{pkgs, ...}: {
  home.username = "jjc";
  home.packages = [
    pkgs.gtkwave
    pkgs.klayout
  ];

  where-the-heart-is.window-decorations = true;
  where-the-heart-is.system-packages = true;

  imports = [
    ./_common.nix
    ../packages/emacs
    ../packages/kitty
  ];
}
