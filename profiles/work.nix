{...}: {
  home.username = "jjc";

  where-the-heart-is.window-decorations = true;
  where-the-heart-is.system-packages = true;

  imports = [
    ./_common.nix
    ../packages/emacs
    ../packages/kitty
  ];
}
