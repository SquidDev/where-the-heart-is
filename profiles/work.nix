{...}: {
  home.username = "jjc";

  where-the-heart-is.window-decorations = true;

  imports = [
    ./_common.nix
    ../packages/kitty
  ];
}
