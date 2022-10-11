{...}: {
  home.username = "squid";

  where-the-heart-is.window-decorations = false;

  imports = [
    ./_common.nix
    ../packages/kitty
    ../packages/java
  ];
}
