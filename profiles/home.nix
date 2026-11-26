{pkgs, ...}: {
  home.username = "squid";
  home.packages = [
    pkgs.advancecomp # https://github.com/amadvance/advancecomp
    pkgs.hotspot     # https://github.com/KDAB/hotspot
    # TODO: pkgs.python311Packages.git-of-theseus # https://github.com/erikbern/git-of-theseus
  ];

  where-the-heart-is.window-decorations = false;
  where-the-heart-is.system-packages = true;

  services.syncthing.enable = true;

  imports = [
    ./_common.nix
    ../packages/emacs
    ../packages/gnome.nix
    ../packages/kitty
    ../packages/move-screenshots
    ../packages/vscode.nix
  ];

  # Flatpak applications:
  #
  #   $ flatpak list --app --columns=application,name
  #   Application ID                              Name
  #   cc.craftos_pc.CraftOS-PC                    CraftOS-PC
  #   info.febvre.Komikku                         Komikku
  #   io.github.seadve.Kooha                      Kooha
  #   md.obsidian.Obsidian                        Obsidian
  #   org.prismlauncher.PrismLauncher             Prism Launcher
}
