{pkgs, ...}: {
  home.username = "squid";
  home.packages = [
    pkgs.advancecomp # https://github.com/amadvance/advancecomp
    pkgs.black       # https://black.readthedocs.io/en/stable/
    pkgs.bloaty      # https://github.com/google/bloaty
    pkgs.elan        # https://github.com/leanprover/elan
    pkgs.hotspot     # https://github.com/KDAB/hotspot
    pkgs.poetry      # https://python-poetry.org/
    pkgs.reuse       # https://reuse.software/
    pkgs.spotify
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
