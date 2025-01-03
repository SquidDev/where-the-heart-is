{ config, lib, pkgs, ... }: {
  config = {
    home.homeDirectory = "/home/${config.home.username}";

    # This value determines the Home Manager release that your configuration is compatible with.
    home.stateVersion = "22.05";

    home.language.base = "en_GB.UTF-8";

    # Sets up all the XDG_ variables to have our nix profile on the path.
    targets.genericLinux.enable = true;

    programs.home-manager.enable = true;

    # Don't install the man/manpath binary, so we just use the system one. Alternative option here is to set the package
    # to emptyDirectory, but is simpler :).
    programs.man.enable = false;

    # Some common programs we always want.
    home.packages = [
      pkgs.bat         # https://github.com/sharkdp/bat
      pkgs.btop        # https://github.com/aristocratos/btop
      pkgs.graphviz
      pkgs.pandoc
      pkgs.shellcheck  # https://github.com/koalaman/shellcheck
      pkgs.tokei       # https://github.com/XAMPPRocky/tokei
      pkgs.tree
    ];
  };

  options.where-the-heart-is = {
    window-decorations = lib.mkOption {
      type = lib.types.bool;
      default = true;
      example = false;
      description = "Whether to display window decorations.";
    };

    system-packages = lib.mkOption {
      type = lib.types.bool;
      default = false;
      example = true;
      description = "Use system packages (those in /usr/bin) rather than Nix-provided one.";
    };
  };

  imports = [
    ../packages/fish
    ../packages/git
  ];
}
