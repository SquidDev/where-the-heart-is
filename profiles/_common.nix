{ config, lib, pkgs, ... }: {
  config = {
    home.homeDirectory = "/home/${config.home.username}";

    # This value determines the Home Manager release that your configuration is compatible with.
    home.stateVersion = "22.05";

    home.language.base = "en_GB.UTF-8";

    programs.home-manager.enable = true;

    # Don't install the man/manpath binary, so we just use the system one. Alternative option here is to set the package
    # to emptyDirectory, but is simpler :).
    programs.man.enable = false;
  };

  options.where-the-heart-is = {
    window-decorations = lib.mkOption {
      type = lib.types.bool;
      default = true;
      example = false;
      description = "Whether to display window decorations.";
    };
  };
}
