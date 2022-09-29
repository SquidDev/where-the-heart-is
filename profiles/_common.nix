{ config, lib, ... }: {
  config = {
    home.homeDirectory = "/home/${config.home.username}";

    # This value determines the Home Manager release that your configuration is compatible with.
    home.stateVersion = "22.05";

    programs.home-manager.enable = true;
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
