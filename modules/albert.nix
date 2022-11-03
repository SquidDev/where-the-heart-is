{ config, lib, pkgs, ... }:

let
  cfg = config.programs.albert;

in {
  options = {
    programs.albert = {
      enable = lib.mkEnableOption "Albert";

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.albert;
        defaultText = lib.literalExpression "pkgs.albert";
        description = "The Albert package to use";
      };

      config = lib.mkOption {
        default = {};
        example = {
          General.hotkey="Alt+Space";
        };
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [cfg.package];
    xdg.configFile."albert/albert.conf".text = lib.generators.toINI {} cfg.config;
  };
}
