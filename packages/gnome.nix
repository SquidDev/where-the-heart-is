{ config, lib, pkgs, ... }:
let
  extensions = [
    pkgs.gnomeExtensions.no-overview
    (pkgs.gnomeExtensions.paperwm.overrideAttrs {
      # Use https://github.com/paperwm/PaperWM/pull/799
      src = pkgs.fetchFromGitHub {
        owner = "paperwm";
        repo = "PaperWM";
        rev = "5552216b2e11178470b0deb7027f7fcbafc5006d";
        hash = "sha256-VCfFTciDuYqmsh+EjUqu5wi0UAOis7CqV377+7hvINg=";
      };
    })
    pkgs.gnomeExtensions.vitals
  ];
in {
  home.packages = lib.mkMerge [
    extensions
    (lib.mkIf (!config.where-the-heart-is.system-packages) [ pkgs.gnome.gnome-tweaks ])
  ];

  dconf.settings = {
    "org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions = map (e: e.extensionUuid) extensions;
      disabled-extensions = [];
    };

    "org/gnome/desktop/interface" = {
      show-battery-percentage = true;
    };

    "org/gnome/shell/extensions/vitals" = {
      hot-sensors = ["_memory_usage_" "__temperature_max__" "_processor_usage_"];
    };

    "org/gnome/shell/extensions/paperwm" = {
      use-default-background = true;
      show-window-position-bar = false;
    };
  };
}
