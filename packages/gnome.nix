{ config, lib, pkgs, ... }:
let
  extensions = [
    pkgs.gnomeExtensions.no-overview
    pkgs.gnomeExtensions.paperwm
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
