{ lib, pkgs, config, ... }: {
  programs.albert.enable = true;

  # Nix albert has transparency issues, so stub out the package and require us
  # to install it manually.
  programs.albert.package = pkgs.emptyDirectory;

  programs.albert.config = {
    General = {
      hotkey = "Alt+Space";
      incrementalSort = true;
      showTray = true;
      terminal = if config.programs.kitty.enable then "launch-kitty" else "tmux new-window -e --";
    };

    "org.albert.extension.applications" = {
      enabled = true;
      fuzzy = true;
      use_generic_name = false;
      use_keywords = false;
      use_non_localized_name = false;
    };

    "org.albert.extension.calculator".enabled = true;
    "org.albert.extension.chromium".enabled = false;
    "org.albert.extension.files".enabled = false;
    "org.albert.extension.firefoxbookmarks".enabled = false;
    "org.albert.extension.hashgenerator".enabled = false;
    "org.albert.extension.mpris".enabled = true;
    "org.albert.extension.snippets".enabled = true;
    "org.albert.extension.ssh".enabled = true;
    "org.albert.extension.system".enabled = false;
    "org.albert.extension.terminal".enabled = true;
    "org.albert.extension.websearch".enabled = false;

    "org.albert.extension.python" = {
      enabled = true;
      enabled_modules = "unicode_emoji2, mpris-state";
    };

    "org.albert.frontend.widgetboxmodel" = {
      alwaysOnTop = true;
      clearOnHide = false;
      displayIcons = true;
      displayScrollbar = false;
      displayShadow = true;
      hideOnClose = false;
      hideOnFocusLoss = true;
      itemCount = 5;
      showCentered = true;
      theme = "Arc Dark Grey";
    };
  };

  xdg.dataFile."albert/org.albert.extension.python/modules/mpris-state" = {
    recursive = true;
    source = ./mpris-state;
  };

  xdg.dataFile."albert/org.albert.extension.python/modules/unicode_emoji2" = {
    recursive = true;
    source = ./unicode_emoji2;
  };
}
