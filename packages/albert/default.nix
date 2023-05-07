{ lib, pkgs, config, ... }: {
  programs.albert.enable = true;

  # Nix albert has transparency issues, so stub out the package and require us
  # to install it manually.
  programs.albert.package = pkgs.emptyDirectory;

  programs.albert.config = {
    General = {
      hotkey = "Alt+Space";
      last_used_version = "0.20.10";
      telemetry = false;
      incrementalSort = true;
      showTray = true;
      terminal = if config.programs.kitty.enable then "launch-kitty" else "tmux new-window -e --";
      fuzzy = true;
      prioritise_perfect_matches = false;
    };

    albert.trigger_enabled = false;

    applications = {
      enabled = true;
      trigger_enabled = false;
      use_generic_name = false;
      use_keywords = false;
      use_non_localized_name = false;
    };

    calculator_muparser.enabled = true;
    pluginregistry.trigger_enabled = false;
    python_eval.enabled = true;
    # mpris.enabled = true;
    mpris_status = {
      enabled = true;
      trigger = "m";
    };
    ssh.enabled = true;
    terminal.enabled = true;

    python = {
      enabled = true;
      watchSources = false;
      enabled_modules = "unicode_emoji2, mpris-state";
    };

    widgetsboxmodel = {
      trigger_enabled = false;
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

  xdg.dataFile."albert/python/plugins/mpris_state" = {
    recursive = true;
    source = ./mpris-state;
  };

  xdg.dataFile."albert/python/plugins/unicode_emoji2" = {
    recursive = true;
    source = ./unicode_emoji2;
  };
}
