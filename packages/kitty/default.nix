{ config, lib, pkgs, ... }: {
  programs.kitty = {
    enable = true;
    package = lib.mkIf config.where-the-heart-is.system-packages pkgs.emptyDirectory;
    settings = {
      enable_audio_bell = false;

      shell_integration = "enabled";
      allow_remote_control = "socket-only";
      listen_on = "unix:@kitty";

      hide_window_decorations = !config.where-the-heart-is.window-decorations;
      window_padding_width = "2";

      scrollback_lines = "20000";

      # Colours
      background_opacity = "0.9";
      font_family = "DejaVu Sans Mono";
      font_size = "10.0";
      background = "#282c34";
      foreground = "#d6d6d6";

      color0 = "#777777";
      color1 = "#ff6c6b";
      color2 = "#98be65";
      color3 = "#da8548";
      color4 = "#61afef";
      color5 = "#c678dd";
      color6 = "#1f5582";
      color7 = "#cccccc";

      color8 = "#777777";
      color9 = "#ff5f5f";
      color10 = "#98be65";
      color11 = "#d7875f";
      color12 = "#5fafff";
      color13 = "#d787d7";
      color14 = "#005f87";
      color15 = "#cccccc";

      # Tab bar theme
      tab_bar_min_tabs = "1";
      tab_bar_style = "custom";
      tab_powerline_style = "slanted";
      tab_title_template = ''{index}:{title}{'Z' if layout_name == 'stack' and num_windows > 1 else ""}'';

      tab_bar_background = "#38394c";

      active_tab_foreground = "#3e3d31";
      active_tab_background = "#eead0e";
      active_tab_font_style = "bold-italic";
      inactive_tab_foreground = "#61afef";
      inactive_tab_background = "#38394c";
      inactive_tab_font_style = "normal";

      window_logo_alpha = "0.5";

      # We reset all shortcuts so we're in a bit of a clean slate.
      clear_all_shortcuts = true;
      kitty_mod = "ctrl+shift";

      # Required to make --location={v,h}split work
      enabled_layouts = "splits:split_axis=horizontal, stack";
    };
    keybindings = {
      "f5" = "load_config_file";

      "kitty_mod+t" = "new_tab";
      "kitty_mod+q" = "close_window_with_confirmation ignore-shell";

      "kitty_mod+c" = "copy_to_clipboard";
      "kitty_mod+v" = "paste_from_clipboard";

      # Tab management
      "ctrl+alt+1" = "goto_tab 1";
      "ctrl+alt+2" = "goto_tab 2";
      "ctrl+alt+3" = "goto_tab 3";
      "ctrl+alt+4" = "goto_tab 4";
      "ctrl+alt+5" = "goto_tab 5";
      "ctrl+alt+6" = "goto_tab 6";
      "ctrl+alt+7" = "goto_tab 7";
      "ctrl+alt+8" = "goto_tab 8";
      "ctrl+alt+9" = "goto_tab 9";
      "ctrl+alt+0" = "goto_tab 0";

      "kitty_mod+," = "set_tab_title";

      # Shell integration
      "kitty_mod+g" = "show_last_command_output";
      "kitty_mod+h" = "show_scrollback";
      "kitty_mod+a" = "scroll_to_prompt -1";
      "kitty_mod+x" = "scroll_to_prompt 1";

      # Window creation and movement.
      "kitty_mod+2" = "launch --cwd=current --location=hsplit";
      "kitty_mod+3" = "launch --cwd=current --location=vsplit";
      "kitty_mod+alt+2" = "launch --location=hsplit";
      "kitty_mod+alt+3" = "launch --location=vsplit";
      "kitty_mod+z" = "toggle_layout stack";

      "kitty_mod+[" = "move_window left";
      "kitty_mod+]" = "move_window right";

      "kitty_mod+left" = "neighboring_window left";
      "kitty_mod+right" = "neighboring_window right";
      "kitty_mod+up" = "neighboring_window up";
      "kitty_mod+down" = "neighboring_window down";

      # Hints
      "kitty_mod+p>f" = "kitten hints --type path --program @";
      "kitty_mod+p>l" = "kitten hints --type line --program @";
      "kitty_mod+p>y" = "kitten hints --type url";
    };
  };

  xdg.configFile."kitty/tab_bar.py".source = ./tab_bar.py;
}
