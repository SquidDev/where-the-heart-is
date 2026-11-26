{lib, pkgs, ...}: {
  programs.fish = {
    enable = true;

    functions.fish_title = {
      description = "Write out the title";
      body = "printf 'terminal: %s (%s)' (status current-command) (pwd)";
    };

    shellInit = builtins.readFile ./init.fish;
  };

  programs.starship = {
    enable = true;

    package = pkgs.starship;

    settings = {
      add_newline = false;

      format = lib.concatStrings [
        "$hostname"
        "$directory"
        "(\\($git_branch$git_state$git_status\\) )"
        "$java"
        "$ocaml"
        "$nix_shell"
        "$character"
      ];
      right_format = "";

      character = {
        success_symbol="[ᐅ](fg:#5fd75f)";
        error_symbol="[ᐅ](fg:#d7005f)";
      };

      directory = {
        style = "fg:#87d7ff";
        fish_style_pwd_dir_length = 1;
      };

      git_branch = {
        format = "[$symbol$branch(:$remote_branch)]($style)";
        style = "fg:#61afef";
      };

      git_status = {
        format = "( $conflicted$staged$deleted$renamed$modified$untracked$ahead_behind)";

        ahead      = "[↑$count](fg:#ff00d7)";
        behind     = "[↓$count](fg:#ff00d7)";
        diverged   = "[↑$ahead_count↓$behind_count](fg:#ff00d7)";
        conflicted = "[✖$count](fg:#d70000)";
        deleted    = "[✘$count](fg:#d70000)";
        modified   = "[✹$count](fg:#d75f00)";
        renamed    = "[»$count](purple)";
        staged     = "[✚$count](fg:#5fff00)";
        untracked  = "[✭$count](fg:#d7ff00)";

        # gitoxide has some odd bugs which I can't track down. Just use git for now.
        use_git_executable = true;
      };

      hostname.style = "fg:#f18903";

      java = {
        format = "[\\($symbol($version)\\)]($style) ";
        style = "bold fg:#f18903";
        symbol = "☕";
      };

      nix_shell = {
        format = "[$symbol]($style)";
        symbol = "❄️";
      };

      ocaml = {
        format = "[\\($symbol$switch_name\\)]($style) ";
        style = "bold fg:#f18903";
        symbol = "🐫";
      };

      time = {
        disabled = false;
        format = "[$time]($style)";
        style = "fg:#87d7ff";
      };
    };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
