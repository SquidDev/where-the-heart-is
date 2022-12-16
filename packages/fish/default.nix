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

    package = pkgs.starship.overrideAttrs(o: {
      # Apply https://github.com/starship/starship/pull/4296/files
      patches = o.patches ++ [./starship-4296.patch];
      doCheck = false;
    });

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
      right_format = lib.concatStrings [
        "$time"
      ];

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
        format = "( $conflicted$staged$renamed$modified$untracked$ahead_behind)";

        ahead      = "[↑$count](fg:#ff00d7)";
        behind     = "[↓$count](fg:#ff00d7)";
        diverged   = "[↑$ahead_count↓$behind_count](fg:#ff00d7)";
        conflicted = "[✖$count](fg:#d70000)";
        deleted    = "[✘$count](fg:#d70000)";
        modified   = "[✹$count](fg:#d75f00)";
        renamed    = "[»$count](purple)";
        staged     = "[✚$count](fg:#5fff00)";
        untracked  = "[✭$count](fg:#d7ff00)";
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
    stdlib = ''
      layout_poetry() {
        PYPROJECT_TOML="${"$"}{PYPROJECT_TOML:-pyproject.toml}"
        if [[ ! -f "$PYPROJECT_TOML" ]]; then
            log_status "No pyproject.toml found. Executing \`poetry init\` to create a \`$PYPROJECT_TOML\` first."
            poetry init
        fi

        VIRTUAL_ENV=$(poetry env info --path 2>/dev/null ; true)

        if [[ -z $VIRTUAL_ENV || ! -d $VIRTUAL_ENV ]]; then
            log_status "No virtual environment exists. Executing \`poetry install\` to create one."
            poetry install
            VIRTUAL_ENV=$(poetry env info --path)
        fi

        PATH_add "$VIRTUAL_ENV/bin"
        export POETRY_ACTIVE=1
        export VIRTUAL_ENV
      }
    '';
  };
}
