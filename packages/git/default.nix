{ config, lib, pkgs, ... }: {
  programs.git = {
    userName = "Jonathan Coates";

    enable = true;
    aliases = {
      giff = "diff HEAD";
      fixup = "commit --amend --no-edit --date=now";
      update = "fetch --all --prune --multiple";

      # I really should learn the new git commands :)
      unstage = "reset -q HEAD --";
      discard = "checkout --";

      # https://www.erikschierboom.com/2020/02/17/cleaning-up-local-git-branches-deleted-on-a-remote/
      gone = "!git update && git for-each-ref --format '%(refname:short) %(upstream:track)' | awk '$2 == \"[gone]\" {print $1}' | xargs -r git branch -D";
      recommit = "commit -eF .git/COMMIT_EDITMSG";

      # Don't use these much, but I like to keep these around
      plot = "log --graph --abbrev-commit --decorate --date=relative --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)'";
      plota = "log --graph --abbrev-commit --decorate --date=relative --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' --all";
      graph = "log --decorate --oneline --graph";
      stashes = "stash list --format=format:'%C(bold blue)%h%C(reset) %C(bold green)(%ar)%C(reset) %s'";
    };

    includes = [
      { path = "~/.config/git/extra"; }
    ];

    ignores = [
      "/.direnv"
      "/.envrc" # Undecided, but 90% of the time I don't want to check this in.
      "/scratch"
    ];

    attributes = [
      "*.nbt diff=nbt"
    ];

    extraConfig = {
      init.defaultBranch = "main";
      push.default = "simple";
      credential.helper = "store";

      core.autocrlf = "input";
      # core.pager = "less -+X -FR";
      core.sshCommand =
        let ssh = if config.where-the-heart-is.system-packages then "/usr" else "${pkgs.ssh}"; in
        "${ssh}/bin/ssh -o ControlMaster=auto -o ControlPath=~/.ssh/sockets/%r@%h-%p -o ControlPersist=600";

      color.ui = "auto";

      color.diff.meta = "yellow bold";
      color.diff.commit = "green bold";
      color.diff.frag = "magenta bold";
      color.diff.old = "red bold";
      color.diff.new = "green bold";
      color.diff.whitespace = "red reverse";

      color.diff-highlight.oldNormal = "red bold";
      color.diff-highlight.oldHighlight = "red bold 52";
      color.diff-highlight.newNormal = "green bold";
      color.diff-highlight.newHighlight = "green bold 22";

      color.status.untracked = "cyan";

      diff.compactionHeuristic = true;

      # Use https://github.com/vberlier/nbtlib to diff .nbt files
      diff.nbt.textconv = "${pkgs.python3Packages.nbtlib}/bin/nbt --pretty -r";
      diff.nbt.cachetextconv = true;

      pager.status = false;
      pager.branch = false;

      pull.rebase = true;

      fetch.parallel = 0;

      github.user = "SquidDev";
    };

    delta.enable = true;
    extraConfig.delta = {
      hunk-header-decoration-style = "underline";
      hunk-header-line-number-style = "blue bold";
      pager = "less -+X -FR";
    };

    lfs.enable = true;
  };

  home.packages = [
    pkgs.gh
    pkgs.git-absorb
    pkgs.git-sync
    (pkgs.stdenv.mkDerivation {
      name = "git-branches";
      src = ./git-branches;
      unpackPhase = ":";
      installPhase = ''
        install -Dm755 $src $out/bin/git-branches
      '';
    })
  ];
}
