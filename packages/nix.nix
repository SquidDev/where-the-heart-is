{ pkgs, config, inputs, ... }: {
  nix.package = pkgs.nix;
  nix.settings = {
    experimental-features = ["nix-command" "flakes" "repl-flake"];
    extra-platforms = ["aarch64-linux" "arm-linux"];

    max-jobs = 4;
    cores = 4;
    keep-outputs = true;
    keep-derivations = true;

    # So this is fairly useless (you need to mirror to the global nix config),
    # but good to have as a reference!
    substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
      "https://1lab.cachix.org"
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "1lab.cachix.org-1:eYjd9F9RfibulS4OSFBYeaTMxWojPYLyMqgJHDvG1fs="
    ];
  };

  # "nix run nixpkgs#xxx" uses version of nixpkgs instead.
  nix.registry.nixpkgs.flake = inputs.nixpkgs;

  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    inputs.emacs-overlay.overlay
  ];
}
