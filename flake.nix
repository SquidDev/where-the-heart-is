{
  description = "Home Manager configuration of Jane Doe";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, home-manager, emacs-overlay }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; config.allowUnfree = true; };
      mkConfig = profile: home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [
          ./modules
          ./profiles/${profile}.nix
        ];

        extraSpecialArgs = {
          inherit emacs-overlay;
        };
      };
    in rec {
      inherit pkgs;

      devShells."${system}".default = pkgs.mkShell {
        buildInputs = [
          home-manager.packages."${system}".default
        ];
      };

      homeConfigurations.home = mkConfig "home";
      homeConfigurations.remote = mkConfig "remote";
      homeConfigurations.work = mkConfig "work";
      apps."${system}" = {
        bootstrap-home = { type = "app"; program = "${homeConfigurations.home.activationPackage}/activate"; };
        bootstrap-remote = { type = "app"; program = "${homeConfigurations.remote.activationPackage}/activate"; };
        bootstrap-work = { type = "app"; program = "${homeConfigurations.work.activationPackage}/activate"; };
      };
    };
}
