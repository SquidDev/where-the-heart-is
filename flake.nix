{
  description = "Home Manager configuration of Jane Doe";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      mkConfig = profile: home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [
          # ./options.nix
          ./profiles/${profile}.nix
        ];
      };
    in rec {
      homeConfigurations.work = mkConfig "work";
      homeConfigurations.home = mkConfig "home";
      apps."${system}" = {
        bootstrap-work = { type = "app"; program = "${homeConfigurations.work.activationPackage}/activate"; };
        bootstrap-home = { type = "app"; program = "${homeConfigurations.home.activationPackage}/activate"; };
      };
    };
}
