{
  description = "Personal NixOS Configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { 
    nixpkgs,
    home-manager,
    ... 
  }:
    let
      system = "x86_64-linux";
      lib = nixpkgs.lib;
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      nixosConfigurations = {
        nixos-msi = lib.nixosSystem {
          inherit system;
          modules = [
            ./system/hardware-configuration.nix
            ./system/configuration.nix
            ./system/gnome.nix
            home-manager.nixosModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.hadi = import ./system/hadi.nix;
            }
          ];
        };
      };

      homeConfigurations = {
        hadi = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules =
            [
              ./home/hadi.nix
              ./home/emacs
              ./home/nvim
              ./home/gnome.nix
              # ./home/exwm
            ];
        };
      };
    };
}
