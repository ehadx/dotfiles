{ config, pkgs, ... }:

{ system.stateVersion = "24.11";
  programs.dconf.enable = true;

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;

  home-manager.users.hadx = ({ config, pkgs, ... }: {

    programs.home-manager.enable = true;

    imports = [
      ./emacs/home.nix
      ./gnome/home.nix
      ./user/home.nix
      ./user/system.nix
    ];
  });

  imports = [
    <home-manager/nixos>
    ./hardware-configuration.nix
		./system/shared.nix
    ./system/linux.nix
    ./system/nvidia.nix
    ./system/containers.nix
		./emacs/system.nix
		./gnome/system.nix
  ];
}
