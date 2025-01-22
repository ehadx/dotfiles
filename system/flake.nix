{
  description = "My nix system modules";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs, home-manager, ... }:
    {
      modules = {
        shared = ./shared.nix;
        containers = ./containers.nix;
        linux = ./linux.nix;
        mpd = ./mpd.nix;
        nvidia = ./nvidia.nix;
        darwin = ./darwin.nix;
      };
    };
}
