{
  description = "My emacs configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }:
    let
      emacsHomeConfig = { config, pkgs, ... }:
        {
          programs.emacs.enable = true;
          home.file.".emacs.d/init.el".source = ./init.el;
          home.file.".emacs.d/early-init.el".source = ./early-init.el;
          home.file.".emacs.d/eglot-conf.el".source = ./eglot-conf.el;
        };
    in {
      inherit emacsHomeConfig;
      defaultPackage.x86_64-linux = emacsHomeConfig;
    };
}
