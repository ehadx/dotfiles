{ config, pkgs, ... }:

{
  home.username = "hadi";
  home.homeDirectory = "/home/hadi";
  home.stateVersion = "24.05";

  home.packages = [
    pkgs.direnv
    pkgs.nix-direnv
  ];

  home.sessionVariables = {
    EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.git = {
    enable = true;
    userName = "hadi";
    userEmail = "hadx@tutamail.com";
  };
}
