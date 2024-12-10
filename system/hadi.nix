{ config, pkgs, ... }:

{
  imports = [
    ../home/hadi.nix
    ../home/emacs
    ../home/nvim
    ../home/gnome.nix
  ];
}
