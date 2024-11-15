{ config, pkgs, ... }:

{
  services.dunst.enable = true;
  services.dunst.configFile = ./dunstrc;
}
