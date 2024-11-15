{ config, pkgs, ... }:

{
  services.redshift = {
    enable = true;
    tray = true;
    dawnTime = "6:50-7:30";
    duskTime = "16:20-17:20";
    temperature.day = 5500;
    temperature.night = 3000;
  };
}
