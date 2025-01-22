{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    fastfetch
    htop
  ];

  fonts.packages = with pkgs; [
    #noto-fonts
    #noto-fonts-cjk-sans
    #noto-fonts-emoji
    #dina-font
    #iosevka-comfy.comfy-motion-fixed
    # nerd-fonts.sauce-code-pro
    victor-mono
    iosevka-comfy.comfy-wide-motion
    nerd-fonts.jetbrains-mono
    nerd-fonts.iosevka
    nerd-fonts.victor-mono
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
}
