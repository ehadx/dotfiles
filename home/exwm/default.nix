{ config, pkgs, ...}:

{
  imports =
    [
      ../emacs
      ../dunst
      ../redshift.nix
    ];

  home.file.".xinitrc".source = ./.xinitrc;
  home.file.".emacs.d/exwm.el".source = ./exwm.el;

  home.packages = [
    pkgs.brightnessctl
    pkgs.xdg-user-dirs
    pkgs.pwvucontrol
    pkgs.flameshot
  ];

  services.picom.enable = true;
}
