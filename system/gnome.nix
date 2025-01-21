{ config, lib, pkgs, ... }:

{
  services.xserver.desktopManager.gnome.enable = true;
  services.xserver.displayManager.gdm.enable = true;

  services.gnome.core-utilities.enable = false;
  services.gnome.core-developer-tools.enable = false;
  services.gnome.games.enable = false;

  # under core services we can customize the following
  hardware.bluetooth.enable = true;
  services.pulseaudio.enable = false;

  services.dleyna-renderer.enable = false;
  services.dleyna-server.enable = false;

  services.power-profiles-daemon.enable = true;
  services.gnome.localsearch.enable = false;
  services.gnome.tinysparql.enable = false;

  services.gnome.gnome-online-accounts.enable = false;

  services.hardware.bolt.enable = true;
  services.packagekit.enable = false;

  services.libinput.enable = true;
  networking.networkmanager.enable = true;

  # under core shell we can customize the following
  services.colord.enable = true;
  services.gnome.gnome-browser-connector.enable = false;

  services.gnome.gnome-initial-setup.enable = false;
  services.gnome.gnome-remote-desktop.enable = false;

  # gnome-user-share is a session service that exports the contents of the Public folder in your home directory
  # so that they can easily be accessed from other computers on the same local network.
  services.gnome.gnome-user-share.enable = false;

  services.gnome.rygel.enable = false;
  services.printing.enable = false;
  services.system-config-printer.enable = config.services.printing.enable;

  services.telepathy.enable = false;
  services.avahi.enable = false;
  services.geoclue2.enable = false;

  environment.gnome.excludePackages = with pkgs; [
    orca
    gnome-tour
    gnome-shell-extensions
  ];
  environment.systemPackages = with pkgs; [
    dconf-editor
    nautilus
    gnome-tweaks

    ## I Think those are already installed
    # gnome-power-manager
    # gnome-screenshot
    # gpaste
  ];

  services.gnome.sushi.enable = true;
  programs.gnome-terminal.enable = false;
}
