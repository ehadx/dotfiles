{ config, lib, pkgs, ... }:

{
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  zramSwap.enable = true;

  networking.hostName = "nixos-msi";
  networking.networkmanager.enable = true;

  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";
  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
     font = "Lat2-Terminus16";
     keyMap = "us";
     # useXkbConfig = true; # use xkb.options in tty.
  };

  programs.git.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    xkb = {
      layout = "us,ara";
      variant = ",mac";
      options = "grp:shifts_toggle";
    };
    displayManager = {
      startx.enable = true;
    };
    desktopManager = {
      xterm.enable = false;
    };
  };

  services.flatpak.enable = true;
  environment.variables = {
    XDG_DATA_DIRS = lib.mkDefault "$XDG_DATA_DIRS:/var/lib/flatpak/exports/share:~/.local/share/flatpak/exports/share";
    DOTNET_CLI_TELEMETRY_OPTOUT = 1;
  };

  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config.common.default = "*";
  };
  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.hadi = {
    isNormalUser = true;
    uid = 1000;
    initialPassword = "nix123";
    extraGroups = [ "wheel" "docker" ];
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # services.picom.enable = true;
  # services.libinput.enable = true;
}
