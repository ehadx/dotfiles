{
  description = "My gnome configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }:
    {
      system-conf = { config, pkgs, lib, ...} : {
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
      };

      home-conf = { config, pkgs, lib, ... }:
        {
          home.packages = with pkgs.gnomeExtensions; [
            blur-my-shell
            just-perfection
            top-bar-organizer
            burn-my-windows
            caffeine
          ];

          dconf.settings = {
            "org/gnome/shell/keybindings" = {
              switch-to-application-1 = [];
              switch-to-application-2 = [];
              switch-to-application-3 = [];
              switch-to-application-4 = [];
              switch-to-application-5 = [];
              switch-to-application-6 = [];
              switch-to-application-7 = [];
              switch-to-application-8 = [];
              switch-to-application-9 = [];
            };

            "org/gnome/desktop/privacy" = {
              remove-old-temp-files = true;
              remove-old-trash-files = true;
              disable-camera = true;
              disable-microphone = true;
              recent-file-max-age = 30;
            };

            "org/gnome/desktop/wm/preferences" = {
              num-workspaces = 4;
              focus-mode = "click";
              resize-with-right-button = true;
            };

            "org/gnome/desktop/wm/keybindings" = {
              switch-to-workspace-1 = ["<Super>1" "<Super>KP_End"];
              switch-to-workspace-2 = ["<Super>2" "<Super>KP_Down"];
              switch-to-workspace-3 = ["<Super>3" "<Super>KP_Next"];
              switch-to-workspace-4 = ["<Super>4" "<Super>KP_Left"];
              switch-to-workspace-5 = ["<Super>5" "<Super>KP_Begin"];
              switch-to-workspace-6 = ["<Super>6" "<Super>KP_Right"];
              switch-to-workspace-7 = ["<Super>7" "<Super>KP_Home"];
              switch-to-workspace-8 = ["<Super>8" "<Super>KP_Up"];
              switch-to-workspace-9 = ["<Super>9" "<Super>KP_Page_Up"];
              switch-to-workspace-10 = ["<Super>0" "<Super>KP_Insert"];
              switch-to-workspace-last = ["<Super>End"];

              move-to-workspace-1 = ["<Super><Shift>1" "<Super><Shift>KP_End"];
              move-to-workspace-2 = ["<Super><Shift>2" "<Super><Shift>KP_Down"];
              move-to-workspace-3 = ["<Super><Shift>3" "<Super><Shift>KP_Next"];
              move-to-workspace-4 = ["<Super><Shift>4" "<Super><Shift>KP_Left"];
              move-to-workspace-5 = ["<Super><Shift>5" "<Super><Shift>KP_Begin"];
              move-to-workspace-6 = ["<Super><Shift>6" "<Super><Shift>KP_Right"];
              move-to-workspace-7 = ["<Super><Shift>7" "<Super><Shift>KP_Home"];
              move-to-workspace-8 = ["<Super><Shift>8" "<Super><Shift>KP_Up"];
              move-to-workspace-9 = ["<Super><Shift>9" "<Super><Shift>KP_Page_Up"];
              move-to-workspace-10 = ["<Super><Shift>0" "<Super><Shift>KP_Insert"];
              move-to-workspace-last = ["<Super><Shift>End"];
            };
          };
        };
    };
}
