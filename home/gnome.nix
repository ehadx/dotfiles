{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs.gnomeExtensions; [
    blur-my-shell
    just-perfection
    top-bar-organizer
    burn-my-windows
    caffeine
    paperwm
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
      num-workspaces = 10;
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
}
