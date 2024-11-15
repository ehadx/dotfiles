{ config, pkgs, ...}:

{
  services.mpd = {
    enable = true;
    musicDirectory = "/home/hadi/Music";
    user = "hadi";
    extraConfig = ''
      audio_output {
        type "pipewire"
        name "MPD PipeWire Output"
      }
    '';
  };
  systemd.services.mpd.wantedBy = [];
  systemd.services.mpd.environment = {
    # https://gitlab.freedesktop.org/pipewire/pipewire/-/issues/609
    #XDG_RUNTIME_DIR = "/run/user/${toString config.users.users.hadi.uid}"; # User-id must match above user. MPD will look inside this directory for the PipeWire socket.
    XDG_RUNTIME_DIR = "/run/user/1000";
  };

  # required for playing music with pipewire
  services.pipewire.alsa.enable = true;
}
