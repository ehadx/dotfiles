{ config, pkgs, ... }:

{
  system.defaults.finder.AppleShowAllExtensions = true;
  system.defaults.finder._FXShowPosixPathInTitle = true;

  system.defaults.NSGlobalDomain.AppleShowAllExtensions = true;
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 14;
  system.defaults.NSGlobalDomain.KeyRepeat = 1;

  homebrew = {
    enable = true;
    casks = [
      "floorp"  # x86_64-darwin is not supported on nix
      "ghostty" # broken on nix
    ];
  };
}
