{
  description = "My zsh configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }:
    {
      system-conf = { config, pkgs, ... }:
        {
          environment.pathsToLink = [ "/share/zsh" ];
        };
      home-conf = { config, pkgs, ... }:
        {
          programs.starship = {
            enable = true;
            enableZshIntegration = true;
          };

          programs.zsh = {
            enable = true;
            enableCompletion = true;
            syntaxHighlighting.enable = true;
            autosuggestion.enable = true;
            history = {
              ignoreAllDups = true;
              ignoreDups = true;
              ignoreSpace = true;
              expireDuplicatesFirst = true;
            };
            initExtra = "
              setopt hist_find_no_dups
              setopt hist_save_no_dups
              setopt prompt_subst
              setopt no_beep
            ";
          };
        };
    };
}
