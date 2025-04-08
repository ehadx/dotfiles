{ pkgs, config, ... }:
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
