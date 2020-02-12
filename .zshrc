autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git

setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_find_no_dups
setopt hist_reduce_blanks
setopt hist_save_no_dups

setopt prompt_subst

setopt no_beep

for file in .zsh/**/*.zsh; source $file
