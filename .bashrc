# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

alias xbpsi='sudo xbps-install -S'
alias xbpsiy='sudo xbps-install -Sy'
alias xbpsu='sudo xbps-install -S xbps && sudo xbps-install -u'
alias xbpsry='sudo xbps-remove -Ry'
alias xbpsr='sudo xbps-remove -Rf'
alias xbpsrn='sudo xbps-remove -Rfny'
alias xbpsro='sudo xbps-remove -fOo'
alias xbpsq='xbps-query -Rs'
alias xbpslf='xbps-query -l | grep'
alias xbpsm='xbps-query -m'
alias xbpsmf='xbps-query -m | grep'
alias reboot='sudo loginctl reboot'
alias poweroff='sudo loginctl poweroff'
alias hibernate='sudo loginctl hibernate'
alias suspend='sudo loginctl suspend'
alias ls="exa"

export DOTNET_ROOT='~/.dotnet'
export PATH="$PATH:$DOTNET_ROOT"
export HISTCONTROL='ignoreboth:erasedups'

export PS1="[\D{%A %d/%m} \T in \W]\$ "

neofetch
