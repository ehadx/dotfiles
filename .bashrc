# If not running interactively, don't do anything
[[ $- != *i* ]] && return

check_command ()
{
  if ! command -V $1 1>/dev/null 2>&1; then
    echo 1
  else
    echo 0
  fi
}

PS1='[\D{%A %d/%m} \T in \W]\$ '
HISTCONTROL='ignoreboth:erasedups'

if [ $(check_command bat) -eq 0 ]; then
  alias cat='bat'
fi
if [ $(check_command exa) -eq 0 ]; then
  alias ls='exa'
elif [ $(check_command eza) -eq 0 ]; then
  alias ls='eza'
fi

if [ $(check_command xbps-install) -eq 0 ]; then
  echo 'Its void'
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
elif [ $(check_command zypper) -eq 0 ]; then
  echo 'Its OpenSuse'
else
  echo 'Unknown Distro'
fi

if [ $(check_command neofetch) -eq 0 ]; then
  neofetch
elif [ $(check_command fastfetch) -eq 0 ]; then
  fastfetch
fi
