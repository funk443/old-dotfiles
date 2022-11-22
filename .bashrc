#
# ~/.bashrc
#

figlet -kf slant "ad astra" | lolcat
figlet -kf slant "per aspera" | lolcat

export PATH="$HOME/.roswell/bin:/usr/local/texlive/2022/bin/x86_64-linux:/var/lib/flatpak/exports/bin:$HOME/.emacs.d/bin:$PATH"
export EDITOR="emacsclient -t -a 'emacs -nw'"
export VISUAL="emacsclient -c -a emacs"

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias rm='rm -i'
alias bgm='mpv --no-video"$1"'
alias ls='ls --color=auto -alsh'
alias bc='bc -l'
alias feh='feh --auto-zoom --scale-down'
alias ed='ed -p ":"'
alias lock='xlock -mode blank'
alias em='emacsclient -c -nw -a "emacs -nw"'
alias emacs='emacsclient -c -a emacs'
PS1='[\u@\h \W]\$ '

vterm_printf ()
{
  if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
    # Tell tmux to pass the escape sequences through
    printf "\ePtmux;\e\e]%s\007\e\\" "$1"
  elif [ "${TERM%%-*}" = "screen" ]; then
    # GNU screen (screen, screen-256color, screen-256color-bce)
    printf "\eP\e]%s\007\e\\" "$1"
  else
    printf "\e]%s\e\\" "$1"
  fi
}

vterm_prompt_end ()
{
  vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}

PS1=$PS1'\[$(vterm_prompt_end)\]'
