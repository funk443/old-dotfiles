#
# ~/.bashrc
#

export PATH="/usr/local/texlive/2022/bin/x86_64-linux:/var/lib/flatpak/exports/bin:$HOME/.emacs.d/bin:$PATH"
export LD_LIBRARY_PATH="/usr/local/lib"
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
alias em='emacsclient -c -a emacs'
alias perepl='perl -de1'
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

random_startup_message ()
{
  if [ $(($RANDOM % 2)) = 1 ]
  then
    figlet -kf slant "ad astra" | lolcat
    figlet -kf slant "per aspera" | lolcat
  else
    cowsay -f $(shuf -e -n 1 gnu tux) $(shuf -e -n 1 \
                                             "Void is not based on Arch or Debian.  It's just based." \
                                             "There is no system but GNU, and Linux is one of it's kernels.") | lolcat
  fi
}

random_startup_message

PATH="/home/id/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/id/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/id/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/id/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/id/perl5"; export PERL_MM_OPT;
export PATH=$PATH:~/.roswell/bin
export PATH=$PATH:/usr/local/racket/bin
export PATH=$PATH:~/Documents/appimages
export SBCL_HOME=/usr/local/lib/sbcl
eval $(opam env --switch=4.07.0)
