#
# ~/.bashrc
#

figlet -kf slant "ad astra" | lolcat
figlet -kf slant "per aspera" | lolcat

export PATH="/var/lib/flatpak/exports/bin:$HOME/.emacs.d/bin:$PATH"
export EDITOR="vim"

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias rm='rm -i'
alias bgm='mpv --no-video"$1"'
alias ls='ls --color=auto -alsh'
alias bc='bc -l'
alias feh='feh --auto-zoom --scale-down'
alias ed='ed -p ":"'
PS1='[\u@\h \W]\$ '

y ()
{
  mpv --ytdl-format="bestvideo[height<=$2]+bestaudio" "$1" & disown
}

s ()
{
  streamlink --stream-segment-threads 2 --twitch-low-latency --player mpv "$1" "$2" & disown
}

v ()
{
  mpv "$1" & disown
}

