export LC_CTYPE="zh_TW.UTF-8"
export XIM="ibus"
export XMODIFIERS="@im=ibus"
export XIM_PROGRAM="ibus"
export GTK_IM_MODULE="ibus"
export QT_IM_MODULE="ibus"
export SDL_IM_MODULE="ibus"
numlockx &
ibus-daemon -drxR
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
exec dbus-run-session i3
# exec startxfce4
# exec dbus-launch --exit-with-session emacs -mm --debug-init
