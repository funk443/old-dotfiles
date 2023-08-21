export XIM="ibus"
export XMODIFIERS="@im=ibus"
export XIM_PROGRAM="ibus"
export GTK_IM_MODULE="ibus"
export QT_IM_MODULE="ibus"
export SDL_IM_MODULE="ibus"
export XDG_SESSION_TYPE="x11"
export GDK_BACKEND="x11"
# [[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
# exec dbus-run-session qtile start
exec dbus-run-session stumpwm
