(IN-PACKAGE :STUMPWM)

(SET-PREFIX-KEY (KBD "s-t"))

(SETF *SCREEN-MODE-LINE-FORMAT* (LIST "%g | ^>%d")
      *TIME-MODELINE-STRING* "%F %a %T"
      *MODE-LINE-TIMEOUT* 2
      *WINDOW-FORMAT* "<[%s]%t>"
      *MOUSE-FOCUS-POLICY* :CLICK
      *GROUP-FORMAT* "<[%n%s]%t>")

(GRENAME "Firefox")
(GNEWBG "Discord")
(GNEWBG "Emacs")

(DEFINE-FRAME-PREFERENCE "Discord"
    (0 T T :CLASS "discord"))

(ADD-HOOK *QUIT-HOOK* #'(LAMBDA ()
                          (RUN-SHELL-COMMAND "emacsclient -e '(save-buffers-kill-emacs)'")))

(RUN-SHELL-COMMAND "sh ~/dotfiles/misc/qtile_startup_once.sh")
(LET ((COMMANDS-TO-RUN (LIST "feh --bg-fill ~/dotfiles/wallpapers/void_linux_chan.png")))
  (DOLIST (COMMAND COMMANDS-TO-RUN)
    (RUN-SHELL-COMMAND COMMAND)))

(LET ((KEYS-TO-DEFINE (LIST (CONS (KBD "c") "exec alacritty")
                            (CONS (KBD "e") "exec emacsclient -c -a ''")
                            (CONS (KBD "s") "hsplit")
                            (CONS (KBD "S") "vsplit")
                            (CONS (KBD "m") "mode-line")
                            (CONS (KBD "C-k") "remove-split"))))
  (DOLIST (KEY KEYS-TO-DEFINE)
    (DEFINE-KEY *ROOT-MAP* (CAR KEY) (CDR KEY))))
