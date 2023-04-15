(QL:QUICKLOAD :CL-PPCRE)
(IN-PACKAGE :STUMPWM)

(SET-PREFIX-KEY (KBD "s-t"))
(SETF *SCREEN-MODE-LINE-FORMAT* (LIST "%g | ^>%d")
      *TIME-MODELINE-STRING* "%F %a %T"
      *MODE-LINE-TIMEOUT* 2
      *WINDOW-FORMAT* "<[%n%s]%t>"
      *MOUSE-FOCUS-POLICY* :CLICK
      *GROUP-FORMAT* "<[%n%s]%t>"
      *MESSAGE-WINDOW-PADDING* 3
      *MESSAGE-WINDOW-Y-PADDING* 3)
(SET-FONT "-ibm-courier-bold-r-*-*-17-120-100-100-*-*-*-*")

(GRENAME "firefox")
(GNEWBG "discord")

(DEFINE-FRAME-PREFERENCE "discord"
    (0 T T :CLASS "discord"))

(DEFINE-FRAME-PREFERENCE "steam"
    (0 T T :CLASS "Steam"))

(ADD-HOOK *QUIT-HOOK* (LAMBDA ()
                        (RUN-SHELL-COMMAND
                         "emacsclient -e '(save-buffers-kill-emacs)'")))

(ADD-HOOK *FOCUS-GROUP-HOOK* (LAMBDA (NEW-GROUP OLD-GROUP)
                               (MESSAGE "~A" NEW-GROUP)))

(DEFCOMMAND CURRENT-CPU-USAGE () ()
  (MESSAGE "~A" (RUN-SHELL-COMMAND
                 "top -bn1 | grep 'Cpu(s)' | awk '{print $2 + $4}'" T)))

(DEFCOMMAND MY-ECHO-DATE () ()
  (MESSAGE "~A" (RUN-SHELL-COMMAND "date '+%a.  %d %B %Y  %T'" T)))

(DEFCOMMAND CALENDER (ARGS)
  ((:STRING "ARGS: "))
  (IF (STRING= "" ARGS)
      (LET* ((TODAY (PARSE-INTEGER (RUN-SHELL-COMMAND "date '+%d'" T)))
             (CAL-OUTPUT (RUN-SHELL-COMMAND "cal --sun" T)))
        (MULTIPLE-VALUE-BIND (START END)
            (PPCRE:SCAN (FORMAT NIL "\\s~A\\s" TODAY) CAL-OUTPUT)
          (MESSAGE "~A" (FORMAT NIL "~A^R~A^r~A"
                                (SUBSEQ CAL-OUTPUT 0 (1+ START))
                                (SUBSEQ CAL-OUTPUT (1+ START) (1- END))
                                (SUBSEQ CAL-OUTPUT (1- END))))))
      (MESSAGE "~A" (RUN-SHELL-COMMAND (FORMAT NIL "~A ~A --sun" "cal" ARGS) T))))

(LET ((COMMANDS-TO-RUN (LIST "sh ~/dotfiles/misc/qtile_startup_once.sh"
                             "feh --bg-fill ~/dotfiles/wallpapers/void_linux_chan.png"
                             "xsetroot -cursor_name left_ptr")))
  (DOLIST (COMMAND COMMANDS-TO-RUN)
    (RUN-SHELL-COMMAND COMMAND)))

(LET ((KEYS-TO-DEFINE (LIST (CONS (KBD "c") "exec alacritty")
                            (CONS (KBD "e") "exec emacsclient -c -a ''")
                            (CONS (KBD "s") "hsplit")
                            (CONS (KBD "S") "vsplit")
                            (CONS (KBD "m") "mode-line")
                            (CONS (KBD "C-k") "remove-split")
                            (CONS (KBD "O") "other-window")
                            (CONS (KBD "a") "my-echo-date")
                            (CONS (KBD "d") (LET ((M (MAKE-SPARSE-KEYMAP)))
                                              (DEFINE-KEY M (KBD "d")
                                                "calender \"\"")
                                              (DEFINE-KEY M (KBD "f")
                                                "calender")
                                              M)))))
  (DOLIST (KEY KEYS-TO-DEFINE)
    (DEFINE-KEY *ROOT-MAP* (CAR KEY) (CDR KEY))))
