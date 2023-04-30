(ql:quickload '("cl-ppcre" "slynk"))
(in-package stumpwm)
(load-module "stumptray")
(load-module "swm-gaps")

(defvar *slynk-port* (slynk:create-server :port 0 :dont-close t))

(set-prefix-key (kbd "s-t"))
(setf *screen-mode-line-format* (list "%g | %d")
      *time-modeline-string* "%a. %d %b %y %T"
      *mode-line-timeout* 2
      *window-format* "<[%n%s%m]%t>"
      *window-info-format* "%wx%h %n %m (%t)"
      *mouse-focus-policy* :click
      *group-format* "<[%n%s]%t>"
      *message-window-padding* 3
      *message-window-y-padding* 3
      *maxsize-border-width* 2
      *transient-border-width* 2
      *normal-border-width* 2
      swm-gaps:*head-gaps-size* 0
      swm-gaps:*inner-gaps-size* 5
      swm-gaps:*outer-gaps-size* 5)

(let ((unfocus-color "#4e4e4e")
      (focus-color "#478061"))
  (set-focus-color focus-color)
  (set-float-focus-color focus-color)
  (set-unfocus-color unfocus-color)
  (set-float-unfocus-color unfocus-color))

(swm-gaps:toggle-gaps-on)
(set-font "-ibm-courier-bold-r-*-*-17-120-100-100-*-*-*-*")

(xlib:set-wm-class (screen-message-window (current-screen))
                   "stumpwm-message" "stumpwm-message")

(grename "firefox")
(gnewbg "discord")

(define-frame-preference "discord"
  (0 t t :class "discord"))

(define-frame-preference "steam"
  (0 t t :class "Steam"))

(add-hook *quit-hook* (lambda ()
                        (run-shell-command
                         "emacsclient -e '(save-buffers-kill-emacs)'")))
(add-hook *focus-group-hook* (lambda (new-group old-group)
                               (message "~a" new-group)))
(add-hook *new-mode-line-hook* (lambda (mode-line)
                                 (xlib:set-wm-class (mode-line-window mode-line)
                                                    "stumpwm-modeline"
                                                    "stumpwm-modeline")))

(defcommand current-cpu-usage () ()
  (message "~a" (run-shell-command "mpstat" t)))

(defcommand my-echo-date () ()
  (message "~a" (run-shell-command "date '+%a.  %d %b %y  %T'" t)))

(defcommand ss () ()
  (run-shell-command "xfce4-screenshooter"))

(defcommand toggle-systray () ()
  (stumptray::stumptray))

(let ((commands-to-run (list "sh ~/dotfiles/misc/qtile_startup_once.sh"
                             "feh --bg-fill ~/dotfiles/wallpapers/void_linux_chan.png"
                             "xsetroot -cursor_name left_ptr")))
  (dolist (command commands-to-run)
    (run-shell-command command)))

(let ((keys-to-define (list (cons (kbd "c") "exec alacritty")
                            (cons (kbd "e") "exec emacsclient -c -a ''")
                            (cons (kbd "s") "hsplit")
                            (cons (kbd "S") "vsplit")
                            (cons (kbd "m") "mode-line")
                            (cons (kbd "a") "my-echo-date"))))
  (dolist (key keys-to-define)
    (define-key *root-map* (car key) (cdr key))))
