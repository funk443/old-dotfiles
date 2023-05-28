(setq custom-file (locate-user-emacs-file "custom-var.el")
      gc-cons-threshold (* 64 1024 1024)
      gc-cons-percentage 0.75)

(load custom-file 'noerror 'nomessage)
(unless (file-directory-p "~/.emacs.d/autosave")
  (make-directory "~/.emacs.d/autosave"))
(unless (file-directory-p "~/.emacs.d/backup")
  (make-directory "~/.emacs.d/backup"))
(unless (file-directory-p "~/.emacs.d/undo_tree")
  (make-directory "~/.emacs.d/undo_tree"))

(set-frame-parameter (selected-frame) 'alpha-background 85)
(add-to-list 'default-frame-alist '(alpha-background . 85))

(defun display-startup-echo-area-message ()
  (message "ad astra per aspera"))
