(setq custom-file (locate-user-emacs-file "custom-var.el")
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.8)
      ;; initial-frame-alist '((width . 160)))

(load custom-file 'noerror 'nomessage)
(unless (file-directory-p "~/.emacs.d/autosave")
  (make-directory "~/.emacs.d/autosave"))
(unless (file-directory-p "~/.emacs.d/backup")
  (make-directory "~/.emacs.d/backup"))
(unless (file-directory-p "~/.emacs.d/undo_tree")
  (make-directory "~/.emacs.d/undo_tree"))

;; (add-to-list 'default-frame-alist '(font . "NovaMono-12"))
;; (add-to-list 'default-frame-alist '(font . "ShareTechMono-12"))
;; (add-to-list 'default-frame-alist '(font . "IBMPlexMono-12"))

(if (string-match " 29\.[[:digit:]]+" (emacs-version))
    (progn
      (set-frame-parameter (selected-frame) 'alpha-background 85)
      (add-to-list 'default-frame-alist '(alpha-background . 85)))
  (progn (set-frame-parameter (selected-frame) 'alpha '(90 . 75))
         (add-to-list 'default-frame-alist '(alpha . (90 . 75)))))

(defun display-startup-echo-area-message ()
  (message "ad astra per aspera"))
