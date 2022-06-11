(defvar temp-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
