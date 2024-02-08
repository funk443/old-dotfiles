(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backup")))
 '(blink-cursor-mode t)
 '(browse-url-browser-function 'browse-url-xdg-open)
 '(browse-url-secondary-browser-function 'eww-browse-url)
 '(column-number-mode t)
 '(connection-local-criteria-alist
   '(((:application eshell)
      eshell-connection-default-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
      (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(current-language-environment "UTF-8")
 '(custom-safe-themes
   '("ac18cc10455c6c26a98354ba8f9d338842d7ecc9ae3d28c205ed154ef20d74ce" "2050674326d536ddd3dcea87e077d27071cfbbe974a4540b1a57b6b672f64c51" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851" "8d5f22f7dfd3b2e4fc2f2da46ee71065a9474d0ac726b98f647bc3c7e39f2819" "ec2c86933a6e0b96f68f71d4b39ebdd67b43b0b32091b7689acb9acdc2a3e03b" default))
 '(default-input-method "chinese-array30")
 '(delete-selection-mode t)
 '(dired-dwim-target 'dired-dwim-target-next)
 '(dired-listing-switches "-alh --group-directories-first")
 '(display-line-numbers-current-absolute nil)
 '(display-line-numbers-type 'relative)
 '(display-line-numbers-width-start nil)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-default-load-average nil)
 '(erc-nick "CToID")
 '(eshell-banner-message
   " ______     _          _ _ \12|  ____|   | |        | | |\12| |__   ___| |__   ___| | |\12|  __| / __| '_ \\ / _ \\ | |\12| |____\\__ \\ | | |  __/ | |\12|______|___/_| |_|\\___|_|_|\12\12Welcome to Eshell, a shell written entirely in Emacs Lisp!\12-----\12")
 '(fill-column 80)
 '(frame-resize-pixelwise t)
 '(garbage-collection-messages nil)
 '(gnus-init-file "~/dotfiles/.emacs.d/.gnus")
 '(ibuffer-saved-filter-groups
   '(("customized"
      ("Dired"
       (mode . dired-mode))
      ("Magit"
       (name . "magit[-:].+"))
      ("Emacs"
       (name . "*[^*]+*")))))
 '(ido-default-buffer-method 'selected-window)
 '(ido-default-file-method 'selected-window)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(image-animate-loop t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(js-chain-indent t)
 '(line-number-display-limit-width most-positive-fixnum)
 '(line-spacing nil)
 '(major-mode-remap-alist '((perl-mode . cperl-mode)))
 '(menu-bar-mode nil)
 '(mode-line-compact t)
 '(mode-line-position-column-line-format '(" (%l %c)"))
 '(org-agenda-files '("~/Documents/agenda.org"))
 '(org-edit-src-content-indentation 0)
 '(org-export-backends '(ascii html latex md odt))
 '(org-export-with-sub-superscripts '{})
 '(org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
 '(org-image-actual-width 500)
 '(org-log-done 'note)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail org-tempo ol-w3m))
 '(org-src-preserve-indentation t)
 '(org-src-tab-acts-natively t)
 '(org-startup-truncated nil)
 '(org-todo-keywords '((sequence "TODO(t)" "DONE(d)")))
 '(org-use-sub-superscripts '{})
 '(package-install-upgrade-built-in t)
 '(package-selected-packages
   '(bind-key eglot eldoc faceup flymake jsonrpc org project soap-client verilog-mode transient racket-mode ligature-mode "geiser-gambit" plain-theme multiple-cursors cider eat magit markdown-mode dashboard unfill sly magit-section htmlize treesit-auto vterm-toggle ladger-mode screenshot
              #("eshell-prompt-extras" 0 20
                (escaped t))
              "eshell-prompt-extras" use-package arduino-mode xclip yaml-mode visual-fill-column-mode visual-line-mode highlight-indent-guides org-present-mode toc-org neotree which-key ##))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(require-final-newline t)
 '(safe-local-variable-values
   '((display-line-numbers . visual)
     (display-line-numbers-type . visual)))
 '(save-interprogram-paste-before-kill t)
 '(save-place-mode t)
 '(savehist-mode t)
 '(scalable-fonts-allowed t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 101)
 '(scroll-error-top-bottom t)
 '(scroll-preserve-screen-position 1)
 '(sh-basic-offset 2)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(warning-suppress-types '((comp) (comp)))
 '(x-select-enable-clipboard-manager nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
