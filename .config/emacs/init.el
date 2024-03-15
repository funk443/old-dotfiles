;;;; Copyright (C) 2023  CToID

;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.


(require 'package)


(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/"))


(use-package toc-org
  :commands (toc-org-enable)
  :ensure t
  :hook (org-mode . toc-org-enable))

(use-package htmlize
  :ensure t)


(use-package arduino-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package racket-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package sly
  :ensure t
  :init (setq inferior-lisp-program "sbcl")
  :config
  (setq common-lisp-hyperspec-root
        (concat "file:///"
                (expand-file-name (concat user-emacs-directory
                                          "HyperSpec/"))))
  :custom
  (sly-common-lisp-style-default "modern"))

(use-package cider
  :ensure t
  :custom
  (cider-font-lock-dynamically nil)
  (cider-allow-jack-in-without-project t))


(use-package magit
  :ensure t
  :commands (magit magit-clone))

(use-package xclip
  :ensure t)

(use-package eat
  :ensure t
  :config
  (eat-eshell-mode)
  (eat-eshell-visual-command-mode)
  :custom
  (eshell-visual-commands nil))

(use-package which-key
  :ensure t
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 0)
  (which-key-max-display-columns nil)
  (which-key-side-window-max-height 0.25)
  (which-key-idle-delay 0.5)
  (which-key-max-description-length 25)
  :config
  (which-key-mode))

(use-package recentf
  :custom
  (recentf-max-menu-items 10)
  (recentf-max-saved-itmes 10)
  (add-to-list 'recentf-exclude "\\.last\\'")
  :config
  (recentf-mode 1))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(use-package unfill
  :ensure t)

(use-package multiple-cursors
  :ensure t)


(defun startup-function ()
  (require 'org)
  (setq completion-ignore-case t)
  (set-fonts)
  (set-keys)
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (package-initialize))

(defun set-fonts ()
  (let ((font-height 140))
    (set-face-attribute 'default nil
                        :family "IBM Plex Mono"
                        :height font-height)
    (set-face-attribute 'variable-pitch nil
                        :inherit 'default
                        :family "Noto Sans CJK TC")
    (set-face-attribute 'fixed-pitch nil
                        :inherit 'default
                        :family "IBM Plex Mono")))

(defun set-keys ()
  (dolist (keybinding '(("<C-wheel-up>" . text-scale-increase)
                        ("<C-wheel-down>" . text-scale-decrease)
                        ("C-x C-r" . recentf-open-files)
                        ("C-M-=" . count-words)))
    (global-set-key (kbd (car keybinding)) (cdr keybinding))))

(add-hook 'emacs-startup-hook #'startup-function)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
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
 '(global-font-lock-mode nil)
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
 '(js-indent-level 2)
 '(line-number-display-limit-width most-positive-fixnum)
 '(line-spacing nil)
 '(major-mode-remap-alist '((perl-mode . cperl-mode)))
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(mode-line-compact t)
 '(mode-line-position-column-line-format '(" L%l C%c"))
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
   '(go-mode kotlin-ts-mode sly bind-key eglot eldoc faceup flymake jsonrpc org project soap-client verilog-mode transient racket-mode ligature-mode "geiser-gambit" plain-theme multiple-cursors cider eat magit markdown-mode unfill magit-section htmlize treesit-auto vterm-toggle ladger-mode screenshot
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
