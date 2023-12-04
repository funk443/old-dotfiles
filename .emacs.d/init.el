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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package plain-theme
  :ensure t
  :config
  (load-theme 'plain t))

(use-package dashboard
  :ensure t
  :custom
  (dashboard-banner-logo-title "GNU EMACS - Editor MACroS")
  (dashboard-startup-banner "~/.emacs.d/emacs.png")
  (dashboard-center-content t)
  (dashboard-week-agenda nil)
  (dashboard-agenda-time-string-format "%m %d %y %H:%M")
  (dashboard-agenda-prefix-format "%?-12t% s")
  (dashboard-agenda-release-buffers t)
  (dashboard-items nil)
  (dashboard-set-init-info nil)
  :config
  (add-to-list 'dashboard-footer-messages "Free as in Freedom!")
  (add-to-list 'dashboard-footer-messages
               "There is no system but GNU, and Linux is one of its kernels."))
(if (and (< (length command-line-args) 4)
         (member "server-start" command-line-args))
    (let ((command-line-args '("emacs")))
      (dashboard-setup-startup-hook))
  (dashboard-setup-startup-hook))

;; (use-package general
;;   :ensure t
;;   :config (general-eval-setup t))

;; (use-package evil
;;   :ensure t
;;   :init
;;   (setq evil-want-C-u-scroll t
;;         evil-want-integration t
;;         evil-want-keybinding nil
;;         evil-want-minibuffer t)
;;   :config (evil-mode 1))

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config (evil-collection-init))

;; (use-package undo-tree
;;   :after evil
;;   :ensure t
;;   :init
;;   (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo_tree")))
;;   :config
;;   (evil-set-undo-system 'undo-tree)
;;   (global-undo-tree-mode 1))

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

(use-package racket-mode
  :ensure t)

(use-package sly
  :ensure t
  :init (setq inferior-lisp-program "sbcl")
  :config
  (setq common-lisp-hyperspec-root
        (concat "file:///"
                (expand-file-name (concat user-emacs-directory "HyperSpec/"))))
  :custom
  (sly-common-lisp-style-default "modern"))

;; (use-package slime
;;   :ensure t
;;   :init (setq inferior-lisp-program "sbcl"))

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

(use-package emms
  :ensure t
  :custom
  (emms-player-list '(emms-player-mpv))
  (emms-info-functions '(emms-info-native))
  (emms-volume-change-function #'emms-volume-mpv-change)
  :config
  (emms-all)
  :bind
  (("C-c m p" . emms-pause)
   ("C-c m +" . emms-volume-mode-plus)
   ("C-c m -" . emms-volume-mode-minus)))

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

(use-package paren-face
  :ensure t
  :custom
  (paren-face-modes '(lisp-mode
                      emacs-lisp-mode
                      lisp-interaction-mode
                      ielm-mode
                      scheme-mode
                      inferior-scheme-mode
                      clojure-mode
                      cider-repl-mode
                      nrepl-mode
                      sly-mrepl-mode
                      arc-mode
                      inferior-arc-mode
                      racket-mode
                      racket-repl-mode))
  :custom-face
  (parenthesis ((nil (:foreground "ghost white"))))
  :config
  (global-paren-face-mode 1))

(defun startup-function ()
  (require 'org)
  (require 'ibuf-ext)
  (setq frame-title-format "%b - GNU Emacs"
        default-input-method "chinese-array30"
        completion-ignore-case t
        initial-buffer-choice (lambda () (dashboard-open)))
  (setq-default python-indent-offset 2
                rust-indent-offset 2
                css-indent-offset 2
                js-indent-level 2
                standard-indent 2
                perl-indent-level 2)
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "customized")))
  (savehist-mode 1)
  (set-fonts)
  (set-keys)
  (add-hook 'org-mode-hook #'variable-pitch-mode)
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'org-capture-mode-hook #'auto-fill-mode)
  (add-hook 'org-mode-hook #'electric-quote-local-mode)
  (add-hook 'org-capture-mode-hook #'electric-quote-local-mode)
  (package-initialize))

(defun set-fonts ()
  (let ((font-height 140))
    (set-face-attribute 'default nil
                        :family "IBMPlexMono"
                        :height font-height)
    (set-face-attribute 'variable-pitch nil
                        :family "Noto Serif CJK TC"
                        :inherit 'default)
    (set-face-attribute 'fixed-pitch nil
                        :family "IBMPlexMono"
                        :inherit 'default)
    (set-face-attribute 'org-default nil
                        :inherit 'variable-pitch)
    (set-face-attribute 'org-block nil
                        :inherit 'fixed-pitch)
    (set-face-attribute 'org-verbatim nil
                        :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil
                        :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil
                        :inherit 'fixed-pitch)
    (set-face-attribute 'org-drawer nil
                        :inherit 'fixed-pitch)
    (set-face-attribute 'org-special-keyword nil
                        :inherit 'fixed-pitch)
    (set-face-attribute 'org-property-value nil
                        :inherit 'fixed-pitch)))

(defun set-keys ()
  (dolist (keybinding '(("<C-wheel-up>" . text-scale-increase)
                        ("<C-wheel-down>" . text-scale-decrease)
                        ("C-x C-r" . recentf-open-files)
                        ("C-M-=" . count-words)
                        ("C-x C-b" . ibuffer)))
    (global-set-key (kbd (car keybinding)) (cdr keybinding))))

(defvar gc-idle-timer
  (run-with-idle-timer 8 t #'garbage-collect)
  "The idle timer to run garbage collection")

(add-hook 'emacs-startup-hook #'startup-function)
