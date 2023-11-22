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

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :custom-face
  (font-lock-comment-face ((nil (:foreground "#8f8eb1"))))
  :config
  (load-theme 'doom-one-light t))

(use-package all-the-icons
  :ensure t
  :config
  (unless all-the-icons-font-names
    (all-the-icons-install-fonts)))

(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :ensure t
  :after all-the-icons
  :hook (ibuffer-mode  . all-the-icons-ibuffer-mode))

;; (use-package doom-modeline
;;   :ensure t
;;   :init
;;   (doom-modeline-mode 1))

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
  (dashboard-icon-type 'all-the-icons)
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

(use-package org-present
  :commands (org-present)
  :ensure t
  :config
  (add-hook 'org-present-mode-hook
            (lambda ()
              (setq org-image-actual-width 1000
                    header-line-format " ")
              (setq-local face-remapping-alist
                          '((default (:height 2.0) default)
                            (header-line (:height 5.0) header-line)
                            (org-document-title (:height 2.25)
                                                org-document-title)))
              (variable-pitch-mode)
              (org-display-inline-images)
              (org-present-read-only)
              (visual-fill-column-mode 1)
              (org-present-hide-cursor)
              (visual-line-mode 1)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (setq org-image-actual-width 300
                    header-line-format nil)
              (setq-local face-remapping-alist
                          '((default variable-pitch default)))
              (org-remove-inline-images)
              (org-present-read-write)
              (visual-fill-column-mode 0)
              (org-present-show-cursor)
              (visual-line-mode 0))))

(use-package visual-fill-column
  :after org-present
  :ensure t
  :init
  (setq visual-fill-column-width 255
        visual-fill-column-center-text t))

(unless (file-directory-p "~/Documents/org-roam/")
  (make-directory "~/Documents/org-roam/"))
(unless (file-directory-p "~/Documents/org-roam/fleeting/")
  (make-directory "~/Documents/org-roam/fleeting/"))
(unless (file-directory-p "~/Documents/org-roam/literature/")
  (make-directory "~/Documents/org-roam/literature/"))
(use-package org-roam
  :ensure t
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   :map org-roam-dailies-map
   ("Y" . org-roam-dailies-capture-yesterday)
   ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode 1)
  :custom
  (org-roam-directory "~/Documents/org-roam/")
  (org-roam-dailies-directory "fleeting/"))

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

(use-package sudo-edit
  :commands (sudo-edit)
  :ensure t
  :config
  (sudo-edit-indicator-mode 1))

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

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (vertico-resize 'grow-only)
  :init (vertico-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :init (marginalia-mode))

(use-package vertico-directory
  :after vertico
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(partial-completion orderless basic))
  (completion-category-defaults nil))

(use-package consult
  :ensure t
  :config
  (setq completion-in-region-function
        (if vertico-mode
            #'consult-completion-in-region
          #'completion--in-region)))

(use-package unfill
  :ensure t)

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
  ;; (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "customized")))
  (add-hook 'image-mode-new-window-functions
            (lambda (arg)
              (display-line-numbers-mode -1)))
  
  (add-hook 'xwidget-webkit-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)))
  (savehist-mode 1)
  (set-fonts)
  (set-keys)
  (set-fonts-frame)
  (add-hook 'org-mode-hook #'variable-pitch-mode)
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'org-capture-mode-hook #'auto-fill-mode)
  (add-hook 'org-mode-hook #'electric-quote-local-mode)
  (add-hook 'org-capture-mode-hook #'electric-quote-local-mode)
  (package-initialize))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha-background)))
    (set-frame-parameter nil 'alpha-background
                         (if (= alpha 100) 85 100))))

(defun set-fonts ()
  (let ((font-height 140))
    (setq face-font-rescale-alist '(("Noto Sans Mono CJK TC" . 0.92)
                                    ("Noto Sans Mono CJK SC" . 0.92)
                                    ("Noto Sans Mono CJK JP" . 0.92)
                                    ("Noto Sans Mono" . 0.92)))
    (set-face-attribute 'default nil
                        :family "IBMPlexMono"
                        :height font-height)
    (set-face-attribute 'variable-pitch nil
                        :family "IBMPlexSerif"
                        :inherit 'default)
    (set-face-attribute 'fixed-pitch nil
                        :family "IBMPlexMono"
                        :inherit 'default)
    (set-face-attribute 'dashboard-items-face nil
                        :height font-height)
    (set-face-attribute 'mode-line nil
                        :inherit 'fixed-pitch)
    (set-face-attribute 'line-number nil
                        :inherit 'mode-line)
    (set-face-attribute 'line-number-current-line nil
                        :inherit 'line-number)
    (set-face-attribute 'fringe nil
                        :inherit 'line-number)
    (set-face-attribute 'header-line nil
                        :inherit 'mode-line)
    (set-face-attribute 'line-number-current-line nil
                        :inherit 'fixed-pitch)
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
                        :inherit 'fixed-pitch)
    (set-face-attribute 'org-block-begin-line nil
                        :foreground "#8f8eb1")
    (when (member (face-attribute 'default :family)
                  '("IBM Plex Mono" "IBMPlexMono"))
      (set-face-attribute 'line-number nil
                          :slant 'normal
                          :weight 'normal
                          :inherit 'mode-line)
      (set-face-attribute 'line-number-current-line nil
                          :slant 'normal
                          :weight 'bold
                          :inherit 'line-number)
      (set-face-attribute 'dashboard-banner-logo-title nil
                          :weight 'bold)
      (set-face-attribute 'dashboard-footer-face nil
                          :slant 'italic)
      (set-face-attribute 'font-lock-comment-face nil
                          :slant 'italic))))

(defun set-fonts-frame ()
  (let ((font-height 140))
    (set-fontset-font "fontset-default" 'han (font-spec :family "GenYoMin TW"))
    (set-fontset-font "fontset-default" 'han (font-spec :family "HanaMinA")
                      nil 'append)
    (set-fontset-font "fontset-default" 'han (font-spec :family "HanaMinB")
                      nil 'append)
    (set-fontset-font "fontset-default" 'han
                      (font-spec :family "Noto Sans Mono CJK TC") nil 'append)
    (set-fontset-font "fontset-default" 'han
                      (font-spec :family "Noto Sans Mono CJK SC") nil 'append)
    (set-fontset-font "fontset-default" 'big5 (font-spec :family "GenYoMin TW"))
    (set-fontset-font "fontset-default" 'big5 (font-spec :family "HanaMinA")
                      nil 'append)
    (set-fontset-font "fontset-default" 'big5 (font-spec :family "HanaMinB")
                      nil 'append)
    (set-fontset-font "fontset-default" 'big5
                      (font-spec :family "Noto Sans Mono CJK TC") nil 'append)
    (set-fontset-font "fontset-default" 'big5
                      (font-spec :family "Noto Sans Mono CJK SC") nil 'append)
    (set-fontset-font "fontset-default" 'kana
                      (font-spec :family "Noto Sans Mono CJK JP"))
    (set-fontset-font "fontset-default" 'symbol (font-spec :family "IBMPlexMono"))
    (set-fontset-font "fontset-default" 'symbol (font-spec :family "Noto Sans Mono")
                      nil 'append)))

(defun set-keys ()
  (dolist (keybinding '(("<C-wheel-up>" . text-scale-increase)
                        ("<C-wheel-down>" . text-scale-decrease)
                        ("C-x C-r" . recentf-open-files)
                        ("C-M-=" . count-words)
                        ("C-x C-b" . ibuffer)))
    (global-set-key (kbd (car keybinding)) (cdr keybinding))))

(defvar emms-volume--mpv)

(defun emms-volume--mpv-get-volume ()
  (emms-player-mpv-cmd '(get_property volume)
                       (lambda (vol err)
                         (setq emms-volume--mpv (truncate vol)))))

(defun emms-volume-mpv-change (amount)
  (unless (boundp 'emms-volume--mpv)
    (emms-volume--mpv-get-volume))
  (let* ((cur-vol emms-volume--mpv)
         (new-vol (+ amount cur-vol)))
    (cond ((> new-vol 100)
           (setq new-vol 100))
          ((< new-vol 0)
           (setq new-vol 0))
          (t nil))
    (setq emms-volume--mpv new-vol)
    (emms-player-mpv-cmd `(set_property volume ,new-vol))
    (message (format "Volume: %s" new-vol))))

(defun xwidget-webkit-with-external-browser ()
  (interactive nil xwidget-webkit-mode)
  (funcall browse-url-secondary-browser-function
           (xwidget-webkit-uri (xwidget-webkit-current-session))))

(add-hook 'xwidget-webkit-mode-hook
          (lambda ()
            (keymap-set xwidget-webkit-mode-map "&"
                        'xwidget-webkit-with-external-browser)))

(defvar gc-idle-timer
  (run-with-idle-timer 8 t #'garbage-collect)
  "The idle timer to run garbage collection")

(add-hook 'emacs-startup-hook #'startup-function)

(add-hook 'server-after-make-frame-hook
          #'set-fonts-frame)
