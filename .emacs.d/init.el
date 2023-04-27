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
  :init
  (setq doom-themes-enable-bold t
        doom-challenger-deep-brighter-modeline t
        doom-themes-enable-italic t)
  :config
  (load-theme 'doom-challenger-deep t))

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
  :init
  (setq dashboard-banner-logo-title "GNU EMACS - Editor MACroS"
        dashboard-startup-banner "~/.emacs.d/emacs.png"
        dashboard-center-content t
        dashboard-week-agenda nil
        dashboard-agenda-time-string-format "%m %d %y %H:%M"
        dashboard-agenda-prefix-format "%?-12t% s"
        dashboard-agenda-release-buffers t
        dashboard-items nil
        dashboard-set-init-info nil)
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
                                                org-document-title)
                            (org-code (:height 1.0) org-code)
                            (org-verbatim (:height 1.0) org-verbatim)
                            (org-block (:height 1.0) org-block)
                            (org-block-begin-line (:height 0.75)
                                                  org-block-begin-line)))
              (org-display-inline-images)
              (org-present-read-only)
              (visual-fill-column-mode 1)
              (org-present-hide-cursor)
              (visual-line-mode 1)))
  (add-hook 'org-present-quit-hook
            (lambda ()
              (setq org-image-actual-width 300
                    header-line-format nil)
              (setq-local face-remapping-alist nil)
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
  :init (setq inferior-lisp-program "sbcl"))

;; (use-package slime
;;   :ensure t
;;   :init (setq inferior-lisp-program "sbcl"))

(use-package magit
  :ensure t
  :commands (magit magit-clone))

(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-depropertize t
        browse-kill-ring-display-duplicates t))

(use-package xclip
  :ensure t)

(use-package vterm
  :commands (vterm)
  :ensure t)

(use-package which-key
  :ensure t
  :init
  (setq which-key-side-window-location 'bottom
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 0
        which-key-max-display-columns nil
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.5
        which-key-max-description-length 25)
  :config
  (which-key-mode))

(use-package recentf
  :init
  (setq recentf-max-menu-items 10
        recentf-max-saved-items 10)
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
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-mpv)
        emms-info-functions '(emms-info-native)
        emms-volume-change-function #'emms-volume-mpv-change)
  :bind
  (("C-c m p" . emms-pause)
   ("C-c m +" . emms-volume-mode-plus)
   ("C-c m -" . emms-volume-mode-minus)))

(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(defun startup-function ()
  (require 'org)
  (require 'ibuf-ext)
  (setq frame-title-format "%b - GNU Emacs"
        gc-cons-threshold (* 32 1024 1024)
        gc-cons-percentage 0.25
        default-input-method "chinese-array30"
        initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq-default python-indent-offset 2
                rust-indent-offset 2
                css-indent-offset 2
                js-indent-level 2
                standard-indent 2
                perl-indent-level 2)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "customized")))
  (add-hook 'image-mode-new-window-functions
            (lambda (arg)
              (display-line-numbers-mode -1)))
  
  (add-hook 'xwidget-webkit-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)))
  (ido-mode 1)
  (set-fonts)
  (set-keys)
  (add-hook 'org-mode-hook #'variable-pitch-mode)
  (package-initialize))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha-background)))
    (if (= alpha 100) 85 100)))

(defun set-fonts ()
  (let ((font-height 140))
    (set-fontset-font "fontset-default" 'han (font-spec :family "HanaMinA"))
    (set-fontset-font "fontset-default" 'han (font-spec :family "HanaMinB")
                      nil 'append)
    (set-fontset-font "fontset-default" 'han (font-spec :family "Noto Sans TC")
                      nil 'append)
    (set-fontset-font "fontset-default" 'han (font-spec :family "Noto Sans SC")
                      nil 'append)
    (set-fontset-font "fontset-default" 'big5 (font-spec :family "HanaMinA"))
    (set-fontset-font "fontset-default" 'big5 (font-spec :family "HanaMinB")
                      nil 'append)
    (set-fontset-font "fontset-default" 'big5 (font-spec :family "Noto Sans TC")
                      nil 'append)
    (set-fontset-font "fontset-default" 'big5 (font-spec :family "Noto Sans SC")
                      nil 'append)
    (set-fontset-font "fontset-default" 'kana (font-spec :family "Noto Sans JP"))
    (set-fontset-font "fontset-default" 'symbol (font-spec :family "IBMPlexMono"))
    (set-fontset-font "fontset-default" 'symbol (font-spec :family "Noto Sans Mono")
                      nil 'append)
    (setq face-font-rescale-alist '(("Noto Sans TC" . 0.92)
                                    ("Noto Sans SC" . 0.92)
                                    ("Noto Sans JP" . 0.92)
                                    ("Noto Sans Mono" . 0.92)))
    (set-face-attribute 'default nil
                        :family "IBMPlexMono"
                        :height font-height)
    (set-face-attribute 'variable-pitch nil
                        :family "IBMPlexSans"
                        :height font-height
                        :inherit 'default)
    (set-face-attribute 'fixed-pitch nil
                        :family "IBMPlexMono"
                        :height font-height
                        :inherit 'deafult)
    (set-face-attribute 'dashboard-items-face nil
                        :height font-height)
    (set-face-attribute 'header-line nil
                        :inherit nil)
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
    (set-face-attribute 'line-number nil
                        :slant 'normal
                        :weight 'normal
                        :inherit 'fixed-pitch)
    (set-face-attribute 'line-number-current-line nil
                        :slant 'normal
                        :weight 'bold
                        :inherit 'fixed-pitch)
    (set-face-attribute 'dashboard-banner-logo-title nil
                        :weight 'bold)
    (set-face-attribute 'dashboard-footer nil
                        :slant 'italic)
    (set-face-attribute 'font-lock-comment-face nil
                        :slant 'italic)))

(defun set-keys ()
  (dolist (keybinding '(("<C-wheel-up>" . text-scale-increase)
                        ("<C-wheel-down>" . text-scale-decrease)
                        ("C-x C-r" . recentf-open-files)
                        ("C-M-=" . count-words)
                        ("C-x C-b" . ibuffer)
                        ("C-y" . clipboard-yank)
                        ("C-w" . clipboard-kill-region)
                        ("M-w" . clipboard-kill-ring-save)))
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

(add-hook 'emacs-startup-hook #'startup-function)

(add-hook 'server-after-make-frame-hook
          (lambda ()
            (set-fonts)
            (if (equal (buffer-name) "*dashboard*")
                (revert-buffer))))
