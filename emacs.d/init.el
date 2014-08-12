;
; Packages
;

(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalde" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn (package-refresh-contents)
	   (package-install 'use-package)))
(require 'use-package)

(defun my/add-use-package-to-imenu ()
  (interactive)
  (add-to-list 'imenu-generic-expression
	       (list "Packages Used" "\\s-*(use-package\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 1)))
(add-hook 'emacs-lisp-mode-hook 'my/add-use-package-to-imenu)

;
; Minimal UI
;

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(if (fboundp 'tooltip-mode) (tooltip-mode 0))
(setq
 inhibit-splash-screen t
 tooltip-use-echo-area t)

;
; Color themes
;

(use-package solarized-theme
  :ensure
  :init
  (load-theme 'solarized-dark t))

;
; Basic UI
;

; evil - vi compatible keybindings
(use-package evil
  :ensure
  :config
  (progn
    (defadvice ansi-term (after turn-off-evil-in-ansi-term ())
      "Disable whatever the fuck evil mode does in ansi-term"
      (turn-off-evil-mode))
    (ad-activate 'ansi-term)
    (evil-mode 1)))

;
; Basic Configuration
;

(set-frame-font "Source Code Pro-10.5")

(setq
 truncate-lines t ; line wrapping? wtf?
 comment-empty-lines t ; comment empty lines too
 abbrev-file-name "~/.emacs.d/abbref_defs" ; save abbreviations here
 indent-tabs-mode nil ; tabs are evil
 c-basic-offset 4
 c-default-style "k&r"
 tab-width 4)

; show matching parenthesis
(show-paren-mode 1)

; show column numbers too
(column-number-mode 1)

; save and restore previous sessions
(desktop-save-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(backup-directory-alist (quote ((".*" . "~/.saves")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package autopair
  :ensure
  :diminish autopair
  :idle
  (autopair-global-mode))

; battery status
(use-package fancy-battery
  :ensure
  :idle
  (fancy-battery-mode))

;
; Navigation
;

(use-package ido
  :ensure
  :config
  (progn
    (setq ido-everywhere t)
    (setq ido-enable-flex-matching t))
  :init
  (ido-mode 1))

(use-package ido-vertical-mode
  :ensure
  :init
  (ido-vertical-mode))

(use-package ido-ubiquitous
  :ensure
  :init
  (ido-ubiquitous-mode))

(use-package flx-ido
  :ensure
  ; :config
  ; (setq ido-use-faces nil)
  :init
  (flx-ido-mode 1))

(use-package imenu
  :ensure)

(use-package idomenu
  :ensure
  :bind
  ("C-c i" . idomenu))

(use-package smex
  :ensure
  :config
  (setq smex-key-advice-ignore-menu-bar t)
  :bind
  ("M-x" . smex)
  :init
  (smex-initialize))

;
; Buffers
;

(use-package popwin
  :ensure
  :config
  (popwin-mode 1))

(use-package uniquify
  :config
  (progn
    (setq uniquify-strip-comon-prefix t)
    (setq uniquify-buffer-name-style 'forward)))

;
; Projects
;

(use-package projectile
  :ensure
  :config
  (projectile-global-mode))


(use-package magit
  :ensure
  :commands magit-status)

;
; Auto-Complete
;

(use-package auto-complete
  :ensure
  :init
  (progn
    (require 'auto-complete-config)
    (ac-config-default)
    (setq ac-auto-show-menu 0.2)
    (setq ac-auto-start 0)
    (setq ac-candidate-menu-min 1)
    (setq ac-delay 0.2)
    (setq ac-disable-inline t)
    (setq ac-quick-help-delay 1)
    (setq ac-quick-help-height 60)
    (setq ac-show-menu-immediately-on-auto-complete t)
    (setq ac-use-quick-help t)
    (global-auto-complete-mode)))

;
; Snippets
;
(use-package yasnippet
  :ensure
  :idle
  :config
  (progn
    (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/")
    (yas--initialize)))


;
; Flycheck
;

(use-package flycheck
  :ensure
  :idle (global-flycheck-mode))

;
; Set up packages/settings for different modes
;

; text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

; org-mode
(use-package org
  :ensure)
(setq org-directory "~/notebook")
(add-to-list 'auto-mode-alist (cons (concat org-directory "/.*") 'org-mode))
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-log-states-order-reversed nil)
(setq org-agenda-files (list org-directory))

(use-package markdown-mode
  :ensure)

; js2-mode
(use-package js2-mode
  :ensure
  :mode "\\.js\\'")

; emmet
(use-package emmet-mode
  :ensure
  :commands emmet-mode
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :init
  (progn
    (add-hook 'python-mode-hook 'auto-complete-mode)
    (add-hook 'python-mode-hook 'sphinx-doc-mode)))

(use-package jedi
  :ensure
  :init
  (progn
    ; need to pip install epc and jedi
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:complete-on-dot t)))
; TODO: in evil's insert state, map the normal autocomplete to jedi

(use-package graphiviz-dot-mode
  :mode "'\\.dot\\'"
  :config
  (setq graphviz-dot-view-command "dotty"))

(use-package gist
  :ensure
  :defer t
  :config
  (setq gist-view-gist 1))


;
; Custom macros
;

