
; Navigation in this file
(defun my/add-categories-to-imenu ()
  (interactive)
  (add-to-list 'imenu-generic-expression
	       (list "Categories" "^;;; \\(.+\\)" 1)))
(add-hook 'emacs-lisp-mode-hook 'my/add-categories-to-imenu)

;;;
;;; Packages
;;;

(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalde" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
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

;;;
;;; Looks
;;;

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode 0))
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(if (fboundp 'tooltip-mode) (tooltip-mode 0))
(setq
 inhibit-splash-screen t
 tooltip-use-echo-area t)

(use-package solarized-theme
  :ensure
  :init
  (load-theme 'solarized-dark t))

; battery status
(use-package fancy-battery
  :ensure
  :idle
  (fancy-battery-mode))

(set-frame-font "Source Code Pro-10.5")

;;;
;;; Keybindings
;;;

; evil - vi compatible keybindings
(use-package evil
  :ensure
  :config
  (progn
    (evil-set-initial-state 'project-explorer-mode 'emacs)
    (defadvice ansi-term (after turn-off-evil-in-ansi-term ())
      "Disable whatever the fuck evil mode does in ansi-term"
      (turn-off-evil-mode))
    (ad-activate 'ansi-term)
    (evil-mode 1)))

;
; Basic Configuration
;

(setq-default
 indent-tabs-mode nil ; tabs are evil
 c-basic-offset 4)

(setq
 truncate-lines t ; line wrapping? wtf?
 comment-empty-lines t ; comment empty lines too
 abbrev-file-name "~/.emacs.d/abbref_defs" ; save abbreviations here
 c-default-style "k&r"
 tab-width 4)

; single space after period ends sentence
(setq sentence-end-double-space nil)

; show matching parenthesis
(show-paren-mode 1)

; show column numbers too
(column-number-mode 1)

; backups
(setq backup-directory-alist '((".*" . "~/.saves"))
      backup-by-copying t)

;;;
;;; Navigation
;;;

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

;;;
;;; Projects
;;;

(use-package projectile
  :ensure
  :init
  ; "P[project name]" to indicate a project or empty string
  (setq projectile-mode-line '(:eval
                               (let ((mode-line (condition-case nil
                                                    (format " P[%s]"
                                                            (file-name-nondirectory (directory-file-name (projectile-project-root))))
                                                  (error ""))))
                                 mode-line)))
  :config
  (projectile-global-mode))

(use-package project-explorer
  :ensure
  :bind ("<f11>" . project-explorer-open))

(use-package gitignore-mode :ensure)

(use-package gitconfig-mode :ensure)

(use-package magit
  :ensure
  :bind ("<f12>" . magit-status)
  :commands magit-status)

(use-package git-gutter
  :ensure
  :diminish ""
  :init
  (global-git-gutter-mode))


(use-package monky
  :ensure)

;;;
;;; Buffers
;;;

; save and restore previous sessions
(use-package desktop
  :config
  (add-to-list 'desktop-modes-not-to-save 'magit-mode)
  :init
  (desktop-save-mode 1))

(use-package uniquify
  :config
  (progn
    (setq uniquify-strip-comon-prefix t)
    (setq uniquify-buffer-name-style 'forward)))

;;;
;;; Common Modes
;;;

(use-package flycheck
  :ensure
  :init (global-flycheck-mode))

(use-package flycheck-pos-tip
  :ensure)

(use-package company
  :ensure
  :init
  (setq company-idle-delay 0)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package yasnippet
  :ensure
  :config
  (global-set-key (kbd "M-/") 'company-yasnippet)
  :init
  (progn
    (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/")
    (yas-global-mode)))

(use-package gist
  :ensure
  :defer t
  :config
  (setq gist-view-gist 1))

(use-package rainbow-mode
  :ensure
  :init
  (rainbow-mode))

(use-package aggressive-indent
  :ensure
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'scheme-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode))

;;;
;;; Text Editing Modes
;;;

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

; org-mode
(use-package org
  :ensure
  :config
  (progn
    (setq org-directory "~/notebook")
    (add-to-list 'auto-mode-alist (cons (concat org-directory "/.*") 'org-mode))
    (setq org-default-notes-file (concat org-directory "/notes.org"))
    (setq org-log-states-order-reversed nil)
    (setq org-agenda-files (list org-directory))))

(use-package org-pandoc
  :ensure)

(use-package adoc-mode
  :ensure)

(use-package markdown-mode
  :ensure)

(use-package markdown-toc
  :ensure)

(use-package pandoc-mode
  :ensure)

(use-package yaml-mode
  :ensure)

(use-package json-mode
  :ensure)

(use-package web-mode
  :ensure)

(use-package emmet-mode
  :ensure
  :commands emmet-mode
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode)))

(use-package jinja2-mode
  :ensure)

(use-package rpm-spec-mode
  :ensure)

(use-package graphviz-dot-mode
  :ensure
  :mode "'\\.dot\\'"
  :config
  (setq graphviz-dot-view-command "dotty"))

; blasphemy
(use-package vimrc-mode
  :ensure)

;;;
;;; Programming Modes
;;;

(use-package fixme-mode
  :ensure
  :init
  (add-hook 'prog-mode-hook 'fixme-mode))

(use-package autopair
  :ensure
  :diminish autopair
  :init
  (autopair-global-mode))

(use-package pretty-symbols
  :ensure)

(use-package cmake-mode
  :ensure)

(use-package geiser
  :ensure)

(use-package js2-mode
  :ensure
  :mode "\\.js\\'")

(use-package python
  :ensure
  :mode ("\\.py\\'" . python-mode))

(use-package sphinx-doc
  :ensure
  :config
  (add-hook 'python-mode-hook 'sphinx-doc-mode))

(use-package jedi
  :ensure
  :init
  (progn
    ; need to pip install epc and jedi
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:complete-on-dot t)))
; TODO: in evil's insert state, map the normal autocomplete to jedi

(use-package anaconda-mode
  :ensure
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package java-snippets :ensure)

(use-package lua-mode :ensure)

(use-package restclient
  :ensure)

;
; Custom macros
;

