
;;; Commentary:

;; My user-init-file, with all my configuration

;;; Code:

(setq my-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold my-gc-cons-threshold)))

;;;
;;; Packages
;;;

(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; Use use-package to install packages declaratively and keep
;; configuration with the package name
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-defer t)
(setq use-package-always-ensure t)
(setq use-package-enable-imenu-support t)
(require 'use-package)

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

(setq org-directory (expand-file-name "~/notebook"))
(setq my-default-notes-file (concat org-directory "/daily-log.org"))
(setq initial-buffer-choice my-default-notes-file)

;; Some nice themes that I like
(use-package dracula-theme)
(use-package flatui-theme)
(use-package leuven-theme)
(use-package material-theme)
(use-package monokai-theme)
(use-package solarized-theme)
(use-package zenburn-theme)
(use-package challenger-deep-theme)

(load-theme 'dracula t)

(set-frame-font "Source Code Pro-8.0" t)

(use-package diminish)

(use-package spaceline
  :demand
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

;; use icons instead of text in modeline
(use-package mode-icons
  :config
  (mode-icons-mode))

;;;
;;; Keybindings
;;;

;; evil - vi compatible keybindings
(use-package evil
  :demand
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  (add-hook 'git-commit-mode-hook #'evil-insert-state)
  :config
  (progn
    (evil-set-initial-state 'project-explorer-mode 'emacs)
    (evil-set-initial-state 'term-mode 'emacs)
    ;; make * and # behave like vim and include _ (and -) in search
    (setq-default evil-symbol-word-search t)
    (evil-mode 1)))

(use-package evil-collection
  :demand
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-init))

(use-package which-key
  :diminish which-key-mode
  :demand
  :config
  (which-key-mode 1))

;;;
;;; Basic Configuration
;;;

;; Put custom definitions in another file and don't load it
(setq custom-file (locate-user-emacs-file "custom.el"))
;; (load custom-file)

(setq-default
 indent-tabs-mode nil ; tabs are evil
 c-basic-offset 4)

(setq
 truncate-lines t ; line wrapping? wtf?
 comment-empty-lines t ; comment empty lines too
 tab-width 4)

;; single space after period ends sentence
(setq sentence-end-double-space nil)

;; show matching parenthesis
(show-paren-mode 1)

;; show column numbers too
(column-number-mode 1)

;; backups
(setq backup-directory-alist '((".*" . "~/.saves"))
      backup-by-copying t
      delete-old-versions t)

;; replace selection when typing
(pending-delete-mode)

;; paste (aka yank) at point, not where mouse is clicked
(setq mouse-yank-at-point t)

;; I can't understand why editing a version-controlled file (through a
;; symlink) might be dangerous. Maybe I need to use RCS.
(setq vc-follow-symlinks t)

;; start the emacs server
(use-package server
  :init
  (add-hook 'after-init-hook #'server-start))

(setq help-window-select 'always)

;;;
;;; Navigation
;;;

(use-package recentf
  :config
  (setq recentf-max-saved-items 100))

(use-package smex
  :config
  (setq smex-key-advice-ignore-menu-bar t)
  (smex-initialize))

(use-package ido
  :disabled
  :config
  (progn
    (use-package ido-vertical-mode
      :config
      (ido-vertical-mode))

    (use-package ido-ubiquitous
      :config
      (ido-ubiquitous-mode))

    (use-package flx-ido
      :config
      (flx-ido-mode 1))

    (use-package imenu
      :config
      (setq imenu-max-item-length "Unlimited"))

    (use-package idomenu
      :bind
      ("C-c i" . idomenu))

    (setq ido-everywhere t)
    (setq ido-enable-flex-matching t)
    (ido-mode 1)))

(use-package ivy
  :demand
  :diminish ivy-mode
  ;; ido-style keybindings
  :bind (:map ivy-minibuffer-map
              ("C-j" . ivy-immediate-done)
              ("RET" . ivy-alt-done))
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "")
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex)
          (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq projectile-completion-system 'ivy)
  (ivy-mode 1)
  (defun my-ivy-action-insert (x)
    (insert
     (if (stringp x)
         x
       (car x))))

  (defun my-ivy-action-kill-new (x)
    (kill-new
     (if (stringp x)
         x
       (car x))))
  (ivy-set-actions
   t
   '(("i" my-ivy-action-insert "insert")
     ("c" my-ivy-action-kill-new "copy")))

  (use-package flx)
  (use-package ivy-hydra)
  (use-package swiper
    :bind
    ("C-s" . swiper))
  (use-package counsel
    :demand
    :diminish counsel-mode
    :bind
    ("M-x" . counsel-M-x)
    ("C-c i" . counsel-imenu)
    :init
    (counsel-mode 1)))

;;;
;;; Projects
;;;

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  ;; "P[project name]" to indicate a project or empty string
  (setq projectile-mode-line
        '(:eval
          (let ((mode-line
                 (condition-case nil
                     (format " P[%s]"
                             (file-name-nondirectory
                              (directory-file-name (projectile-project-root))))
                   (error ""))))
            mode-line)))
  (projectile-mode 1))

(use-package ack-and-a-half)

(use-package ag)

(use-package project-explorer
  :commands project-explorer-open)

(use-package magit
  :commands magit-status)

(use-package git-gutter
  :diminish ""
  :demand
  :config
  (global-git-gutter-mode))

(use-package hydra
  :config
  (defun my-switch-or-create-ansi-term ()
    "Switch to ansi-term buffer if one exists; otherwise make one."
    (interactive)
    (if (and (boundp 'term-ansi-buffer-name)
             term-ansi-buffer-name
             (term-check-proc term-ansi-buffer-name))
        (switch-to-buffer term-ansi-buffer-name)
      (ansi-term "/bin/bash")))
  (defhydra my-hydra-main (:color blue)
    "Main"
    ("b" my-hydra-file-bugs/body "bugs")
    ("i" (find-file user-init-file) "init file")
    ("o" my-hydra-org/body "org")
    ("p" my-hydra-projects/body "projects")
    ("t" my-switch-or-create-ansi-term "terminal"))
  (defhydra my-hydra-file-bugs (:color blue)
    "Bugs"
    ("e" report-emacs-bug "emacs")
    ("i" (browse-url "http://icedtea.classpath.org/bugzilla/enter_bug.cgi?product=IcedTea") "icedtea")
    ("r" (browse-url "https://bugzilla.redhat.com/enter_bug.cgi") "redhat")
    ("t" (browse-url "http://icedtea.classpath.org/bugzilla/enter_bug.cgi?product=Thermostat") "thermostat"))
  (defhydra my-hydra-org (:color blue)
    "Org"
    ("a" (org-agenda nil "n") "agenda")
    ("c" org-capture "capture")
    ("d" (find-file (concat org-directory "/to-discuss.org")) "to discuss")
    ("k" (find-file (concat org-directory "/checklist-professional.org")) "checklist")
    ("l" (find-file my-default-notes-file) "daily log"))
  (defhydra my-hydra-projects (:color blue)
    "Projects"
    ("e" project-explorer-open "project explorer")
    ("c" magit-clone "clone git project")
    ("g" magit-status "magit")
    ("p" projectile-switch-project "switch projects"))
  :bind ("<f12>" . my-hydra-main/body))

;;;
;;; Buffers
;;;

(use-package uniquify
  ;; ensure t breaks uniquify, for some reason
  :ensure nil
  :config
  (progn
    (setq uniquify-strip-common-prefix t)
    (setq uniquify-buffer-name-style 'forward)))

;;;
;;; Common Modes
;;;

(use-package flyspell
  :ensure nil
  :diminish flyspell-mode
  :init
  (add-hook 'text-mode-hook #'turn-on-flyspell)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (use-package flycheck-package
    :init
    (add-hook 'after-init-hook #'flycheck-package-setup)))

(use-package company
  :demand
  :diminish company-mode
  :config
  (add-hook 'after-init-hook #'global-company-mode)
  (setq company-idle-delay 0.5))

(use-package abbrev
  :ensure nil
  :diminish ""
  :init
  (setq abbrev-file-name "~/.emacs.d/abbref_defs") ; save abbreviations here
  (setq-default abbrev-mode t))

(use-package yasnippet
  :diminish yas-minor-mode
  :bind ("M-/" . company-yasnippet)
  :init
  (progn
    (use-package yasnippet-snippets)
    (defun my-disable-yas-in-term ()
      (yas-minor-mode -1))
    (add-hook 'after-init-hook #'yas-global-mode)
    (add-hook 'term-mode-hook #'my-disable-yas-in-term))
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/"))

(use-package editorconfig
  :diminish ""
  :config
  (editorconfig-mode 1))

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package bug-reference
  :config
  (progn
    (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
    (add-hook 'text-mode-hook #'bug-reference-mode)
    (setq bug-reference-bug-regexp
          "\\<\\(RFC\\|PR\\|JDK-\\|JEP-?\\|RH\\(?:BZ\\)\\|CVE-?\\) ?\\([0-9-]+\\)\\>")
    (setq bug-reference-url-format
          (lambda ()
            (let ((kind (match-string-no-properties 1))
                  (id (match-string-no-properties 2)))
              (cond ((or (string= kind "RH") (string= kind "RHBZ"))
                     (format "https://bugzilla.redhat.com/show_bug.cgi?id=%s" id))
                    ((string= kind "PR")
                     (format "http://icedtea.classpath.org/bugzilla/show_bug.cgi?id=%s" id))
                    ((string-prefix-p "JDK" kind)
                     (format "https://bugs.openjdk.java.net/browse/JDK-%s" id))
                    ((string-prefix-p "JEP" kind)
                     (format "http://openjdk.java.net/jeps/%s" id))
                    ((string-prefix-p "CVE" kind)
                     (format "http://www.cve.mitre.org/cgi-bin/cvename.cgi?name=%s" id))
                    ((string= "RFC" kind)
                     (format "http://tools.ietf.org/html/rfc%s" id))
                    (t (error (concat "Unknown item type: " kind)))))))))

(use-package crux)

(use-package gist
  :defer t
  :config
  (setq gist-view-gist 1))

(use-package hideshow
  :ensure nil ;; builtin
  :config
  (add-hook 'json-mode-hook #'hs-minor-mode)
  (define-key evil-normal-state-map (kbd "TAB") #'hs-toggle-hiding))

(use-package fpaste
  :commands fpaste
  :config
  (setq fpaste-user user-login-name))

(use-package rh-paste
  :load-path "~/devel/emacs-rh-paste/"
  :ensure nil
  :commands rh-paste)

(use-package rainbow-mode
  :config
  (rainbow-mode))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'scheme-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode))

;;;
;;; Text/Markup Editing Modes
;;;

(add-hook 'text-mode-hook #'turn-on-auto-fill)

(use-package writeroom-mode)

(use-package org
  :init
  (add-to-list 'auto-mode-alist `(,(concat org-directory "/.*'") . org-mode))
  :config
  (progn
    ;; fontify code in code blocks
    (setq org-src-fontify-natively t)

    ;; bullets

    (use-package org-bullets
      :config
      (setq org-bullets-bullet-list '("#" "##" "###" "####"))
      (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

    ;; capture
    (setq org-default-notes-file my-default-notes-file)
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
             "* TODO %?\n:PROPERTIES:\n:Created: %U\n:END:\n  %i\n  %a"
             :created t)))

    ;; refile
    (setq org-refile-use-outline-path t)

    ;; agenda

    (setq org-agenda-files (list org-directory))
    (setq org-agenda-span 'fortnight)
    ;; don't show tasks as scheduled if they are already shown as deadline
    (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
    ;; show log mode by default
    (setq org-agenda-show-log t)
    ;; show agenda in the current window and don't modify my existing window setup
    (setq org-agenda-window-setup 'current-window)
    ;; don't show tasks that are scheduled or have deadlines in the
    ;; normal todo list
    (setq org-agenda-todo-ignore-deadlines (quote all))
    (setq org-agenda-todo-ignore-scheduled (quote all))
    ;;sort tasks in order of when they are due and then by priority
    (setq org-agenda-sorting-strategy
          '((agenda deadline-up priority-down)
            (todo priority-down category-keep)
            (tags priority-down category-keep)
            (search category-keep)))
    (setq org-deadline-warning-days 14)
    (setq org-log-states-order-reversed nil)))

(use-package adoc-mode
  :init
  :mode "\\.adoc\\'")

(use-package diff-mode
  ;; files with '.patch.' in the middle of their name are patch files too
  :mode "\\.patch\\..*\\'")

(use-package dockerfile-mode)

(use-package markdown-mode
  :mode ("\\.md\\'"
         ("\\README.md\\'" . gfm-mode)))

(use-package markdown-toc)

(use-package pandoc-mode)

(use-package yaml-mode)

(use-package json-mode)

(use-package web-mode
  :mode "\\.cshtml\\'")

(use-package sgml-mode
  :config
  (advice-add 'sgml-delete-tag
              :after
              (lambda (arg)
                (indent-region (point-min) (point-max)))))

(use-package emmet-mode
  :commands emmet-mode
  :config
  (progn
    (add-hook 'sgml-mode-hook #'emmet-mode)
    (add-hook 'web-mode-hook #'emmet-mode)
    (add-hook 'css-mode-hook #'emmet-mode)
    (add-hook 'nxml-mode-hook #'emmet-mode)))

(use-package jinja2-mode)

(use-package rpm-spec-mode)

(use-package graphviz-dot-mode
  :mode "'\\.dot\\'"
  :config
  (setq graphviz-dot-view-command "dotty"))

(use-package gitignore-mode :defer t)

(use-package gitconfig-mode :defer t)

(use-package hgignore-mode :defer t)

(use-package vimrc-mode :defer t)

;;;
;;; Programming Modes
;;;


(use-package semantic
  :config
  (add-hook 'prog-mode-hook #'semantic-mode))

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(use-package fixme-mode
  :config
  (add-hook 'prog-mode-hook #'fixme-mode))

(use-package ws-butler
  :commands ws-butler-mode
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode))

(use-package elec-pair
  :config
  (electric-pair-mode 1))

(use-package pretty-symbols)

(use-package elisp-mode
  :ensure nil
  :demand
  :bind (:map emacs-lisp-mode-map
              ("C-c e b" . eval-buffer)
              ("C-c e d" . eval-defun)
              ("C-c e f" . emacs-lisp-byte-compile-and-load)
              ("C-c e r" . eval-region)
              ("C-c e t" . my-run-all-ert-tests)
              ("C-c f f" . find-function)
              ("C-c f l" . find-library)
              ("C-c f v" . find-variable))
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  ;; Navigation in emacs lisp files
  (defun my-add-sections-to-imenu ()
    ""
    (interactive)
    (add-to-list 'imenu-generic-expression
                 (list "Sections" "^;;; +\\(.+\\)" 1)))
  (add-hook 'emacs-lisp-mode-hook #'my-add-sections-to-imenu)
  (defun my-run-all-ert-tests ()
    "Run all ert tests for current buffer, then focus back to it."
    (interactive)
    (let ((original-buffer (current-buffer)))
      (eval-buffer)
      (ert t)
      (select-window (get-buffer-window original-buffer)))))

(use-package cmake-mode)

(use-package cc-mode
  :defer t
  :config
  (add-to-list 'c-default-style '(other . "k&r")))

(use-package omnisharp
  :demand
  :config
  (setq omnisharp-server-executable-path
        (expand-file-name "~/local/omnisharp-1.25.0/run"))
  (add-to-list 'company-backends 'company-omnisharp))

(defun my-csharp-mode-setup ()
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)
  (local-set-key (kbd "C-c C-c") 'recompile))

(use-package csharp-mode :defer t
  :init
  (add-hook 'csharp-mode-hook #'omnisharp-mode)
  (add-hook 'csharp-mode-hook #'my-csharp-mode-setup))

(use-package geiser
  :config
  (setq geiser-repl-history-filename
        (convert-standard-filename
         (locate-user-emacs-file "geiser-history" ".geiser-history"))))

(use-package groovy-mode)

(use-package js2-mode
  :mode "\\.js\\'")

(use-package python
  :mode ("\\.py\\'" . python-mode))

(use-package sphinx-doc
  :config
  (add-hook 'python-mode-hook #'sphinx-doc-mode))

(use-package jedi
  :config
  (progn
    ;; need to pip install epc and jedi
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:complete-on-dot t)))
;; TODO: in evil's insert state, map the normal autocomplete to jedi

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook #'anaconda-mode))

(use-package java-snippets)

(use-package jtreg
  :load-path "~/devel/emacs-jtreg/"
  :ensure nil
  :config
  (setq jtreg-dir "~/local/jtreg/lib/"))

(use-package jar-manifest-mode)

(use-package lua-mode)

(use-package restclient
  :commands restclient-mode
  :config
  (use-package company-restclient))

;;;
;;; Utility functions
;;;

;;;
;;; Too small to be packages
;;;

;; Test: RFC 1231

(defun my-rfc-lookup-text (rfc-num)
  "Look up an RFC-NUM using tools.ietf.org."
  (interactive (list
                (read-number "RFC number:" (thing-at-point 'number))))
  (let ((url (format "https://tools.ietf.org/rfc/rfc%s.txt" rfc-num))
        (display-rfc (lambda (status)
                       (let ((body (progn (goto-char (point-min))
                                          ;; find first newline indicating end of http headers
                                          (re-search-forward "^$")
                                          ;; skip empty lines
                                          (re-search-forward "[^\n]")
                                          (beginning-of-line)
                                          (prog1 (buffer-substring (point) (point-max))
                                            (kill-buffer))))
                             (buffer (progn
                                       (when (get-buffer "*rfc*")
                                         (kill-buffer "*rfc*"))
                                       (get-buffer-create "*rfc*"))))
                         (with-current-buffer buffer
                           (erase-buffer)
                           (goto-char (point-min))
                           (insert body)
                           (goto-char (point-min)))
                         (view-buffer-other-window buffer)))))
    (url-retrieve url display-rfc)))

(defun my-rfc-lookup-html (rfc-num)
  "Look up RFC-NUM using tools.ietf.org."
  (interactive (list
                (read-number "RFC number:" (thing-at-point 'number))))
  (let ((url (format "https://tools.ietf.org/html/rfc%s.txt" rfc-num)))
    (eww-browse-url url)))

(defun my-mutt-mail-region (point mark)
  "Generate a mail from the text between POINT and MARK and send it out."
  (interactive "r")
  (unless (region-active-p)
    (user-error "Select a region to generate mail from"))
  (let ((file (make-temp-file "emacs-mutt-mail"))
        (subject (format-time-string "Status %F"))
        (recipient "dotnet-team@redhat.com"))
    (write-region point mark file)
    (start-process "emacs-mutt-mail"
                   "emacs-mutt-mail"
                   "gnome-terminal" "--" "mutt" "-i" file "-s" subject recipient)))

;;;
;;; Playground
;;;

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'my-gnus-config)

;;; init.el ends here
