
;;; Commentary:

;; My user-init-file, with all my configuration

;;; Code:

;;;
;;; Packages
;;;

(setq package-enable-at-startup nil)
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("marmalde" . "http://marmalade-repo.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(defun my-add-use-package-to-imenu ()
  (interactive)
  (add-to-list 'imenu-generic-expression
	       (list "Packages Used" "\\s-*(use-package\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 1)))
(add-hook 'emacs-lisp-mode-hook #'my-add-use-package-to-imenu)

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
  :init
  (load-theme 'solarized-dark t))

(set-frame-font "Source Code Pro-10.5" t)

(use-package diminish)

;;;
;;; Keybindings
;;;

;; evil - vi compatible keybindings
(use-package evil
  :config
  (progn
    (evil-set-initial-state 'project-explorer-mode 'emacs)
    (defadvice ansi-term (after turn-off-evil-in-ansi-term ())
      "Disable whatever the fuck evil mode does in ansi-term"
      (turn-off-evil-mode))
    (ad-activate 'ansi-term)
    (evil-mode 1)))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))

;;;
;;; Basic Configuration
;;;

(setq-default
 indent-tabs-mode nil ; tabs are evil
 c-basic-offset 4)

(setq
 truncate-lines t ; line wrapping? wtf?
 comment-empty-lines t ; comment empty lines too
 abbrev-file-name "~/.emacs.d/abbref_defs" ; save abbreviations here
 tab-width 4)

;; single space after period ends sentence
(setq sentence-end-double-space nil)

;; show matching parenthesis
(show-paren-mode 1)

;; show column numbers too
(column-number-mode 1)

;; backups
(setq backup-directory-alist '((".*" . "~/.saves"))
      backup-by-copying t)

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
  (add-hook 'after-init-hook 'server-start))

;;;
;;; Navigation
;;;

(use-package recentf)

(use-package ido
  :init
  (progn
    (setq ido-everywhere t)
    (setq ido-enable-flex-matching t)
    (ido-mode 1)))

(use-package ido-vertical-mode
  :init
  (ido-vertical-mode))

(use-package ido-ubiquitous
  :init
  (ido-ubiquitous-mode))

(use-package flx-ido
  :init
  (flx-ido-mode 1))

(use-package imenu
  :config
  (setq imenu-max-item-length "Unlimited"))

(use-package idomenu
  :bind
  ("C-c i" . idomenu))

(use-package smex
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
  :init
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
  :config
  (projectile-global-mode))

(use-package ack-and-a-half)

(use-package project-explorer)

(use-package magit
  ;; :bind ("<f12>" . magit-status)
  :commands magit-status)

(use-package git-gutter
  :diminish ""
  :init
  (global-git-gutter-mode))

(use-package hydra
  :config
  (defhydra hydra-main (:color blue)
    "Main"
    ("b" hydra-file-bugs/body "bugs")
    ("d" dashboard "dashboard")
    ("i" (find-file user-init-file) "init file")
    ("o" hydra-org/body "org")
    ("p" hydra-projects/body "projects"))
  (defhydra hydra-file-bugs (:color blue)
    "Bugs"
    ("e" report-emacs-bug "emacs")
    ("i" (browse-url "http://icedtea.classpath.org/bugzilla/enter_bug.cgi?product=IcedTea") "icedtea")
    ("r" (browse-url "https://bugzilla.redhat.com/enter_bug.cgi") "redhat")
    ("t" (browse-url "http://icedtea.classpath.org/bugzilla/enter_bug.cgi?product=Thermostat") "thermostat"))
  (defhydra hydra-org (:color blue)
    "Org"
    ("a" org-agenda "agenda")
    ("c" org-capture "capture")
    ("d" (find-file (concat org-directory "/to-discuss.org")) "to discuss")
    ("l" (find-file (concat org-directory "/daily-log.org")) "daily log"))
  (defhydra hydra-projects (:color blue)
    ("e" project-explorer-open "project explorer")
    ("c" magit-clone "clone git project")
    ("g" magit-status "magit")
    ("p" projectile-switch-project "switch projects"))
  :bind ("<f12>" . hydra-main/body))

;;;
;;; Buffers
;;;

;; save and restore previous sessions
(use-package desktop
  :config
  (add-to-list 'desktop-modes-not-to-save 'magit-mode)
  :init
  (desktop-save-mode 1))

(use-package uniquify
  :ensure nil
  :config
  (progn
    (setq uniquify-strip-common-prefix t)
    (setq uniquify-buffer-name-style 'forward)))

;;;
;;; Common Modes
;;;

(use-package flycheck
  :commands global-flycheck-mode
  :init
  (use-package flycheck-package
    :config
    (flycheck-package-setup))
  :config (global-flycheck-mode))

(use-package company
  :diminish company-mode
  :init
  (setq company-idle-delay 0)
  (add-hook 'after-init-hook #'global-company-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (progn
    (global-set-key (kbd "M-/") 'company-yasnippet)
    (defun my-disable-yas-in-term ()
      (yas-minor-mode -1))
    (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/")
    (yas-global-mode)
    (add-hook 'term-mode-hook #'my-disable-yas-in-term)))

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

(use-package define-word
  :defer t
  :bind ("C-c d" . my-define-word)
  :config
  (defun my-define-word (word)
    (interactive (list (read-string "Word: " (thing-at-point 'word t))))
    (define-word word)))

(use-package gist
  :defer t
  :config
  (setq gist-view-gist 1))

(use-package fpaste
  :commands fpaste
  :config
  (setq fpaste-user user-login-name))

(use-package rainbow-mode
  :init
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
(add-hook 'text-mode-hook #'turn-on-flyspell)

(use-package writeroom-mode)

(use-package org
  :defer t
  :init
  (progn
    ;; fontify code in code blocks
    (setq org-src-fontify-natively t)
    (setq org-directory "~/notebook")
    (add-to-list 'auto-mode-alist (cons (concat org-directory "/.*") 'org-mode))
    (setq org-default-notes-file (concat org-directory "/notes.org"))
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
    (setq org-log-states-order-reversed nil)
    (use-package org-bullets
      :defer t
      :init
      (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))))

(use-package adoc-mode
  :defer t)

(use-package diff-mode
  :config
  ;; files with '.patch.' in the middle of their name are patch files too
  (add-to-list 'auto-mode-alist
               '("\\.patch\\..*\\'" . diff-mode)))

(use-package dockerfile-mode)

(use-package markdown-mode
  :defer t)

(use-package markdown-toc
  :defer t)

(use-package pandoc-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package json-mode
  :defer t)

(use-package web-mode
  :defer t)

(use-package sgml-mode
  :defer t
  :config
  (advice-add 'sgml-delete-tag
              :after
              (lambda (arg)
                (indent-region (point-min) (point-max)))))

(use-package emmet-mode
  :commands emmet-mode
  :init
  (progn
    (add-hook 'sgml-mode-hook #'emmet-mode)
    (add-hook 'css-mode-hook #'emmet-mode)
    (add-hook 'nxml-mode-hook #'emmet-mode)))

(use-package jinja2-mode
  :defer t)

(use-package rpm-spec-mode
  :defer t)

(use-package graphviz-dot-mode
  :mode "'\\.dot\\'"
  :config
  (setq graphviz-dot-view-command "dotty"))

(use-package gitignore-mode)

(use-package gitconfig-mode)

(use-package hgignore-mode)

(use-package vimrc-mode)

;;;
;;; Programming Modes
;;;

(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(use-package semantic
  :config
  (add-hook 'prog-mode-hook #'semantic-mode))

(defun my-maybe-make-executable ()
  (let ((file-name (buffer-file-name)))
    (when (and (save-excursion
                 (save-restriction
                   (widen)
                   (goto-char (point-min))
                   (looking-at-p "^#!")))
               (not (file-executable-p file-name)))
      (start-process "chmod" nil "chmod" "u+x" file-name))))
(add-hook 'after-save-hook #'my-maybe-make-executable)

(use-package fixme-mode
  :init
  (add-hook 'prog-mode-hook #'fixme-mode))

(use-package autopair
  :diminish autopair-mode
  :init
  (autopair-global-mode))

(use-package pretty-symbols)

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

;; Navigation in emacs lisp files
(defun my-add-sections-to-imenu ()
  (interactive)
  (add-to-list 'imenu-generic-expression
	       (list "Sections" "^;;; +\\(.+\\)" 1)))
(add-hook 'emacs-lisp-mode-hook #'my-add-sections-to-imenu)

(use-package cmake-mode)

(use-package cc-mode
  :defer
  :config
  (add-to-list 'c-default-style '(other . "k&r")))

(use-package geiser
  :config
  (setq geiser-repl-history-filename
        (convert-standard-filename
         (locate-user-emacs-file "geiser-history" ".geiser-history"))))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package python
  :mode ("\\.py\\'" . python-mode))

(use-package sphinx-doc
  :config
  (add-hook 'python-mode-hook #'sphinx-doc-mode))

(use-package jedi
  :init
  (progn
    ;; need to pip install epc and jedi
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:complete-on-dot t)))
;; TODO: in evil's insert state, map the normal autocomplete to jedi

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook #'anaconda-mode))

(use-package java-snippets)

(use-package jar-manifest-mode)

(use-package lua-mode )

(use-package restclient
  :config
  (use-package company-restclient))

;;;
;;; Custom macros
;;;

;; the following two functions are adapted from the similarly named
;; functions in prelude

(defalias 'my-kill-file-and-buffer 'my-delete-file-and-buffer)

(defun my-delete-file-and-buffer ()
  "Delete the current buffer and it's associated file."
  (interactive)
  (when (yes-or-no-p "Really kill buffer and file? ")
    (let ((file-name (buffer-file-name)))
      (when file-name
        (kill-buffer)
        (if (vc-backend file-name)
            (vc-delete-file)
          (delete-file file-name))
        (message "Deleted file %s" file-name)))))

(defun my-rename-file-and-buffer (new-name)
  "Rename the current file and buffer to NEW-NAME."
  (interactive "FNew name:")
  (let* ((old-name (buffer-file-name))
         (file-exists (and old-name (file-exists-p old-name))))
    (if file-exists
        (if (vc-backend old-name)
            ;; vc-rename-file takes care of renaming the buffer
            (vc-rename-file old-name new-name)
          (progn
            (rename-file old-name new-name old-name)
            ;; set-visited-file-name renames the buffer too
            (set-visited-file-name new-name t t)))
      (rename-buffer new-name))))

(defun twiddle-case (beg end)
  (interactive "r")
  (defun twiddle-case-call-on-region (beg end)
    (let ((string (buffer-substring-no-properties beg end))
          (deactivate-mark))
      (funcall (cond
                ((string-equal string (upcase string)) #'downcase-region)
                ((string-equal string (downcase string)) #'capitalize-region)
                (t #'upcase-region))
               beg end)))
  (if (use-region-p)
      (twiddle-case-call-on-region beg end)
    (twiddle-case-call-on-region (point) (1+ (point)))))

;; (define-key evil-normal-state-map "~" #'twiddle-case)

;; Test: RFC 1231

(defun my-rfc-lookup-text (rfc-num)
  "Look up an RFC using tools.ietf.org."
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
  "Look up an RFC using tools.ietf.org."
  (interactive (list
                (read-number "RFC number:" (thing-at-point 'number))))
  (let ((url (format "https://tools.ietf.org/html/rfc%s.txt" rfc-num)))
    (eww-browse-url url)))

;;; init.el ends here
