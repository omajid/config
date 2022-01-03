
;;; Commentary:

;; My user-init-file, with all my configuration

;;; Code:

(defun my-display-startup-time ()
  "Show startup time for Emacs."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'my-display-startup-time)

(defvar my-init-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold my-init-gc-cons-threshold)))

;;;
;;; Packages
;;;

(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
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
(setq use-package-verbose t)

(require 'use-package)

(use-package gnu-elpa-keyring-update)

;;;
;;; Looks
;;;

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode 0))
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(if (fboundp 'tooltip-mode) (tooltip-mode 0))
(setq
 inhibit-splash-screen t)

;; Some nice themes that I like
(use-package challenger-deep-theme)
(use-package doom-themes)
(use-package dracula-theme)
(use-package flatui-theme)
(use-package leuven-theme)
(use-package material-theme)
(use-package monokai-theme)
(use-package solarized-theme)
(use-package zenburn-theme)

(load-theme 'dracula t)

(set-frame-font "Source Code Pro-8.0" t)
(set-face-attribute 'fixed-pitch-serif nil :family "Source Code Pro")

(use-package all-the-icons
  :demand
  :init
  (unless (file-exists-p (expand-file-name "~/.local/share/fonts/all-the-icons.ttf"))
    (all-the-icons-install-fonts t))
  (use-package all-the-icons-ivy
    :demand
    :after (all-the-icons ivy)
    :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
    :config
    (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
    (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
    (all-the-icons-ivy-setup)))

(use-package diminish)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

;;;
;;; Keybindings
;;;

;; evil - vi compatible keybindings
(use-package evil
  :demand
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil ; needed by evil-collection
        evil-undo-system 'undo-fu)
  (add-hook 'git-commit-mode-hook #'evil-insert-state)
  :config
  (evil-set-initial-state 'project-explorer-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  ;; make * and # behave like vim: include _ and - in search
  (setq-default evil-symbol-word-search t)
  (evil-mode 1)

  ;; fix "gf" to not prompt
  (defun my-find-file-at-point ()
    (interactive)
    (require 'ffap)
    (let* ((guess (ffap-guesser))
           (filename (expand-file-name guess)))
      (find-file filename)))
  (evil-add-command-properties #'my-find-file-at-point :jump t)
  (evil-define-key nil evil-normal-state-map "gf" 'my-find-file-at-point))

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
  :commands which-key-mode
  :init
  (add-hook 'after-init-hook #'which-key-mode))

;;;
;;; Basic Configuration
;;;

;; Put custom definitions in another file and don't load it
(setq custom-file (locate-user-emacs-file "custom.el"))
;; (load custom-file)

;; dont jump screen on cursor scroll
(setq scroll-conservatively 101
      scroll-margin 5)

(setq-default
 indent-tabs-mode nil ; tabs are evil
 c-basic-offset 4)

(setq
 truncate-lines t ; line wrapping? wtf?
 comment-empty-lines t ; comment empty lines too
 tab-width 4)

;; single space after period ends sentence
(setq sentence-end-double-space nil)

(setq
 user-mail-address "omajid@redhat.com")

;; show matching parenthesis
(show-paren-mode 1)
;; by default, there’s a small delay before showing a matching parenthesis
(setq show-paren-delay 0)
;; highlight parenthesis when cursor is on either side of one
(setq show-paren-when-point-inside-paren t)

;; show column numbers too
(column-number-mode 1)

;; put all backups in one location instead of polluting all over the file system
(setq backup-directory-alist '((".*" . "~/.saves/"))
      backup-by-copying t
      delete-old-versions t)
;; same thing with put autosave files
(setq auto-save-file-name-transforms
      '((".*"  "~/.saves/" t)))
;; lock files must be placed in the current directory; I would rather not use them
(setq create-lockfiles nil)

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

(setq compilation-scroll-output t)

;;;
;;; Navigation
;;;

(use-package recentf
  :config
  (setq recentf-max-saved-items 1000))

(use-package smex
  :demand
  :config
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
  (setq mu4e-completing-read-function 'ivy-completing-read)
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
  (projectile-mode 1)

  (defun my-end-to-end-test ()
    "Run e2e test for project."
    (interactive)
    (let* ((project-name (projectile-project-name))
           (command
            (cond
             ((string= project-name "tachometer")
              "make check")
             (t (user-error (concat "Unknown project name: " project-name)))))
           (compilation-read-command nil))
      (projectile--run-project-cmd command
                                   nil
                                   :prompt-prefix "e2e command: "
                                   :save-buffers t)))

  (define-key projectile-command-map (kbd "`") #'my-end-to-end-test))

(use-package ag)

(use-package project-explorer
  :commands project-explorer-open)

(use-package magit
  :commands magit-status)

(use-package git-gutter
  :diminish ""
  :init
  (add-hook 'text-mode-hook #'git-gutter-mode)
  (add-hook 'prog-mode-hook #'git-gutter-mode)
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
    ("b" my-file-bug "bugs")
    ("e" mu4e "email")
    ("i" (find-file user-init-file) "init file")
    ("o" my-hydra-org/body "org")
    ("p" my-hydra-projects/body "projects")
    ("t" my-switch-or-create-ansi-term "terminal"))
  (defhydra my-hydra-org (:color blue)
    "Org"
    ("a" (org-agenda nil "n") "agenda")
    ("c" org-capture "capture")
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

(use-package ace-window
  :config
  (set-face-attribute 'aw-leading-char-face nil :height 4.0)
  :bind ("C-x o" . ace-window))

(use-package uniquify
  :ensure nil ;; builtin
  :config
  (progn
    (setq uniquify-buffer-name-style 'forward)))

;;;
;;; Common Modes
;;;

(use-package flyspell
  :ensure nil ;; builtin
  :diminish flyspell-mode
  :init
  (add-hook 'text-mode-hook #'turn-on-flyspell)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (use-package flycheck-package
    :after flycheck
    :init
    (add-hook 'after-init-hook #'flycheck-package-setup))
  (use-package flycheck-mypy)
  :config
  (setq flycheck-python-flake8-executable "python3"
        flycheck-python-pycompile-executable "python3"
        flycheck-python-pylint-executable "python3"))

(use-package company
  :demand
  :diminish company-mode
  :config
  (add-hook 'after-init-hook #'global-company-mode)
  (setq company-idle-delay 1
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t))

(use-package abbrev
  :ensure nil ;; builtin
  :diminish ""
  :init
  (add-hook 'after-init-hook #'abbrev-mode)
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

(use-package undo-fu)

(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package bug-reference
  :demand
  :config
  (progn
    (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
    (add-hook 'text-mode-hook #'bug-reference-mode)
    (add-hook 'rpm-spec-mode-hook #'bug-reference-mode)
    (setq bug-reference-bug-regexp
          "\\<\\(RFC\\|PR\\|JDK-\\|JEP-?\\|RH\\(?:BZ\\)\\|CVE-?\\|RHSA-?\\|RHBA-?\\|RHEA-?\\)\\(?:#\\| \\)?\\([0-9]+\\(?::?[-0-9]+\\)?\\)[[:space:]]")
    (setq bug-reference-url-format
          (lambda ()
            (let ((kind (upcase (match-string-no-properties 1)))
                  (id (match-string-no-properties 2)))
              (cond ((or (string-prefix-p "RHSA" kind) (string-prefix-p "RHBA" kind) (string-prefix-p "RHEA" kind))
                     (let ((kind (string-trim-right kind "-")))
                       (format "https://access.redhat.com/errata/%s-%s" kind id)))
                    ((or (string= kind "RH") (string= kind "RHBZ"))
                     (format "https://bugzilla.redhat.com/show_bug.cgi?id=%s" id))
                    ((string= kind "PR")
                     (format "http://icedtea.classpath.org/bugzilla/show_bug.cgi?id=%s" id))
                    ((string-prefix-p "JDK" kind)
                     (format "https://bugs.openjdk.java.net/browse/JDK-%s" id))
                    ((string-prefix-p "JEP" kind)
                     (format "http://openjdk.java.net/jeps/%s" id))
                    ((string-prefix-p "CVE" kind)
                     (format "http://www.cve.mitre.org/cgi-bin/cvename.cgi?name=%s" id))
                    ((or (string= "RFC" kind) (string= "rfc" kind))
                     (format "http://tools.ietf.org/html/rfc%s" id))
                    (t (error (concat "Unknown item type: " kind id)))))))))

(use-package crux)

(use-package hideshow
  :ensure nil ;; builtin
  :after evil
  :init
  (add-hook 'json-mode-hook #'hs-minor-mode)
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (define-key evil-normal-state-map (kbd "TAB") #'hs-toggle-hiding))

(use-package rainbow-mode
  :config
  (rainbow-mode))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'scheme-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode))

(use-package ws-butler
  :commands ws-butler-mode
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  (add-hook 'rpm-spec-mode-hook #'ws-butler-mode)
  (add-hook 'text-mode-hook #'ws-butler-mode))

;;;
;;; Mail
;;;

(defvar my-status-mail-recipient nil
  "Email address used for sending status mails.")

(defun my-get-file-contents (filename)
  "Get the contents of the file with FILENAME as one string."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-substring (point-min) (point-max))))

(use-package mu4e
  :ensure nil
  :demand
  :init
  (use-package org-mu4e
    :ensure nil
    :demand)
  (defun my-filter-contact-address (contact)
    "Don't complete email address to noreply@ or no-reply@"
    (if (string-match-p "noreply\\|no-reply\\|mojo-notify" contact)
        nil
      contact))
  (setq
   mail-user-agent 'mu4e-user-agent
   message-send-mail-function #'smtpmail-send-it
   mu4e-change-filenames-when-moving t
   mu4e-compose-signature (my-get-file-contents (expand-file-name "~/config/email-signature"))
   mu4e-contact-process-function #'my-filter-contact-address
   mu4e-drafts-folder "/[Gmail]/Drafts"
   mu4e-headers-include-related nil ; by default, it shows entire thread, even if the rest of the messages are in a different folder
   mu4e-index-update-in-background t
   mu4e-refile-folder "/[Gmail]/All Mail"
   mu4e-sent-folder "/[Gmail]/Sent Mail"
   mu4e-trash-folder "/[Gmail]/Trash"
   mu4e-update-interval (* 60 15)
   mu4e-view-show-addresses t
   mu4e-view-show-images t
   shr-color-visible-luminance-min 80) ; make html messages more legible on dark color schemes
  :config
  (set-face-attribute 'mu4e-header-highlight-face nil
                      :underline nil)

  (defun my-mu4e-get-mail-header (msg header-name)
    (let ((path (mu4e-message-field msg :path)))
      (my-mu4e-get-mail-header-path header-name path)))

  ;; from https://etienne.depar.is/emacs.d/mu4e.html
  (defun my-mu4e-get-mail-header-path (header-name path)
    (message "%s" path)
    (replace-regexp-in-string
     "[ \t\n]*$"
     ""
     (shell-command-to-string
      (concat "/usr/bin/sed -En '/^" header-name ":/I{:loop t;h;n;/^( |\\t)/{H;x;s/\\n//;t loop};x;p}' '" path "' | sed -n 's/^" header-name ": \\(.*\\)$/\\1/Ip'"))))

  (defun my-mu4e-view-async-download (&optional multi)
    "Offer to (download) urls(s) async. If MULTI (prefix-argument) is nil,
download a single one, otherwise, offer to fetch a range of
URLs. The urls are fetched to `mu4e-attachment-dir'."
    (interactive "P")
    (mu4e~view-handle-urls "URL to download" multi
                           (lambda (url)
                             (let ((filename (file-name-nondirectory (car (url-path-and-query (url-generic-parse-url url)))))
                                   (default-directory (mu4e~get-attachment-dir url)))
                               (when (file-exists-p filename)
                                 (signal 'file-already-exists (list "File already exists" filename)))
                               (start-process "mu4e-download" "*download*" "curl" url "-o" filename)))))
  (add-to-list 'mu4e-view-actions
               '("Download async" . my-mu4e-view-curl-download) t)
  (eval-after-load 'evil
    (evil-define-key 'normal mu4e-view-mode-map "gd" 'my-mu4e-view-async-download))

  (defun my-mu4e-unsubscribe-list (msg)
    ""
    (let* ((list-unsubscribe (my-mu4e-get-mail-header msg "list-unsubscribe"))
           (mailto-start (string-match "<mailto:" list-unsubscribe)))
      (if mailto-start
          (let* ((mailto-end (string-match ">" list-unsubscribe (+ 1 mailto-start)))
                 (mailto-address (substring list-unsubscribe (+ mailto-start 1) mailto-end)))
            (url-mailto (url-generic-parse-url mailto-address))
            (goto-char (point-min))
            (re-search-forward "--\n")
            (delete-region (point) (point-max)))
        (let* ((url-start (or (string-match "<" list-unsubscribe) 0))
               (url-end (or (string-match ">" list-unsubscribe url-start) (length list-unsubscribe)))
               (unsubscribe-url (substring list-unsubscribe url-start url-end)))
          (message "%s" unsubscribe-url)
          (browse-url unsubscribe-url)))))

  (add-to-list 'mu4e-view-actions
               '("xunsubscribe from mailing list" . my-mu4e-unsubscribe-list) t)

  (defun my-status-mail-from-org-block ()
    "Create a status report mail with a src block as the body."
    (interactive)
    (unless (org-in-block-p '("src"))
      (user-error "Not in a block"))
    (save-mark-and-excursion
      (org-babel-mark-block)
      (let ((subject (format-time-string "Status %F"))
            (recipient my-status-mail-recipient)
            (text (buffer-substring-no-properties (region-beginning) (region-end))))
        (url-mailto (url-generic-parse-url (concat "mailto:" recipient "?subject=" subject)))
        (goto-char (point-min))
        (search-forward "\n\n")
        (insert text)))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'my-redhat-mail-config)


;;;
;;; Text/Markup Editing Modes
;;;

(use-package org-mind-map
  :after org
  :init
  (require 'ox-org)
  :config
  (setq org-mind-map-engine "dot"))       ; Default. Directed Graph

(use-package adoc-mode
  :init
  :mode "\\.adoc\\'"
  :config
  (add-hook 'adoc-mode-hook #'turn-off-auto-fill))

(use-package diff-mode
  ;; files with '.patch.' in the middle of their name are patch files too
  :mode "\\.patch\\..*\\'")

(use-package docker)

(use-package dockerfile-mode
  :config
  (setq dockerfile-mode-command "podman"))

(use-package emmet-mode
  :commands emmet-mode
  :config
  (progn
    (add-hook 'sgml-mode-hook #'emmet-mode)
    (add-hook 'web-mode-hook #'emmet-mode)
    (add-hook 'css-mode-hook #'emmet-mode)
    (add-hook 'nxml-mode-hook #'emmet-mode)))

(use-package graphviz-dot-mode
  :mode "'\\.dot\\'"
  :config
  (setq graphviz-dot-view-command "dotty"))

(use-package gitignore-mode :defer t)

(use-package gitconfig-mode :defer t)

(use-package json-mode
  :config
  (setq js-indent-level 2))

(use-package markdown-mode
  :mode ("\\.md\\'"
         ("\\README.md\\'" . gfm-mode)))

(use-package markdown-toc)

(use-package org
  :config

  ;; Basics
  (setq org-directory (expand-file-name "~/notebook"))
  (defvar my-default-notes-file (concat org-directory "/daily-log.org"))

  (setq org-catch-invisible-edits 'error
        org-cycle-separator-lines 0
        org-src-fontify-natively t            ; fontify code in code blocks
        org-src-window-setup 'current-window  ; edit code in current window
        org-src-preserve-indentation t)       ; dont indent code when editing and exporting

  (setq org-startup-folded t)

  ;; Editing
  ;; (setq org-hide-leading-stars t)

  ;; TODO-related items
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(g)" "WAIT(w@/!)" "|" "DONE(d!)" "DELEGATED(l@)" "CANCELLED(c@)")))

  ;; Tags

  ;; Logging
  (setq org-log-done 'time)
  (setq org-log-states-order-reversed nil)

  ;; Capture and Refile

  (defun my-heading-of-new-tasks ()
    "The heading for next reporting day (Tuesday)."
    (concat "Week ending " (string-trim (shell-command-to-string "date -d 'next Tuesday' +%Y-%m-%d"))))
  (setq org-default-notes-file my-default-notes-file)
  (add-hook 'org-capture-mode-hook #'evil-insert-state)
  (setq org-capture-templates
        `(("t" "Todo" entry (file+olp org-default-notes-file ,(my-heading-of-new-tasks) "Tasks")
           "* TODO %?\n:PROPERTIES:\n:Created: %U\n:END:\n  %i\n  %a"
           :created t)))

  (setq org-refile-use-outline-path t)

  ;; Agenda
  (require 'org-agenda)
  ;; (setq org-agenda-inhibit-startup t)
  (setq org-agenda-dim-blocked-tasks nil)
  (add-hook 'org-agenda-mode-hook #'hl-line-mode)

  (setq org-agenda-files (list org-directory))
  (setq org-agenda-span 'fortnight)

  ;; show log mode by default
  ;; (setq org-agenda-start-with-log-mode t)

  (setq org-agenda-window-setup 'current-window)

  ;; don't show tasks that are scheduled or have deadlines in the
  ;; normal todo list
  (setq org-agenda-todo-ignore-deadlines 'all)
  (setq org-agenda-todo-ignore-scheduled 'all)

  ;;sort tasks in order of when they are due and then by priority
  (setq org-agenda-sorting-strategy
        '((agenda deadline-up priority-down)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep)))
  (setq org-deadline-warning-days 14)

  ;; specialized agenda for reporting

  ;; (add-to-list 'org-agenda-custom-commands
  ;;              '("W" "Weekly review"
  ;;                agenda ""
  ;;                ((org-agenda-span 8)
  ;;                 (org-agenda-start-day "-8d")
  ;;                 (org-agenda-start-on-weekday 2)
  ;;                 (org-agenda-start-with-log-mode t)
  ;;                 (org-agenda-skip-function
  ;;                  '(org-agenda-skip-entry-if 'nottodo 'done)))))

  (use-package htmlize)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)))

  ;; Export and Publishing

  )

(use-package pandoc-mode)

(use-package rpm-spec-mode)

(use-package simple
  :ensure nil ;; builtin
  :diminish auto-fill-function
  :init
  (add-hook 'text-mode-hook #'turn-on-auto-fill))


(use-package sgml-mode
  :config
  (advice-add 'sgml-delete-tag
              :after
              (lambda (arg)
                (indent-region (point-min) (point-max)))))

(use-package vimrc-mode :defer t)

(use-package web-mode
  :mode "\\.cshtml\\'")

(use-package writegood-mode
  :init
  (add-hook 'text-mode-hook #'writegood-mode))

(use-package writeroom-mode
  :init
  ;; git-gutter-mode messes with writeroom-mode's text-centering
  ;; https://github.com/joostkremers/writeroom-mode/issues/40
  (defun my-disable-git-gutter-mode ()
    "Disable git-gutter-mode."
    (when (fboundp 'git-gutter-mode)
      (git-gutter-mode 0)))
  :config
  (add-hook 'writeroom-mode-hook #'my-disable-git-gutter-mode))

(use-package yaml-mode)

;;;
;;; Programming Modes
;;;

(defun my-show-trailing-whitespace ()
  "Show trailing white-space."
  (setq show-trailing-whitespace 't))
(add-hook 'prog-mode-hook #'my-show-trailing-whitespace)

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(use-package elisp-mode
  :ensure nil
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

(defun my-csharp-mode-setup ()
  "Configure csharp-mode."
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)
  (local-set-key (kbd "C-c C-c") 'recompile)
  (evil-local-set-key 'normal (kbd "C-S-o") #'omnisharp-fix-usings)
  (evil-local-set-key 'insert (kbd "C-S-o") #'omnisharp-fix-usings))

(use-package csharp-mode
  :init
  ;; (add-hook 'csharp-mode-hook #'omnisharp-mode)
  (add-hook 'csharp-mode-hook #'my-csharp-mode-setup))

(use-package csproj-mode
  :init
  (add-hook 'csproj-mode-hook #'omnisharp-mode))

(use-package go-mode
  :init
  (defun my-go-mode-configure ()
    "Configure buffer for writing in golang."
    (setq indent-tabs-mode t
          tab-width 4))
  (add-hook 'go-mode-hook #'my-go-mode-configure)
  :config
  (setq godef-command "~/go/bin/godef"))

(use-package groovy-mode)

(use-package java-snippets)

(use-package jar-manifest-mode)

(use-package js2-mode
  :mode "\\.js\\'")

(use-package lsp-mode
  :hook
  ((go-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands
  (lsp lsp-deferred)
  :config
  (setq lsp-modeline-diagnostic-scope :project))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lua-mode)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :init
  (use-package pipenv))

(use-package restclient
  :commands restclient-mode
  :config
  (use-package company-restclient
    :demand
    :init
    (add-to-list 'company-backends #'company-restclient)))

(use-package typescript-mode
  :init
  (use-package tide
    :init
    (defun my-setup-tide-mode ()
      (tide-setup)
      (tide-hl-identifier-mode 1))
    (add-hook 'typescript-mode-hook #'my-setup-tide-mode)))

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

(defun my-virt-find-file (virt-name)
  "Run `find-file' in a virtual machine with name VIRT-NAME."
  (interactive "sVirtual Machine Name: ")
  (let* ((ip-addr (string-trim (shell-command-to-string (concat "virt-addr " (shell-quote-argument virt-name)))))
         (default-directory (concat "/ssh:" ip-addr ":/")))
    (call-interactively #'find-file)))

;;;
;;; Playground
;;;

(defvar my-bug-alist
  '(("IcedTea" . "http://icedtea.classpath.org/bugzilla/enter_bug.cgi?product=IcedTea")
    (".NET Core on Fedora" . "https://bugzilla.redhat.com/enter_bug.cgi?product=Fedora&component=dotnet3.1")
    (".NET Core on RHEL 7" . "https://bugzilla.redhat.com/enter_bug.cgi?product=dotNET")
    (".NET Core on RHEL 8" . "https://bugzilla.redhat.com/enter_bug.cgi?product=Red%20Hat%20Enterprise%20Linux%208&component=dotnet3.1")
    (".NET Core CoreCLR" . "https://github.com/dotnet/coreclr/issues/new/choose")
    (".NET Core CoreFx" . "https://github.com/dotnet/corefx/issues/new/choose")
    (".NET Core Runtime" . "https://github.com/dotnet/runtime/issues/new/choose")
    (".NET Core SDK" . "https://github.com/dotnet/sdk/issues/new/choose")
    (".NET Core source-build" . "https://github.com/dotnet/source-build/issues/new/choose")
    ("Thermostat" .  "http://icedtea.classpath.org/bugzilla/enter_bug.cgi?product=Thermostat")))

(defun my-file-bug ()
  "Pick a project and report a bug against it.

Uses `my-bug-alist' to select the bug."
  (interactive)
  (let* ((choice (completing-read "Bug against: " my-bug-alist))
         (url (cdr (assoc choice my-bug-alist))))
    (browse-url url)))

(defun my-blog-new-post (title)
  "Create a new blog post with TITLE."
  (interactive "sTitle:")
  (let* ((blog-root-dir (expand-file-name "~/devel/omajid.github.io/"))
         (default-directory blog-root-dir)
         (blog-post-dir (concat blog-root-dir "content/posts/"))
         (blog-post-filename (concat (format-time-string "%Y-%m-%d-") (string-join (split-string (downcase title)) "-") ".md"))
         (blog-post-file (concat blog-post-dir blog-post-filename))
         (hugo-program "hugo")
         (hugo-args (list "new" (concat "posts/" blog-post-filename))))
    (apply #'process-file hugo-program nil nil nil hugo-args)
    (find-file blog-post-file)))

(defun my-blog-last-post ()
  "Get to the last blog post."
  (interactive)
  (let* ((blog-post-dir (expand-file-name "~/devel/omajid.github.io/content/posts/"))
         (blog-files (directory-files blog-post-dir))
         ;; blog-files is sorted and my posts are named as YYYY-MM-DD
         ;; so the last item in the list is the latest post
         (last-blog-post (nth (- (safe-length blog-files ) 1) blog-files))
         (last-blog-post-file (concat blog-post-dir last-blog-post)))
    (find-file last-blog-post-file)))

(defun my-human-time-string-to-timestamp (human-time)
  "Find out the actual time (Lisp timestamp) from HUMAN-TIME.

human-time can be text like 'next month' or 'tomorrow'."
  (date-to-time (with-temp-buffer
                  (call-process "env" nil t nil "LC_ALL=C" "LANGUAGE=" "date" "-Rd" human-time)
                  (or (bobp) (delete-backward-char 1))
                  (buffer-string))))

;;; init.el ends here
