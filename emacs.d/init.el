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
; Packages
;

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalde" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;
; Basic UI
;

; evil - vi compatible keybindings
(require 'evil)
(evil-mode 1)

;
; Basic Configuration
;

(setq
 truncate-lines t ; line wrapping? wtf?
 comment-empty-lines t ; comment empty lines too
 indent-tabs-mode nil) ; tabs are evil

; show matching parenthesis
(show-paren-mode 1)

; show column numbers too
(column-number-mode 1)

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

;
; Color themes
;

(color-theme-solarized-dark)

;
; Navigation
;

(require 'ido)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(ido-mode 1)

(require 'ido-vertical-mode)
(ido-vertical-mode)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode)

(require 'imenu)
(require 'idomenu)
(global-set-key (kbd "C-c i") 'idomenu)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;
; Projects
;

(require 'projectile)
(projectile-global-mode)

;
; Snippets
;
(require 'yasnippet)
; custom snippets
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/")
(yas--initialize)


;
; Set up packages/settings for different modes
;

; text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

; org-mode
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)
(setq org-directory "~/notebook")
(add-to-list 'auto-mode-alist '("~/notebook/.*" . org-mode))
(setq org-default-notes-file (concat org-directory "/notes.org"))
 (setq org-todo-keywords
       '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

; markdown-mode
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

; ReStructured Text
(autoload 'rst-mode "rst mode" nil t)
(add-to-list 'auto-mode-alist '("\\.re?st$" . rst-mode))

; js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

; zencoding
(autoload 'zencoding-mode "zencoding mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html$" . zencoding-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))

(setq graphviz-dot-view-command "dotty")

;
; Custom macros
;
