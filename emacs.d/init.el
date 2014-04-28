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
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalde" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

; auto-install packages
(let ((ensure-package-installed (lambda (package)
				  (if (not (package-installed-p package))
				      (package-install package))))
      (packages (list
		 'autopair
		 'cmake-mode
		 'emmet-mode
		 'evil
		 'flycheck
		 'flx-ido
		 'ido
		 'ido-vertical-mode
		 'ido-ubiquitous
		 'idomenu
		 'jedi
		 'markdown-mode
		 'org
		 'projectile 'dash
		 'python-mode
		 'rainbow-delimiters
		 'ruby-mode
		 'smex
		 'solarized-theme
		 'yasnippet)))
  (mapc ensure-package-installed packages))

;
; Basic UI
;

; evil - vi compatible keybindings
(require 'evil)
(evil-mode 1)
(defadvice ansi-term (after turn-off-evil-in-ansi-term ())
  "Disable whatever the fuck evil mode does in ansi-term"
  (turn-off-evil-mode))
(ad-activate 'ansi-term)

;
; Basic Configuration
;

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

(require 'flx-ido)
(flx-ido-mode 1)
;(setq ido-use-faces nil)

(require 'imenu)
(require 'idomenu)
(global-set-key (kbd "C-c i") 'idomenu)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(setq smex-key-advice-ignore-menu-bar t)

(require 'uniquify)
(setq uniquify-strip-comon-prefix t)
(setq uniquify-buffer-name-style 'forward)

;
; Projects
;

(require 'projectile)
(projectile-global-mode)

;
; Auto-Complete
;

(require 'auto-complete)
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
(global-auto-complete-mode)

;
; Snippets
;
(require 'yasnippet)
; custom snippets
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/")
(yas--initialize)

(require 'autopair)
(autopair-global-mode)

;
; Flycheck
;
(add-hook 'after-init-hook 'global-flycheck-mode)

;
; Set up packages/settings for different modes
;

; text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

; org-mode
(setq org-directory "~/notebook")
(add-to-list 'auto-mode-alist (cons (concat org-directory "/.*") 'org-mode))
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-log-states-order-reversed nil)
(setq org-agenda-files (list org-directory))

; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

; emmet
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

; python-mode
(add-hook 'python-mode-hook 'auto-complete-mode)
; need to pip install epc and jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'sphinx-doc-mode)
; TODO: in evil's insert state, map the normal autocomplete to jedi

; misc
(setq graphviz-dot-view-command "dotty")

;
; Color themes
;

(load-theme 'solarized-dark t)


;
; Custom macros
;
