(setq inhibit-startup-message t)
(set-scroll-bar-mode nil)

(setq tab-width 2)
(setq c-basic-offset 2)
(setq indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)

(setq column-number-mode t)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook (lambda ()
			    (display-line-numbers-mode)))

(setq backup-directory-alist '(("." . "~/.emacs.d/emacs_saves")))

;; Font Configuration ----------------------------------------------------------
(set-face-attribute 'default nil
		    :font "JetBrains Mono"
		    :height 120)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
		    :font "JetBrains Mono"
		    :height 120)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
		    :font "DejaVu Sans"
		    :height 140
		    :weight 'regular)

(set-fontset-font t 'arabic "Noto Kufi Arabic")

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))

(load-theme 'wombat)

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap Info-foto-emacs-command-node] . helpful-function)
  :config
  (global-set-key (kbd "C-c C-d") #'helpful-at-point))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 1))

(use-package csharp-mode)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (csharp-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company :commands company-mode)

(setq org-support-shift-select t)
(defun hadi/org-mode-setup ()
  (org-indent-mode 1)
  ;; (variable-pitch-mode 1)
  (visual-line-mode 1)
  (keymap-global-set "C-c o l" #'org-store-link)
  (keymap-global-set "C-c o g" #'org-agenda)
  (keymap-global-set "C-c o c" #'org-capture)
  (setq-local electric-indent-mode -1))

(add-hook 'org-mode-hook (lambda () (hadi/org-mode-setup)))
