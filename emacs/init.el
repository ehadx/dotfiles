(defun ehadx/line-numbers-mode-rotate nil
  (interactive)

  (if (not display-line-numbers)
      (funcall (lambda nil ()
                 (display-line-numbers-mode)
                 (setq display-line-numbers 'visual)))
    (pcase display-line-numbers
      ('visual
       (setq display-line-numbers t))
      (t
       (display-line-numbers-mode -1)))))

(defun ehadx/c-mode-hook nil
  (interactive)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))

(use-package emacs
  :ensure nil
  :bind (("C-, C-n" . ehadx/line-numbers-mode-rotate))
  :hook
  (before-save . delete-trailing-whitespace)
  (package-menu-mode . (lambda nil (hl-line-mode t)))
  (prog-mode . (lambda nil
                 (display-line-numbers-mode)
                 (setq display-line-numbers 'visual)))
  (c-mode . (lambda nil (ehadx/c-mode-hook)))
  (cc-mode . (lambda nil (ehadx/c-mode-hook)))
  (c++-mode . (lambda nil (ehadx/c-mode-hook)))
  :init
  (load-theme 'gruvbox-dark-hard)
  (tool-bar-mode -1)

  :config
  (setq custom-file "~/.emacs.d/custom.el")
  (load-file custom-file)

  (setq indent-tabs-mode 'indent-relative)

  (column-number-mode t)
  (delete-selection-mode t)

  (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
  (setq visible-bell t)
  (set-fontset-font t 'arabic "Noto Sans Arabic"))

(use-package package
  :ensure nil
  :config
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t))

(use-package dired
  :ensure nil
  :config
  (add-hook 'dired-mode-hook (lambda () (hl-line-mode t))))

(use-package ido
  :ensure nil
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-auto-merge-delay-time 5))

(defun ehadx/hs-init nil
  (hs-minor-mode nil)
  ; (hs-hide-all)
  )

(use-package hideshow
  :ensure nil
  :bind (("C-, C-." . hs-toggle-hiding)
         ("C-, C-l" . hs-hide-level)
         ("C-, C-s" . hs-show-all))
  :hook
  (rust-mode . ehadx/hs-init)
  (emacs-lisp-mode . ehadx/hs-init))

(use-package icomplete
  :ensure nil
  :config
  (icomplete-vertical-mode t)
  (fido-vertical-mode t))

(use-package org
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (variable-pitch-mode t)
                             (visual-line-mode t)))

  ;; Make sure org-indent face is available
  (require 'org-indent)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package visual-fill-column
  :ensure t
  :init
  (setq-default visual-fill-column-center-text t)
  (setq-default visual-fill-column-width 100)
  (setq-default visual-fill-column-fringes-outside-margins t)
  (setq-default visual-fill-column-extra-text-width '(10 . 10))
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap Info-foto-emacs-command-node] . helpful-function))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 1))

(use-package pdf-tools
  :ensure t
  :defer t
  :commands (pdf-loader-install)
  :mode "\\.pdf\\'"
  :init
  (pdf-loader-install)
  :config
  (add-to-list 'revert-without-query ".pdf"))

(use-package rust-mode
  :ensure t
  :defer t
  :config
  (setq rust-mode-treesitter-derive t)
  (setq rust-format-on-save t)
  ;; force the use of spaces instead of tabs
  (add-hook 'rust-mode-hook
	        (lambda () (setq indent-tabs-mode nil))))

(use-package markdown-mode :ensure t :defer t)
(use-package zig-mode :ensure t :defer t)
(use-package csharp-mode :ensure t :defer t)
(use-package gruber-darker-theme :ensure t)
(use-package gruvbox-theme :ensure t)
(use-package nix-mode :ensure t :defer t)
(use-package wgrep :ensure t)
(use-package magit :ensure t)
