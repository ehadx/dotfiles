(use-package emacs
  :ensure nil
  :init
  ;(load-theme 'modus-vivendi)

  :config
  (setq custom-file "~/.emacs.d/custom.el")
  (load-file custom-file)
  (load-file "~/.emacs.d/eglot-conf.el")
  ;(load-file "~/Projects/hy-lang-mode/hy-lang-mode.el")

  (column-number-mode t)
  (delete-selection-mode t)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq indent-line-function 'insert-tab)

  (unless (package-installed-p 'quelpa)
    (with-temp-buffer
      (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
      (eval-buffer)
      (quelpa-self-upgrade))
    (quelpa
     '(quelpa-use-package
       :fetcher git
       :url "https://github.com/quelpa/quelpa-use-package.git"))
    (require 'quelpa-use-package))

  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)

  (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
  (setq visible-bell t)

  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'package-menu-mode-hook (lambda () (hl-line-mode t)))

  (add-hook
   'prog-mode-hook
   (lambda nil
     (display-line-numbers-mode)
     (setq display-line-numbers 'relative)
     (outline-minor-mode 1)))

  (set-fontset-font t 'arabic "Noto Sans Arabic"))

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

(use-package icomplete
  :ensure nil
  :config
  (icomplete-vertical-mode t)
  (fido-vertical-mode t))

(use-package org
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (visual-line-mode t))))

(use-package evil
  :ensure t
  :defer t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-motion-state-modes '())
  (setq evil-insert-state-modes '())
  (setq evil-normal-state-modes '())
  (setq evil-visual-state-modes '())
  :config
  (evil-mode 1))

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

(use-package fsharp-mode
  :ensure t
  :defer t
  :config
  (require 'fsharp-mode)
  (setq-default fsharp-indent-offset 4)
  (add-hook 'fsharp-mode-hook 'highlight-indentation-mode)
  (add-hook 'fsharp-mode-hook #'eglot-ensure))

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
  :init
  (setq rust-mode-treesitter-derive nil)
  :config
  (setq rust-format-on-save t)
  ;; force the use of spaces instead of tabs
  (add-hook 'rust-mode-hook
	    (lambda () (setq indent-tabs-mode nil))))

(use-package markdown-mode :ensure t :defer t)
(use-package web-mode :ensure t :defer t)
(use-package hy-mode :ensure t :defer t)
(use-package zig-mode :ensure t :defer t)
(use-package csharp-mode :ensure t :defer t)
(use-package clojure-mode :ensure t :defer t)
(use-package typescript-mode :ensure t :defer t)
(use-package modus-themes :ensure t)
(use-package standard-themes :ensure t)
(use-package gruvbox-theme :ensure t)
(use-package ef-themes :ensure t)
(use-package gruber-darker-theme :ensure t)
(use-package toml-mode :ensure t :defer t)
(use-package nix-mode :ensure t :defer t)
(use-package toml-mode :ensure t :defer t)
(use-package wgrep :ensure t)
(use-package magit :ensure t)
(use-package direnv :ensure t :config (direnv-mode))
(use-package roc-ts-mode :ensure t :mode ("\\.roc\\'" . roc-ts-mode))
(use-package zig-ts-mode :ensure t)
(use-package hl-todo :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook (lambda () (nerd-icons-dired-mode))))
