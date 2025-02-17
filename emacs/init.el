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

(use-package emacs
  :ensure nil
  :bind (("C-, C-n" . ehadx/line-numbers-mode-rotate))
  :hook
  (before-save . delete-trailing-whitespace)
  (package-menu-mode . (lambda nil (hl-line-mode t)))
  (prog-mode . (lambda nil
                 (display-line-numbers-mode)
                 (setq display-line-numbers 'visual)))

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

  (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
  (setq visible-bell t)
  (set-fontset-font t 'arabic "Noto Sans Arabic"))

(use-package package
  :ensure nil
  :config
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
  (rust-ts-mode . ehadx/hs-init)
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
  (setq rust-mode-treesitter-derive t)
  :config
  (setq rust-format-on-save t)
  ;; force the use of spaces instead of tabs
  (add-hook 'rust-mode-hook
	        (lambda () (setq indent-tabs-mode nil))))

(use-package web-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.cshtml?\\'" . web-mode))

  (setq web-mode-engines-alist
        '(("razor" . "\\.cshtml\\'")))
  :hook
  (web-mode . (lambda nil (setq tab-width 2))))

(use-package markdown-mode :ensure t :defer t)
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
