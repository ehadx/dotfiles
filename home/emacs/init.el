(use-package emacs
  :ensure nil
  :init
  (load-theme 'modus-vivendi)

  :config
  (setq custom-file "~/.emacs.d/custom.el")
  (load-file custom-file)
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
     (setq display-line-numbers 'relative)))

  (set-fontset-font t 'arabic "Noto Sans Arabic"))


(use-package dired
  :ensure nil
  :config
  (add-hook 'dired-mode-hook (lambda () (hl-line-mode t))))


(use-package ido
  :ensure nil
  :config
  (ido-mode t)
  (ido-everywhere t))


(use-package icomplete
  :ensure nil
  :init
  :config
  (icomplete-vertical-mode t)
  (fido-vertical-mode t))


(use-package org
  :ensure nil
  :config
  (visual-line-mode))

(use-package eglot
  :ensure t
  :init
  :config
    (add-hook 'java-mode-hook (lambda nil (eglot-ensure)))
    (add-hook 'python-mode-hook (lambda nil (eglot-ensure)))
    (add-hook 'csharp-mode-hook (lambda nil (eglot-ensure)))
    (setq eglot-connect-timeout 60)
    (setq
     eglot-server-programs
     '(
       ((js-mode typescript-mode) . (eglot-deno "deno" "lsp"))
       ((java-mode java-ts-mode) "~/opt/jvm/jdtls2/bin/jdtls" "-configuration" "~/.cache/jdtls" "-data" "/home/hadi/Projects/StockFX/workspace/stockfx" "-javaagent:/home/hadi/.m2/repository/org/projectlombok/lombok/1.18.34/lombok-1.18.34.jar")
       ((python-mode python-ts-mode) "pyright-langserver" "--stdio")
       ((csharp-mode csharp-ts-mode) "~/opt/omnisharp/OmniSharp" "-lsp")))

    (cl-defmethod eglot-execute-command
      (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
      "Eclipse JDT breaks spec and replies with edits as arguments."
      (mapc #'eglot--apply-workspace-edit arguments))

    (defclass eglot-deno (eglot-lsp-server) ()
      :documentation "A custom class for deno lsp.")

    (cl-defmethod eglot-initialization-options ((server eglot-deno))
      "Passes through required deno initialization options"
      (list :enable t
	    :lint t)))

(use-package evil
  :ensure t
  :defer t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-motion-state-modes '())
  (setq evil-insert-state-modes '())
  (setq evil-normal-state-modes '(prog-mode))
  (setq evil-visual-state-modes '()))

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
  (setq-default fsharp-indent-offset 2)
  (add-hook 'fsharp-mode-hook 'highlight-indentation-mode)
  (add-hook 'fsharp-mode-hook #'eglot-ensure))


(use-package eglot-fsharp
  :ensure t
  :after fsharp-mode
  :config
  (setq eglot-fsharp-server-install-dir nil))


(use-package pdf-tools
  :ensure t
  :defer t
  :commands (pdf-loader-install)
  :mode "\\.pdf\\'"
  :init
  (pdf-loader-install)
  :config
  (add-to-list 'revert-without-query ".pdf"))


(use-package csharp-mode :ensure t :defer t)
(use-package clojure-mode :ensure t :defer t)
(use-package typescript-mode :ensure t :defer t)
(use-package ef-themes :ensure t)
(use-package modus-themes :ensure t)
(use-package standard-themes :ensure t)
(use-package toml-mode :ensure t :defer t)
(use-package nix-mode :ensure t :defer t)
(use-package toml-mode :ensure t :defer t)
(use-package wgrep :ensure t)
(use-package magit :ensure t)
(use-package direnv :ensure t :config (direnv-mode))

(use-package nerd-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook (lambda () (nerd-icons-dired-mode))))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-support-imenu t)
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 4)
  (setq doom-modeline-hud nil)
  (setq doom-modeline-window-width-limit 70)
  (setq doom-modeline-project-detection 'auto)

  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   auto => emacs/l/comint.el (in a project) or comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   file-name-with-project => FOSS|comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  ;; If you are experiencing the laggy issue, especially while editing remote files
  ;; with tramp, please try `file-name' style.
  ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
  (setq doom-modeline-buffer-file-name-style 'auto)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-lsp-icon t)
  (setq doom-modeline-time-icon t)
  (setq doom-modeline-time-live-icon t)
  (setq doom-modeline-time-analogue-clock t)
  (setq doom-modeline-time-clock-size 0.7)
  (setq doom-modeline-unicode-fallback t)
  (setq doom-modeline-buffer-name t)
  (setq doom-modeline-highlight-modified-buffer-name t)
  (setq doom-modeline-column-zero-based t)
  (setq doom-modeline-percent-position '(-3 "%p"))
  (setq doom-modeline-position-line-format '("L%l"))
  (setq doom-modeline-position-column-format '("C%c"))
  (setq doom-modeline-position-column-line-format '("%l:%c"))
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (setq doom-modeline-buffer-encoding t)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-total-line-number nil)
  (setq doom-modeline-vcs-icon t)
  (setq doom-modeline-vcs-max-length 15)
  (setq doom-modeline-vcs-display-function #'doom-modeline-vcs-name)
  (setq doom-modeline-check-icon t)
  (setq doom-modeline-check-simple-format nil)
  (setq doom-modeline-number-limit 99)
  (setq doom-modeline-workspace-name t)
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-display-default-persp-name t)
  (setq doom-modeline-persp-icon t)
  (setq doom-modeline-lsp t)

  ;; Whether display the GitHub notifications. It requires `ghub' package.
  (setq doom-modeline-github nil)
  (setq doom-modeline-github-interval (* 30 60))
  (setq doom-modeline-modal t)

  ;; Whether display the modal state icon.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (setq doom-modeline-modal-icon nil)

  ;; Whether display the modern icons for modals.
  (setq doom-modeline-modal-modern-icon t)

  ;; When non-nil, always show the register name when recording an evil macro.
  (setq doom-modeline-always-show-macro-register nil)

  ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
  ;; (setq doom-modeline-mu4e nil)
  ;; also enable the start of mu4e-alert
  ;; (mu4e-alert-enable-mode-line-display)

  ;; Whether display the gnus notifications.
  (setq doom-modeline-gnus t)

  ;; Whether gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
  (setq doom-modeline-gnus-timer 2)

  ;; Wheter groups should be excludede when gnus automatically being updated.
  (setq doom-modeline-gnus-excluded-groups '("dummy.group"))

  ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
  ;; (setq doom-modeline-irc t)

  ;; Function to stylize the irc buffer names.
  (setq doom-modeline-irc-stylize 'identity)

  ;; Whether display the battery status. It respects `display-battery-mode'.
  (setq doom-modeline-battery t)

  ;; Whether display the time. It respects `display-time-mode'.
  (setq doom-modeline-time t)

  ;; Whether display the misc segment on all mode lines.
  ;; If nil, display only if the mode line is active.
  ;; (setq doom-modeline-display-misc-in-all-mode-lines t)

  ;; The function to handle `buffer-file-name'.
  (setq doom-modeline-buffer-file-name-function #'identity)

  ;; The function to handle `buffer-file-truename'.
  (setq doom-modeline-buffer-file-truename-function #'identity)

  ;; Whether display the environment version.
  (setq doom-modeline-env-version t)

  ;; Or for individual languages
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-ruby t)
  (setq doom-modeline-env-enable-perl t)
  (setq doom-modeline-env-enable-go t)
  (setq doom-modeline-env-enable-elixir t)
  (setq doom-modeline-env-enable-rust t)

  ;; Change the executables to use for the language version string
  (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
  (setq doom-modeline-env-ruby-executable "ruby")
  (setq doom-modeline-env-perl-executable "perl")
  (setq doom-modeline-env-go-executable "go")
  (setq doom-modeline-env-elixir-executable "iex")
  (setq doom-modeline-env-rust-executable "rustc")

  ;; What to display as the version while a new one is being loaded
  (setq doom-modeline-env-load-string "...")

  ;; By default, almost all segments are displayed only in the active window. To
  ;; display such segments in all windows, specify e.g.
  ;; (setq doom-modeline-always-visible-segments '(mu4e irc))

  ;; Hooks that run before/after the modeline version string is updated
  (setq doom-modeline-before-update-env-hook nil)
  (setq doom-modeline-after-update-env-hook nil))
