(use-package emacs
  :ensure nil
  :init
  :config
  (setq custom-file "~/.emacs.custom.el")
  (load-file custom-file)
  (set-scroll-bar-mode nil)
  (setq inhibit-startup-screen nil)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (load-theme 'modus-vivendi)
  (column-number-mode nil)
  (delete-selection-mode nil)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq indent-line-function 'insert-tab)
  (require 'package)
  (add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (setq backup-directory-alist '(("." . "~/.emacs.d/emacs_saves")))
  (setq ring-bell-function 'ignore)
  (setq visible-bell 1)
  (add-hook 'prog-mode-hook (lambda nil
			      (display-line-numbers-mode)
			      (setq display-line-numbers 'relative)
			      )))
  ;; Font Configuration ----------------------------------------------------------
  ;; (set-face-attribute 'default nil
  ;;		    :font "JetBrains Mono"
  ;;		    :height 120)

  ;; Set the fixed pitch face
  ;; (set-face-attribute 'fixed-pitch nil
  ;; 		    :font "JetBrains Mono"
  ;; 		    :height 120)

  ;; ;; Set the variable pitch face
  ;; (set-face-attribute 'variable-pitch nil
  ;; 		    :font "DejaVu Sans"
  ;; 		    :height 140
  ;; 		    :weight 'regular)

  ;; (set-fontset-font t 'arabic "Noto Kufi Arabic")


(use-package icomplete
  :ensure nil
  :init
  :config
  (icomplete-vertical-mode nil)
  (fido-vertical-mode nil))


(use-package eglot
  :ensure nil
  :init
  :config
    (add-hook 'java-mode-hook (lambda nil (eglot-ensure)))
    (add-hook 'python-mode-hook (lambda nil (eglot-ensure)))
    (add-hook 'csharp-mode-hook (lambda nil (eglot-ensure)))
    (setq eglot-connect-timeout 60)
    (setq eglot-server-programs '(
				  ((java-mode java-ts-mode) "~/opt/jvm/jdtls2/bin/jdtls" "-configuration" "~/.cache/jdtls" "-data" "/home/hadi/Projects/StockFX/workspace/stockfx" "-javaagent:/home/hadi/.m2/repository/org/projectlombok/lombok/1.18.34/lombok-1.18.34.jar")
				  ((python-mode python-ts-mode) "pyright-langserver" "--stdio")
				  ((csharp-mode csharp-ts-mode) "~/opt/omnisharp/OmniSharp" "-lsp")
				  ))
    (cl-defmethod eglot-execute-command
      (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
      "Eclipse JDT breaks spec and replies with edits as arguments."
      (mapc #'eglot--apply-workspace-edit arguments)))


(use-package evil
  :ensure nil)


(use-package exwm
  :ensure nil
  :config
  (require 'exwm)
  ;; Set the initial workspace number.
  (setq exwm-workspace-number 4)
  ;; Make class name the buffer name.
  (add-hook 'exwm-update-class-hook
	    (lambda () (exwm-workspace-rename-buffer exwm-class-name)))
  ;; Global keybindings.
  (setq exwm-input-global-keys
	`(([?\s-r] . exwm-reset) ;; s-r: Reset (to line-mode).
          ([?\s-w] . exwm-workspace-switch) ;; s-w: Switch workspace.
          ([?\s-&] . (lambda (cmd) ;; s-&: Launch application.
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command cmd nil cmd)))
          ;; s-N: Switch to certain workspace.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
			(lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))
  ;; Enable EXWM
  (exwm-enable))
