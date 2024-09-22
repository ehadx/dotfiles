(use-package emacs
  :ensure nil
  :init
  :config
  (set-scroll-bar-mode nil)
  (load-theme 'modus-vivendi)
  (column-number-mode nil)
  (delete-selection-mode nil)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq indent-line-function 'insert-tab)
  (setq display-line-numbers 'relative)
  (setq backup-directory-alist '(("." . "~/.emacs.d/emacs_saves")))
  (setq ring-bell-function 'ignore)
  (setq visible-bell 1)
  (add-hook 'prog-mode-hook (lambda nil (display-line-numbers-mode))))
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
    ;;(setq jdtls-path )
    ;;(setq jdtls-data-path )
    ;;(setq jdtls-javaagent )
    (setq eglot-server-programs '(
				  ((java-mode java-ts-mode) "~/opt/jvm/jdtls2/bin/jdtls" "-configuration" "~/.cache/jdtls" "-data" "/home/hadi/Projects/StockFX/workspace/stockfx" "-javaagent:/home/hadi/.m2/repository/org/projectlombok/lombok/1.18.34/lombok-1.18.34.jar")
				  ((python-mode python-ts-mode) "pyright-langserver" "--stdio")
				  ))
    (cl-defmethod eglot-execute-command
      (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
      "Eclipse JDT breaks spec and replies with edits as arguments."
      (mapc #'eglot--apply-workspace-edit arguments)))

(use-package evil
  :ensure t)
