;; Simple dmenu Implementation

(defvar exwm/dmenu-list
  '(("floorp" .
     "flatpak run --branch=stable --arch=x86_64 --command=floorp --file-forwarding one.ablaze.floorp")
    ("freetube" .
     "flatpak run --branch=stable --arch=x86_64 --command=/app/bin/run.sh --file-forwarding io.freetubeapp.FreeTube")
    ("flameshot" . "flameshot")
    ("dbeaver" .
     "flatpak run --branch=stable --arch=x86_64 --command=/app/bin/dbeaver io.dbeaver.DBeaverCommunity")
    ("pwvucontrol" . "pwvucontrol")))

(defvar exwm/apps (list))
(defvar exwm/apps-initialized nil)

(defun exwm/dmenu (cmd)
  (unless exwm/apps-initialized
    (dolist (app exwm/dmenu-list) (add-to-list 'exwm/apps (car app) t))
    (setq exwm/apps-initialized t))
  (interactive (list (completing-read "Choose command to run: " exwm/apps)))
  (setq cmd-to-run (alist-get cmd exwm/dmenu-list nil nil 'string-equal))
  (if (not cmd-to-run) (print "Not Supported")
    (start-process-shell-command cmd-to-run nil cmd-to-run)))

;; Volume Control with wpctl

(defun exwm/wpctl-vol-fast-up ()
  (interactive)
  (shell-command "wpctl set-volume @DEFAULT_AUDIO_SINK@ 10%+ && wpctl get-volume @DEFAULT_AUDIO_SINK@"))

(defun exwm/wpctl-vol-fast-down ()
  (interactive)
  (shell-command "wpctl set-volume @DEFAULT_AUDIO_SINK@ 10%- && wpctl get-volume @DEFAULT_AUDIO_SINK@"))

(defun exwm/wpctl-vol-slow-up ()
  (interactive)
  (shell-command "wpctl set-volume @DEFAULT_AUDIO_SINK@ 1%+ && wpctl get-volume @DEFAULT_AUDIO_SINK@"))

(defun exwm/wpctl-vol-slow-down ()
  (interactive)
  (shell-command "wpctl set-volume @DEFAULT_AUDIO_SINK@ 1%- && wpctl get-volume @DEFAULT_AUDIO_SINK@"))


;; Brightness Control with brightnessctl

(defun exwm/brightnessctl-fast-up ()
  (interactive)
  (shell-command "brightnessctl s 5%+"))

(defun exwm/brightnessctl-fast-down ()
  (interactive)
  (shell-command "brightnessctl s 5%-"))

(defun exwm/brightnessctl-slow-up ()
  (interactive)
  (shell-command "brightnessctl s 1%+"))

(defun exwm/brightnessctl-slow-down ()
  (interactive)
  (shell-command "brightnessctl s 1%-"))

;; Dunst Stuff

(defun exwm/dunst-history ()
  (interactive)
  (shell-command "dunstctl history-pop"))

(defun exwm/dunst-history-clear ()
  (interactive)
  (shell-command "dunstctl history-clear"))

;; Exwm Configuration

(defun exwm/init-hook ()
  (display-battery-mode 1)
  (setq display-time-day-and-date t)
  (display-time-mode 1))


(defun exwm/manage-finish-hook ()
  (interactive)
  (pcase exwm-class-name
    ("mpv"
     (exwm-floating-toggle-floating)
     (exwm-layout-hide-mode-line))
    ("pwvucontrol"
     (exwm-floating-toggle-floating)
     (exwm-layout-hide-mode-line))
    ("floorp"
     (pcase exwm-title
       ("Library"
	(exwm-floating-toggle-floating)
	(set-frame-width (exwm-get-selected-frame) 430)
	(set-frame-height (exwm-get-selected-frame 430))
       ))
    )))

(defun exwm/update-class-hook ()
  (exwm-workspace-rename-buffer exwm-class-name))


(defun exwm/include-title ()
  (exwm-workspace-rename-buffer
   (format
    "%s: %s" exwm-class-name
    (if (< (length exwm-title) 30)
	exwm-title
      (concat (substring exwm-title 0 30) "...")))))

(defun exwm/update-title-hook ()
  (pcase exwm-class-name
    ("floorp"
     (exwm/include-title))
    ("Freetube"
     (exwn/include-title))))

(use-package exwm
  :ensure t
  :config
  (require 'exwm)

  ;; Set the initial workspace number.
  (setq exwm-workspace-number 4)

  (require 'exwm-randr)
  (exwm-randr-mode)

  ;; Make class name the buffer name.
  (add-hook 'exwm-update-class-hook #'exwm/update-class-hook)

  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook #'exwm/manage-finish-hook)

  (add-hook 'exwm-init-hook #'exwm/init-hook)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'exwm/update-title-hook)

  ;; Automatically move EXWM buffer to current workspace when selected
  (setq exwm-layout-show-all-buffers nil)

  ;; Display all EXWM buffers in every workspace buffer list
  (setq exwm-workspace-show-all-buffers nil)

  ;; These keys should always pass through to Emacs
  (setq
   exwm-input-prefix-keys
   '(?\C-x
     ?\C-u
     ?\C-h
     ?\M-x
     ?\M-`
     ?\M-&
     ?\M-:
     ?\C-\M-j  ;; Buffer list
     ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Global keybindings.
  (setq exwm-input-global-keys
	`(([?\s-r] . exwm-reset) ;; s-r: Reset (to line-mode).
	  ([?\s-p] . exwm/dmenu)
	  ([?\s-n] . exwm/dunst-history)
	  ([?\s-N] . exwm/dunst-history-clear)

	  ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

	  (,(kbd "<XF86AudioLowerVolume>") . exwm/wpctl-vol-fast-down)
	  (,(kbd "<XF86AudioRaiseVolume>") . exwm/wpctl-vol-fast-up)
	  (,(kbd "<XF86MonBrightnessDown>") . exwm/brightnessctl-fast-down)
	  (,(kbd "<XF86MonBrightnessUp>") . exwm/brightnessctl-fast-up)
	  (,(kbd "<S-XF86AudioLowerVolume>") . exwm/wpctl-vol-slow-down)
	  (,(kbd "<S-XF86AudioRaiseVolume>") . exwm/wpctl-vol-slow-up)
	  (,(kbd "<S-XF86MonBrightnessDown>") . exwm/brightnessctl-slow-down)
	  (,(kbd "<S-XF86MonBrightnessUp>") . exwm/brightnessctl-slow-up)

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

  (setq exwm-systemtray-height 20)
  (exwm-enable)
  (exwm-systemtray-mode 1))


(use-package exwm-modeline
  :ensure t
  :after exwm
  :config
  (setq exwm-modeline-short t)
  (exwm-modeline-mode))
