(require 'subr-x)
;; Make the launcher only show app names
(use-package! counsel
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only))

(defun elken/playerctl-format (function format)
  "Invoke playerctl for FUNCTION using FORMAT to present output"
  (string-trim (shell-command-to-string (format "playerctl %s --format '%s'" function format))))

(defun elken/exwm-update-class ()
  "Update the buffer to be the name of the window"
  (exwm-workspace-rename-buffer exwm-class-name))

(defun elken/exwm-update-title ()
  "Update the window title when needed"
  (pcase (downcase (or exwm-class-name ""))
    ("firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))
    ("spotify" (exwm-workspace-rename-buffer (format "Spotify: %s" (elken/playerctl-format "--player=spotify metadata" "{{ artist }} - {{ title }}"))))))

(defun elken/configure-window-by-class()
  "Configuration for windows (grouped by WM_CLASS)"
  (interactive)
  (pcase (downcase (or exwm-class-name ""))
    ("mpv" (exwm-floating-toggle-floating)
     (exwm-layout-toggle-mode-line))
    ("discord" (exwm-workspace-move-window 3)
     (elken/reload-tray))
    ("megasync" (exwm-floating-toggle-floating)
     (exwm-layout-toggle-mode-line))
    ("spotify" (exwm-workspace-move-window 4))
    ("firefox" (exwm-workspace-move-window 2))))

(after! (exwm doom-modeline)
  ;; Rudimentary version of my now-playing segment
  (defvar now-playing--timer nil)
  (defvar now-playing-text '())
  (defun now-playing-fetch ()
    (async-start
     (lambda ()
       (require 'subr-x)
       (defun elken/playerctl-format (function format)
         "Invoke playerctl for FUNCTION using FORMAT to present output"
         (string-trim (shell-command-to-string (format "playerctl --player=spotify %s --format '%s'" function format))))
       (let ((is-playingp (equal "playing" (elken/playerctl-format "status" "{{ lc(status) }}")))
             (player-icon (if (equal "spotify" (elken/playerctl-format "metadata" "{{ playerName }}")) "" "")))
         (concat
          player-icon
          "|"
          (if is-playingp "" "")
          "|"
          (elken/playerctl-format "metadata" "{{ artist }} - {{ title }}") )))
     (lambda (result)
       (message "")
       (setq now-playing-text (split-string result "|")))))
  (defun now-playing-timer ()
    (if (timerp now-playing--timer)
        (cancel-timer now-playing--timer))
    (setq now-playing--timer
          (run-with-timer 5 5 #'now-playing-fetch)))
  (now-playing-timer)
  (doom-modeline-def-segment now-playing
    (let ((player (elt now-playing-text 0))
          (status (elt now-playing-text 1))
          (text (elt now-playing-text 2)))
      (concat
       (propertize player 'face 'success)
       (doom-modeline-spc)
       status
       (doom-modeline-spc)
       (propertize (truncate-string-to-width text 40 nil nil "...") 'face 'bold))))
  (doom-modeline-def-segment exwm-workspaces
    (exwm-workspace--update-switch-history)
    (concat
     (doom-modeline-spc)
     (elt exwm-workspace--switch-history (exwm-workspace--position (selected-frame)))))
  (doom-modeline-def-modeline 'main
    '(bar workspace-name exwm-workspaces modals matches buffer-info remote-host parrot selection-info buffer-position)
    '(now-playing objed-state misc-info persp-name grip mu4e gnus github debug repl lsp minor-modes major-mode process vcs checker)))

(defun elken/run-application (command)
  "Run the specified command as an application"
  (call-process "gtk-launch" nil 0 nil command))

(defvar elken/process-alist '())

(defun elken/kill-process--action (process)
  "Do the actual process killing"
  (when process
    (ignore-errors
      (kill-process (cdr process))))
  (setq elken/process-alist (remove process elken/process-alist)))

(defun elken/kill-process ()
  "Kill a background process"
  (interactive)
  (ivy-read "Kill process: " elken/process-alist
            :action #'elken/kill-process--action
            :caller 'elken/kill-process))

(defun elken/run-in-background (command &optional args)
  "Run the specified command as a daemon"
  (elken/kill-process--action (assoc command elken/process-alist))
  (setq elken/process-alist
        (cons `(,command . ,(start-process-shell-command command nil (format "%s %s" command (or args "")))) elken/process-alist)))

(defun elken/reload-tray ()
  "Restart the systemtray"
  (interactive)
  (exwm-systemtray--exit)
  (exwm-systemtray--init))

(defun elken/set-wallpaper (file)
  "Set the DE wallpaper to be $FILE"
  (interactive)
  (start-process-shell-command "feh" nil (format "feh --bg-scale %s" file)))

(defun elken/exwm-init-hook ()
  "Various init processes for exwm"
  ;; Daemon applications
  (elken/run-in-background "pasystray")
  (elken/run-in-background "megasync")
  (elken/run-in-background "nm-applet")

  ;; Startup applications
  (elken/run-application "discord")
  (elken/run-application "firefox")

  ;; Default emacs behaviours
  (exwm-workspace-switch-create 1)
  (eshell))

(use-package! desktop-environment
  :after exwm
  :config
  (setq desktop-environment-screenlock-command "gnome-screensaver-command -l")
  (desktop-environment-mode))

(use-package! exwm
  :config
  ;; Show all buffers for switching
  (setq exwm-workspace-show-all-buffers t)

  ;; Set a sane number of default workspaces
  (setq exwm-workspace-number 10)

  ;; Define workspace setup for monitors
  (setq exwm-randr-workspace-monitor-plist '(2 "DP-0" 3 "DP-0"))

  (setq exwm-workspace-index-map
        (lambda (index) (number-to-string index)))

  ;; Set the buffer to the name of the window class
  (add-hook 'exwm-update-class-hook #'elken/exwm-update-class)

  ;; Init hook
  (add-hook 'exwm-init-hook #'elken/exwm-init-hook)

  ;; Update window title
  (add-hook 'exwm-update-title-hook #'elken/exwm-update-title)

  ;; Configure windows as created
  (add-hook 'exwm-manage-finish-hook #'elken/configure-window-by-class)

  ;; /Always/ pass these to emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j
          ?\C-\ ))

  ;; Shortcut to passthrough next keys
  (map! :map exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Setup screen layout
  (require 'exwm-randr)
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil "sh ~/.screenlayouts/default.sh")

  ;; Set the wallpaper
  (elken/set-wallpaper "~/Pictures/FFVIIR_Midgar.png")

  ;; Setup tray
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 16)
  (exwm-systemtray-enable)

  ;; Date/time
  (setq display-time-format " [ %H:%M %d/%m/%y]")
  (setq display-time-default-load-average nil)
  (display-time-mode 1)

  (setq exwm-input-global-keys
        '(
          ([?\s- ] . counsel-linux-app)
          ([?\s-r] . exwm-reset)
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ([?\s-&] . (lambda (command) (interactive (list (read-shell-command "> ")))
                       (start-process-shell-command command nil command)))

          ([?\s-w] . exwm-workspace-switch)

          ([?\s-Q] . (lambda () (interactive) (kill-buffer)))
          ([?\s-`] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 0)))
          ([?\s-1] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 1)))
          ([?\s-2] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 2)))
          ([?\s-3] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 3)))
          ([?\s-4] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 4)))
          ([?\s-5] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 5)))
          ([?\s-6] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 6)))
          ([?\s-7] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 7)))
          ([?\s-8] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 8)))
          ([?\s-9] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 9)))))
  (exwm-enable))
