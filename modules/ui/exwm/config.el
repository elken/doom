;;; ui/exwm/config.el -*- lexical-binding: t; -*-

;; Make the launcher only show app names
(use-package! counsel
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only))

(defun elken/playerctl-format (function format)
  "Invoke playerctl for FUNCTION using FORMAT to present output"
  (string-trim (shell-command-to-string (format "playerctl %s --format '%s'" function format))))

(defun elken/exwm-get-index (index)
  "Get the correct index from the passed index"
  (- index 1))

(defun elken/run-application (command)
  "Run the specified command as an application"
  (call-process "gtk-launch" nil 0 nil command))

(defun elken/exwm-update-class ()
  "Update the buffer to be the name of the window"
  (exwm-workspace-rename-buffer exwm-class-name))

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

(defun elken/exwm-update-title ()
  "Update the window title when needed"
  (pcase (downcase (or exwm-class-name ""))
    ("spotify" (exwm-workspace-rename-buffer (format "Spotify: %s" (elken/playerctl-format "--player=spotify metadata" "{{ artist }} - {{ title }}"))))
    (_ (exwm-workspace-rename-buffer (format "%s" exwm-title)))))

(defun elken/configure-window-by-class()
  "Configuration for windows (grouped by WM_CLASS)"
  (interactive)
  (pcase (downcase (or exwm-class-name ""))
    ("mpv" (progn
             (exwm-floating-toggle-floating)
             (exwm-layout-toggle-mode-line)))
    ("discord" (progn
                 (exwm-workspace-move-window (elken/exwm-get-index 3))
                 (elken/reload-tray)))
    ("megasync" (progn
                  (exwm-floating-toggle-floating)
                  (exwm-layout-toggle-mode-line)))
    ("spotify" (exwm-workspace-move-window (elken/exwm-get-index 4)))
    ("firefox" (exwm-workspace-move-window (elken/exwm-get-index 2)))))

(defun elken/exwm-init-hook ()
  "Various init processes for exwm"
  ;; Daemon applications
  (elken/run-in-background "pasystray")
  (elken/run-in-background "megasync")
  (elken/run-in-background "nm-applet")

  ;; Startup applications
  (elken/run-application "spotify")
  (elken/run-application "discord")
  (elken/run-application "firefox")

  ;; Default emacs behaviours
  ;; TODO Take this out of emacs
  (mu4e t))

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

(after! (exwm doom-modeline)
  (setq all-the-icons-scale-factor 1.1)
  (use-package! doom-modeline-now-playing
    :config
    (doom-modeline-now-playing-timer))
  (doom-modeline-def-segment exwm-buffer-info
    (concat
     (let ((face (if (doom-modeline--active)
                     'doom-modeline-buffer-file
                   'mode-line-inactive)))
       (doom-modeline-icon 'octicon "browser" "" ""
                           :face face :v-adjust -0.05))
     (doom-modeline-spc)
     (doom-modeline--buffer-name)))
  (doom-modeline-def-segment exwm-workspaces
    (exwm-workspace--update-switch-history)
    (concat
     (doom-modeline-spc)
     (elt (let* ((num (exwm-workspace--count))
                 (sequence (number-sequence 0 (1- num)))
                 (not-empty (make-vector num nil)))
            (dolist (i exwm--id-buffer-alist)
              (with-current-buffer (cdr i)
                (when exwm--frame
                  (setf (aref not-empty
                              (exwm-workspace--position exwm--frame))
                        t))))
            (mapcar
             (lambda (i)
               (mapconcat
                (lambda (j)
                  (format (if (= i j) "[%s]" " %s ")
                          (propertize
                           (apply exwm-workspace-index-map (list j))
                           'face
                           (cond ((frame-parameter (elt exwm-workspace--list j)
                                                   'exwm-urgency)
                                  '(:inherit warning :weight bold))
                                 ((= i j) '(:inherit underline :weight bold))
                                 ((aref not-empty j) '(:inherit success :weight bold))
                                 (t `((:foreground ,(face-foreground 'mode-line-inactive))))))))
                sequence ""))
             sequence))
          (exwm-workspace--position (selected-frame)))))
  (setf (alist-get 'exwm-mode all-the-icons-mode-icon-alist)
        '(all-the-icons-octicon "browser" :v-adjust -0.05))
  (doom-modeline-def-modeline 'exwm
    '(bar workspace-name exwm-workspaces debug exwm-buffer-info)
    '(now-playing objed-state misc-info persp-name grip mu4e gnus github repl lsp major-mode process "  "))
  (defun doom-modeline-set-exwm-modeline ()
    "Set exwm mode-line"
    (doom-modeline-set-modeline 'exwm))
  (add-hook 'exwm-mode-hook #'doom-modeline-set-exwm-modeline)
  (doom-modeline-def-modeline 'main
    '(bar workspace-name exwm-workspaces debug modals matches buffer-info remote-host parrot selection-info)
    '(now-playing objed-state misc-info persp-name grip mu4e gnus github repl lsp major-mode process vcs checker "  ")))

;; Used to handle screen locking (currently unused), media keys and screenshotting
(use-package! desktop-environment
  :after exwm
  :config
  (setq desktop-environment-screenshot-command "flameshot gui")
  (desktop-environment-mode))

;; The meat and potatoes as they say
(use-package! exwm
  :commands (exwm-enable)
  :config
  ;; Enabled debugging when doom is in debug mode
  (when doom-debug-p
    (advice-add #'exwm-input--on-buffer-list-update :before
                (lambda (&rest r)
                (exwm--log "CALL STACK: %s" (cddr (reverse (xcb-debug:-call-stack))))))
    (exwm-debug))

  ;; Show all buffers for switching
  (setq exwm-workspace-show-all-buffers t)

  ;; Set a sane number of default workspaces
  (setq exwm-workspace-number 5)

  ;; Define workspace setup for monitors
  (setq exwm-randr-workspace-monitor-plist `(,(elken/exwm-get-index 2) "DP-0" ,(elken/exwm-get-index 3) "DP-0"))

  (setq exwm-workspace-index-map
        (lambda (index) (number-to-string (+ 1 index))))

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
          ?\C-w
          ?\M-`
          ?\M-&
          ?\M-:
          ?\M-\
          ?\C-g
          ?\C-\M-j
          ?\C-\ ))

  ;; Shortcut to passthrough next keys
  (map! :map exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Setup screen layout
  (require 'exwm-randr)
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil "xrandr --output HDMI-0 --primary --mode 2560x1440 --pos 0x1080 --output DP-0 --mode 2560x1080 --pos 0x0")

  ;; Set the wallpaper
  (elken/set-wallpaper (expand-file-name "images/background.png" doom-private-dir))

  ;; Setup tray
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 16)
  (exwm-systemtray-enable)

  ;; Date/time
  (setq display-time-format " [ %H:%M %d/%m/%y]")
  (setq display-time-default-load-average nil)
  (display-time-mode 1)

  (exwm-input-set-key (kbd "<s-return>") '+eshell/toggle)

  (setq exwm-input-global-keys
        '(
          ([?\s- ] . counsel-linux-app)
          ([?\s-r] . exwm-reset)
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ([?\s-&] . (lambda (command) (interactive (list (read-shell-command "[command] $ ")))
                       (start-process-shell-command command nil command)))

          ([?\s-e] . (lambda () (interactive) (dired "~")))

          ([?\s-w] . exwm-workspace-switch)

          ([?\s-Q] . (lambda () (interactive) (kill-buffer)))
          ([?\s-1] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create (elken/exwm-get-index 1))))
          ([?\s-2] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create (elken/exwm-get-index 2))))
          ([?\s-3] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create (elken/exwm-get-index 3))))
          ([?\s-4] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create (elken/exwm-get-index 4))))
          ([?\s-5] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create (elken/exwm-get-index 5))))
          ([?\s-6] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create (elken/exwm-get-index 6))))
          ([?\s-7] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create (elken/exwm-get-index 7))))
          ([?\s-8] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create (elken/exwm-get-index 8))))
          ([?\s-9] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create (elken/exwm-get-index 9)))))))
