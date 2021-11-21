;;; ui/exwm/cli.el -*- lexical-binding: t; -*-

(defun desktop-template (&optional debug-p)
  (let ((script (if debug-p "start-debug.sh"
                  "start.sh"))
        (title (if debug-p "(Debug)"
                 "")))
    (format "[Desktop Entry]
Name=Doom EXWM %s
Comment=Doom-flavoured Emacs Window Manager
Exec=%s
TryExec=sh
Type=Application
DesktopNames=exwm
" title (doom-dir doom-private-dir "exwm" script))))

(defun script-template (&optional debug-p)
  (format "#!/usr/bin/env bash
export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/$(id -u)/bus
export _JAVA_AWT_WM_NOREPARENTING=1
wmname LG3D
emacs -mm --with-exwm %s"
          (if debug-p
              "--debug-init"
            "")))

(defcligroup! "EXWM"
  "For managing EXWM"
  (defcli! exwm-install ()
    "Install relevant files for EXWM"
    (let ((tramp-verbose-old 'tramp-verbose)
          (desktop (desktop-template))
          (desktop-debug (desktop-template t))
          (start-script (script-template))
          (start-debug-script (script-template t)))
      (setq tramp-verbose 0)

      (print! (start "Login manager setup"))
      (print-group!
       (unless (file-readable-p "/usr/share/xsessions/doom-exwm.desktop")
         (print! "Creating primary EXWM session")
         (with-current-buffer (find-file-noselect (doom--sudo-file-path "/usr/share/xsessions/doom-exwm.desktop"))
           (goto-char (point-max))
           (insert desktop)
           (save-buffer)))
       (print! (success "Primary session setup!"))

       (unless (file-readable-p "/usr/share/xsessions/doom-exwm-debug.desktop")
         (print! "Creating debug EXWM session")
         (with-current-buffer (find-file-noselect (doom--sudo-file-path "/usr/share/xsessions/doom-exwm-debug.desktop"))
           (goto-char (point-max))
           (insert desktop-debug)
           (save-buffer)))
       (print! (success "Debug session setup!")))
      (setq tramp-verbose tramp-verbose-old))


    (print! (start "Local script setup"))
    (when (not (file-directory-p (doom-dir doom-private-dir "exwm")))
      (print! "Creating %s" (doom-dir "exwm")))
    (make-directory (doom-dir doom-private-dir "exwm") 'parents)
    (mapc (lambda (file)
            (let ((filename (car file))
                  (body (cdr file)))
              (if (file-exists-p! filename (doom-dir doom-private-dir "exwm"))
                  (print! (warn "%s already exists, skipping") filename))))
          `(("start.sh" . ,(script-template))
            ("start-debug.sh" . ,(script-template t))))))
