;;; ui/exwm/doctor.el -*- lexical-binding: t; -*-

(dolist (app '(("flameshot" . "screenshots")
               ("playerctl" . "song information")
               ("wmname" . "some apps")
               ("feh" . "setting the background")
               ("picom" . "transparency")))
  (when (not (executable-find (car app)))
    (warn! (format "%s is missing, %s won't work" (car app) (cdr app)))))
