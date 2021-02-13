;;; ui/exwm/doctor.el -*- lexical-binding: t; -*-

(dolist (app '(("flameshot" . "screenshots")
               ("playerctl" . "song information")
               ("picom" . "transparency")))
  (when (not (executable-find (car app)))
    (print (format "%s is missing, %s won't work" (car app) (cdr app)))))
