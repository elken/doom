;;; ui/exwm/init.el -*- lexical-binding: t; -*-


(defconst IS-EXWM (not (null (member "--with-exwm" command-line-args))))
(add-to-list 'command-switch-alist '("--with-exwm" . (lambda (_) (pop command-line-args-left))))

(when (and doom-interactive-p
           (not doom-reloading-p)
           IS-EXWM)
  (require 'exwm)
  (exwm-enable))
