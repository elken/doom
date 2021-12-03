;; -*- no-byte-compile: t; -*-
;;; ui/exwm/packages.el

(package! exwm)
(package! desktop-environment)
(package! doom-modeline-now-playing)
(package! doom-modeline-exwm
  :recipe (:local-repo "~/build/elisp/doom-modeline-exwm"
           :build (:not compile)))
