;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu)
(when (featurep! +icons)
  (package! kind-icon))
(when (featurep! +orderless)
  (package! orderless))
(package! corfu-doc :recipe (:host github :repo "galeo/corfu-doc"))
(package! cape)
