;; -*- no-byte-compile: t; -*-
;;; lang/sql/packages.el

(package! sqlup-mode :pin "3f9df9c88d6a7f9b1ae907e401cad8d3d7d63bbf")
(when (modulep! +mysql)
  (package! mysql-to-org-mode :pin "c5eefc71200f2e1d0d67a13ed897b3cdfa835117"))
