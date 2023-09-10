;;; checkers/jinx/config.el -*- lexical-binding: t; -*-

(use-package! jinx
  :init (global-jinx-mode)
  :custom
  (jinx-languages "en_GB")
  (jinx-include-modes '(text-mode prog-mode))
  (jinx-include-faces
   '((prog-mode font-lock-doc-face)
     (conf-mode font-lock-comment-face)))
  (jinx-exclude-regexps
   '((t "[A-Z]+\\>"
      "\\<[[:upper:]][[:lower:]]+\\>"
      "\\w*?[0-9\.'\"-]\\w*"
      "[a-z]+://\\S-+"
      "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?")))
  :bind
  (("M-$" . jinx-correct)))
