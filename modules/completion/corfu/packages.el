;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu
  :pin "24dccafeea114b1aec7118f2a8405b46aa0051e0"
  :recipe (:files (:defaults "extensions/*.el")))
(when (modulep! +icons)
  (package! kind-icon))
(when (modulep! +orderless)
  (package! orderless :pin "b24748093b00b37c3a572c4909f61c08fa27504f"))
(package! corfu-doc
  :recipe (:host github :repo "galeo/corfu-doc"))
(package! cape)
(package! yasnippet-capf
  :recipe (:host github :repo "elken/yasnippet-capf"))
(package! package-capf
  :recipe (:host github :repo "elken/package-capf"))

(when (modulep! :os tty)
  (package! popon
    :recipe (:type git :repo "https://codeberg.org/akib/emacs-popon"))
  (package! corfu-terminal
    :recipe (:type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
  (package! corfu-doc-terminal
    :recipe (:type git :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")))
