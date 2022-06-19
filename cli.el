;;; cli.el -*- lexical-binding: t; -*-


(defcli! (publish) ()
  "Publish my doom config as a html page."
  (require 'core-start)
  (require 'ox-publish)
  (require 'htmlize)
  ;; Set theme needed for ox-chameleon
  (setq custom-enabled-themes `(,doom-theme))
  (print! (green "Starting publish\n"))

  ;; UI setup to properly load colours and include fontified minor modes
  (run-hooks 'doom-init-ui-hook)
  (add-hook 'htmlize-before-hook (lambda ()
                                   (rainbow-delimiters-mode-enable)
                                   (highlight-numbers--turn-on)
                                   (highlight-quoted--turn-on)
                                   (font-lock-flush)
                                   (font-lock-ensure)))

  ;; TODO Remove this and make engrave-faces work properly
  (after! engrave-faces
    (add-to-list 'engrave-faces-themes
                 '(doom-nord
                   (default :short "default" :slug "D" :family "Iosevka" :foreground "#ECEFF4" :background "#2E3440" :slant normal :weight normal)
                   (variable-pitch :short "var-pitch" :slug "vp" :family "Overpass")
                   (shadow :short "shadow" :slug "h" :foreground "#4C566A")
                   (success :short "success" :slug "sc" :foreground "#A3BE8C")
                   (warning :short "warning" :slug "w" :foreground "#EBCB8B")
                   (error :short "error" :slug "e" :foreground "#BF616A")
                   (link :short "link" :slug "l" :foreground "#81A1C1" :weight bold)
                   (link-visited :short "link" :slug "lv" :foreground "#ee82ee" :weight bold)
                   (highlight :short "link" :slug "hi" :foreground "#191C25" :background "#81A1C1")
                   (font-lock-comment-face :short "fl-comment" :slug "c" :foreground "#6f7787")
                   (font-lock-comment-delimiter-face :short "fl-comment-delim" :slug "cd" :foreground "#6f7787")
                   (font-lock-string-face :short "fl-string" :slug "s" :foreground "#A3BE8C")
                   (font-lock-doc-face :short "fl-doc" :slug "d" :foreground "#78808f")
                   (font-lock-doc-markup-face :short "fl-doc-markup" :slug "m" :foreground "#81A1C1")
                   (font-lock-keyword-face :short "fl-keyword" :slug "k" :foreground "#81A1C1")
                   (font-lock-builtin-face :short "fl-builtin" :slug "b" :foreground "#81A1C1")
                   (font-lock-function-name-face :short "fl-function" :slug "f" :foreground "#88C0D0")
                   (font-lock-variable-name-face :short "fl-variable" :slug "v" :foreground "#D8DEE9")
                   (font-lock-type-face :short "fl-type" :slug "t" :foreground "#8FBCBB")
                   (font-lock-constant-face :short "fl-constant" :slug "o" :foreground "#81A1C1")
                   (font-lock-warning-face :short "fl-warning" :slug "wr" :foreground "#EBCB8B")
                   (font-lock-negation-char-face :short "fl-neg-char" :slug "nc" :foreground "#81A1C1" :weight bold)
                   (font-lock-preprocessor-face :short "fl-preprocessor" :slug "pp" :foreground "#81A1C1" :weight bold)
                   (font-lock-regexp-grouping-construct :short "fl-regexp" :slug "rc" :foreground "#81A1C1" :weight bold)
                   (font-lock-regexp-grouping-backslash :short "fl-regexp-backslash" :slug "rb" :foreground "#81A1C1" :weight bold)
                   (org-block :short "org-block" :slug "obl" :background "#373E4C")
                   (org-block-begin-line :short "org-block-begin" :slug "obb" :foreground "#6f7787" :background "#373E4C")
                   (org-block-end-line :short "org-block-end" :slug "obe" :foreground "#6f7787" :background "#373E4C")
                   (outline-1 :short "o-1" :slug "oa" :foreground "#81A1C1" :weight bold)
                   (outline-2 :short "o-2" :slug "ob" :foreground "#B48EAD" :weight bold)
                   (outline-3 :short "o-3" :slug "oc" :foreground "#5D80AE" :weight bold)
                   (outline-4 :short "o-4" :slug "od" :foreground "#a0b8d0" :weight bold)
                   (outline-5 :short "o-5" :slug "oe" :foreground "#c6aac1" :weight bold)
                   (outline-6 :short "o-6" :slug "of" :foreground "#c0d0e0" :weight bold)
                   (outline-7 :short "o-7" :slug "og" :foreground "#d9c6d6" :weight bold)
                   (outline-8 :short "o-8" :slug "oh" :foreground "#e5ecf2" :weight bold)
                   (highlight-numbers-number :short "hl-number" :slug "hn" :foreground "#B48EAD" :weight bold)
                   (highlight-quoted-quote :short "hl-qquote" :slug "hq" :foreground "#81A1C1")
                   (highlight-quoted-symbol :short "hl-qsymbol" :slug "hs" :foreground "#8FBCBB")
                   (rainbow-delimiters-depth-1-face :short "rd-1" :slug "rda" :foreground "#81A1C1")
                   (rainbow-delimiters-depth-2-face :short "rd-2" :slug "rdb" :foreground "#B48EAD")
                   (rainbow-delimiters-depth-3-face :short "rd-3" :slug "rdc" :foreground "#A3BE8C")
                   (rainbow-delimiters-depth-4-face :short "rd-4" :slug "rdd" :foreground "#5D80AE")
                   (rainbow-delimiters-depth-5-face :short "rd-5" :slug "rde" :foreground "#8FBCBB")
                   (rainbow-delimiters-depth-6-face :short "rd-6" :slug "rdf" :foreground "#81A1C1")
                   (rainbow-delimiters-depth-7-face :short "rd-7" :slug "rdg" :foreground "#B48EAD")
                   (rainbow-delimiters-depth-8-face :short "rd-8" :slug "rdh" :foreground "#A3BE8C")
                   (rainbow-delimiters-depth-9-face :short "rd-9" :slug "rdi" :foreground "#5D80AE"))))

  (defun +org-publish-rename (props)
    "Things to do after the project finishes."
    (let ((out-dir (plist-get props :publishing-directory)))
      (when (file-exists-p (expand-file-name "config.html" out-dir))
        (rename-file
         (expand-file-name "config.html" out-dir)
         (expand-file-name "index.html" out-dir) t))))

  (setq org-html-validation-link nil
        org-html-head-include-scripts nil
        org-html-head-include-default-style nil
        org-html-divs '((preamble "header" "top")
                        (content "main" "content")
                        (postamble "footer" "postamble"))
        org-html-container-element "section"
        org-html-metadata-timestamp-format "%Y-%m-%d"
        org-html-checkbox-type 'html
        org-html-html5-fancy t
        org-html-htmlize-output-type 'css
        org-html-head-include-default-style nil
        ;; org-html-style-default (dotdoom-load-file "head-styles.html")
        org-html-head-include-scripts nil
        ;; org-html-scripts (dotdoom-load-file "head-scripts.html")
        org-html-doctype "html5"
        org-html-home/up-format "%s\n%s\n")

  (setq org-publish-project-alist
        '(("dotfiles"
           :base-directory "."
           :base-extension "org"
           :publishing-directory "out"
           :exclude "README.org"
           :publishing-function org-html-publish-to-html
           :completion-function +org-publish-rename
           :html-head-include-scripts nil
           :html-head-include-default-style nil
           :with-creator t)

          ("images"
           :base-directory "./images"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "out/images"
           :recursive t
           :publishing-function org-publish-attachment)

          ("static"
           :base-directory "./static"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "out/static"
           :recursive t
           :publishing-function org-publish-attachment)

          ("site" :components ("dotfiles" "static" "images"))))

  (org-publish-project "site" t))
