;;; cli.el -*- lexical-binding: t; -*-


(defcli! (publish) ()
  "Publish my doom config as a html page."
  (require 'doom-start)
  (require 'ox-publish)
  (require 'htmlize)
  (require 'engrave-faces)

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
                   (default :short "default" :slug "D" :family "Iosevka Nerd Font" :foreground "#ECEFF4" :background "#2E3440" :slant normal :weight regular)
                   (variable-pitch :short "var-pitch" :slug "vp" :family "Montserrat")
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
                   (org-block :short "org-block" :slug "ob" :background "#373E4C")
                   (org-block-begin-line :short "org-block-begin" :slug "obb" :foreground "#6f7787" :background "#373E4C")
                   (org-block-end-line :short "org-block-end" :slug "obe" :foreground "#6f7787" :background "#373E4C")
                   (outline-1 :short "outline-1" :slug "Oa" :foreground "#81A1C1" :weight bold)
                   (outline-2 :short "outline-2" :slug "Ob" :foreground "#B48EAD" :weight bold)
                   (outline-3 :short "outline-3" :slug "Oc" :foreground "#5D80AE" :weight bold)
                   (outline-4 :short "outline-4" :slug "Od" :foreground "#a0b8d0" :weight bold)
                   (outline-5 :short "outline-5" :slug "Oe" :foreground "#c6aac1" :weight bold)
                   (outline-6 :short "outline-6" :slug "Of" :foreground "#c0d0e0" :weight bold)
                   (outline-7 :short "outline-7" :slug "Og" :foreground "#d9c6d6" :weight bold)
                   (outline-8 :short "outline-8" :slug "Oh" :foreground "#e5ecf2" :weight bold)
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
                   (rainbow-delimiters-depth-9-face :short "rd-9" :slug "rdi" :foreground "#5D80AE")
                   (ansi-color-yellow :short "ansi-yellow" :slug "any" :foreground "#EBCB8B" :background "#EBCB8B")
                   (ansi-color-red :short "ansi-red" :slug "anr" :foreground "#BF616A" :background "#BF616A")
                   (ansi-color-black :short "ansi-black" :slug "anb" :foreground "#2E3440")
                   (ansi-color-green :short "ansi-green" :slug "ang" :foreground "#A3BE8C" :background "#A3BE8C")
                   (ansi-color-blue :short "ansi-blue" :slug "anB" :foreground "#81A1C1" :background "#81A1C1")
                   (ansi-color-cyan :short "ansi-cyan" :slug "anc" :foreground "#88C0D0" :background "#88C0D0")
                   (ansi-color-white :short "ansi-white" :slug "anw" :background "#ECEFF4")
                   (ansi-color-magenta :short "ansi-magenta" :slug "anm" :foreground "#B48EAD" :background "#B48EAD")
                   (ansi-color-bright-yellow :short "ansi-bright-yellow" :slug "ANy" :foreground "#edd29c" :background "#edd29c")
                   (ansi-color-bright-red :short "ansi-bright-red" :slug "ANr" :foreground "#c87880" :background "#c87880")
                   (ansi-color-bright-black :short "ansi-bright-black" :slug "ANb" :foreground "#191C25" :background "#2C333F")
                   (ansi-color-bright-green :short "ansi-bright-green" :slug "ANg" :foreground "#b0c79d" :background "#b0c79d")
                   (ansi-color-bright-blue :short "ansi-bright-blue" :slug "ANB" :foreground "#93afca" :background "#93afca")
                   (ansi-color-bright-cyan :short "ansi-bright-cyan" :slug "ANc" :foreground "#99c9d7" :background "#99c9d7")
                   (ansi-color-bright-white :short "ansi-bright-white" :slug "ANw" :foreground "#F0F4FC" :background "#F0F4FC")
                   (ansi-color-bright-magenta :short "ansi-bright-magenta" :slug "ANm" :foreground "#bf9eb9" :background "#bf9eb9"))))

  (defun +org-publish-rename (props)
    "Things to do after the project finishes."
    (let ((out-dir (plist-get props :publishing-directory)))
      (when (file-exists-p (expand-file-name "config.html" out-dir))
        (rename-file
         (expand-file-name "config.html" out-dir)
         (expand-file-name "index.html" out-dir) t))))

  (setq org-html-validation-link nil
        org-html-head-include-scripts t
        org-html-head-include-default-style nil
        org-html-divs '((preamble "header" "top")
                        (content "main" "content")
                        (postamble "footer" "postamble"))
        org-html-container-element "section"
        org-html-metadata-timestamp-format "%Y-%m-%d"
        org-html-checkbox-type 'html
        org-html-html5-fancy t
        org-html-htmlize-output-type 'css
        org-html-doctype "html5"
        org-html-home/up-format "%s\n%s\n"
        org-export-with-broken-links t
        org-html-head "<link rel='stylesheet' type='text/css' href='static/index.css' />\n<link rel='shortcut icon' type='image/png' href='https://raw.githubusercontent.com/eccentric-j/doom-icon/master/cute-doom/src/doom.iconset/icon_32x32.png'>"
        org-html-scripts "<script src='static/index.js'></script>"
        org-export-headline-levels 8
        org-publish-project-alist
        `(("dotfiles"
           :base-directory ,doom-user-dir
           :base-extension "org"
           :publishing-directory "out"
           :exclude "README.org"
           :publishing-function org-html-publish-to-html
           :completion-function +org-publish-rename
           :with-creator t
           :section-numbers nil)

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
