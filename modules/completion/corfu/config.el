;;; completion/corfu/config.el -*- lexical-binding: t; -*-

(defvar +corfu-global-capes
  '(:completion)
  "A list of global capes to be available at all times.
The key :completion is used to specify where completion candidates should be
placed, otherwise they come first.")

(defvar +corfu-capf-hosts
  '(lsp-completion-at-point
    eglot-completion-at-point
    elisp-completion-at-point
    tags-completion-at-point-function)
  "A prioritised list of host capfs to create a super cape onto from
  `+corfu-global-capes'.")

(use-package! corfu
  :custom
  (corfu-auto t)
  (corfu-on-exact-match nil)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (completion-cycle-threshold 1)
  (tab-always-indent 'complete)
  (corfu-min-width 50)
  :hook
  (doom-first-buffer . global-corfu-mode)
  :config
  (when (modulep! +minibuffer)
    (add-hook! 'minibuffer-setup-hook
      (defun corfu-move-to-minibuffer ()
        "Move current completions to the minibuffer"
        (interactive)
        (let ((completion-extra-properties corfu--extra)
              completion-cycle-threshold completion-cycling)
          (apply #'consult-completion-in-region completion-in-region--data)))))

  ;; Dirty hack to get c completion running
  ;; Discussion in https://github.com/minad/corfu/issues/34
  (when (and (modulep! :lang cc)
             (equal tab-always-indent 'complete))
    (map! :map c-mode-base-map
          :i [remap c-indent-line-or-region] #'completion-at-point))

  ;; Reset lsp-completion provider
  (after! lsp-mode
    (setq lsp-completion-provider :none))

  (add-hook! '(lsp-mode-hook eglot-mode-hook after-change-major-mode-hook)
    (defun +corfu--load-capes ()
      "Load all capes specified in `+corfu-global-capes'."
      (interactive)
      (when-let ((host (cl-intersection +corfu-capf-hosts completion-at-point-functions)))
        (setq-local
         completion-at-point-functions
         (cl-substitute
          (apply #'cape-super-capf (cl-substitute (car host) :completion (cl-pushnew :completion +corfu-global-capes)))
          (car host)
          completion-at-point-functions)))))

  (map! :map corfu-map
        "C-SPC"    #'corfu-insert-separator
        "C-n"      #'corfu-next
        "C-p"      #'corfu-previous
        "M-m"      #'corfu-move-to-minibuffer
        (:prefix "C-x"
                 "C-k"     #'cape-dict
                 "C-f"     #'cape-file))

  (after! evil
    (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
    (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
    (evil-make-overriding-map corfu-map))

  (defadvice! +corfu--org-return (orig) :around '+org/return
    (if (and (modulep! :completion corfu)
             corfu-mode
             (>= corfu--index 0))
        (corfu-insert)
      (funcall orig)))

  (unless (display-graphic-p)
    (corfu-doc-terminal-mode)
    (corfu-terminal-mode)))


(use-package! orderless
  :when (modulep! +orderless)
  :init
  (setq completion-styles '(orderless partial-completion)
        completion-category-overrides '((file (styles . (partial-completion))))))


(use-package! kind-icon
  :after corfu
  :when (modulep! +icons)
  :commands (kind-icon-margin-formatter)
  :init
  (setq kind-icon-default-face 'corfu-default
        kind-icon-use-icons nil
        kind-icon-mapping
        `((array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
          (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
          (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
          (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
          (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
          (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
          (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
          (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
          (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
          (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
          (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
          (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
          (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
          (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
          (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
          (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
          (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
          (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
          (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
          (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
          (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
          (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
          (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
          (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
          (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
          (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
          (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
          (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
          (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
          (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
          (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
          (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
          (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
          (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
          (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
          (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)))
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package! cape
  :defer t
  :init
  (map!
   [remap dabbrev-expand] 'cape-dabbrev)

  (add-hook! 'latex-mode-hook
    (defun +corfu--latex-set-capfs ()
      (make-local-variable '+corfu-global-capes)
      (add-to-list '+corfu-global-capes #'cape-tex)))

  (add-to-list '+corfu-global-capes #'cape-file)
  (add-to-list '+corfu-global-capes #'cape-dabbrev t))


(use-package! corfu-history
  :after corfu
  :init
  (after! savehist
    (add-to-list 'savehist-additional-variables 'corfu-history))
  :hook (corfu-mode . corfu-history-mode))


(use-package! corfu-quick
  :after corfu
  :bind (:map corfu-map
              ("C-q" . corfu-quick-insert)))


(use-package! corfu-echo
  :after corfu
  :hook (corfu-mode . corfu-echo-mode))


(use-package! corfu-info
  :after corfu)


(use-package! corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode))


(use-package! yasnippet-capf
  :after corfu
  :init
  (add-to-list '+corfu-global-capes #'yasnippet-capf))


(use-package! package-capf
  :after corfu
  :init
  (add-hook! 'emacs-lisp-mode-hook
    (defun +corfu--emacs-lisp-set-capfs ()
      (make-local-variable '+corfu-global-capes)
      (add-to-list '+corfu-global-capes #'package-capf)
      (+corfu--load-capes))))


(use-package! evil-collection-corfu
  :when (modulep! :editor evil +everywhere)
  :defer t
  :init (setq evil-collection-corfu-key-themes '(default magic-return))
  :config
  (evil-collection-corfu-setup))
