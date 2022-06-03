;;; completion/corfu/config.el -*- lexical-binding: t; -*-

;; (use-package! corfu
;;   :init
;;   (global-corfu-mode +1)
;;   (add-hook 'minibuffer-setup-hook #'+corfu-enable-in-minibuffer)
;;   :config
;;   (setq corfu-auto t
;;         corfu-auto-prefix 2
;;         corfu-auto-delay 0.2
;;         corfu-min-width 80
;;         corfu-max-width corfu-min-width
;;         corfu-count 14
;;         corfu-scroll-margin 4
;;         corfu-cycle nil)
;;   ;; https://github.com/minad/corfu/issues/12#issuecomment-869037519
;;   (after! evil
;;     (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
;;     (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
;;     (evil-make-overriding-map corfu-map)
;;     (add-hook 'evil-insert-state-exit-hook #'corfu-quit)))

;; (use-package! kind-icon
;;   :when (featurep! +icons)
;;   :after corfu
;;   :config
;;   (setq kind-icon-use-icons t
;;           kind-icon-default-face 'corfu-default
;;           kind-icon-blend-background nil
;;           kind-icon-blend-frac 0.08
;;           svg-lib-icons-dir (expand-file-name "svg-lib" doom-cache-dir))
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
;;   (add-hook 'doom-load-theme-hook #'kind-icon-reset-cache))

;; (use-package! corfu-doc
;;   :init
;;   (add-hook 'corfu-mode #'corfu-doc-mode)
;;   :config
;;   (setq corfu-doc-delay 0.5
;;         corfu-doc-max-width 70
;;         corfu-doc-max-height 20
;;         corfu-echo-documentation nil))

;; (use-package! cape
;;   :commands (cape-dabbrev cape-file cape-keyword cape-symbol cape-abbrev
;;                           cape-ispell cape-line cape-dict cape-tex cape-sgml
;;                           cape-rfc1345)
;;   :init
;;   (map! "M-/" #'cape-dabbrev
;;         "C-M-/" #'dabbrev-expand
;;         (:when (featurep! :editor evil)
;;          (:prefix "C-x"
;;           :i "C-l" #'cape-line
;;           :i "C-k"  (cmd!
;;                      (let ((completion-at-point-functions
;;                             '(cape-ispell cape-keyword)))
;;                        (completion-at-point)))
;;           :i "C-f" #'cape-file
;;           :i "C-]"  (cmd!
;;                      (let ((completion-at-point-functions
;;                             `(,(cape-company-to-capf #'company-etags))))
;;                        (completion-at-point)))
;;           :i "s" #'cape-ispell
;;           :i "C-s" (cmd!
;;                     (let ((completion-at-point-functions
;;                            `(,(cape-company-to-capf #'company-yasnippet))))
;;                       (completion-at-point))))))

;;   (add-hook 'prog-mode-hook
;;             (defun +cape-set-up-programming-capfs ()
;;               (add-to-list 'completion-at-point-functions #'cape-file)
;;               (add-to-list 'completion-at-point-functions #'cape-tex)
;;               (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
;;               (add-to-list 'completion-at-point-functions #'cape-keyword t)))

;;   (add-hook 'text-mode-hook
;;             (defun +cape-set-up-writing-capfs ()
;;               (add-to-list 'completion-at-point-functions #'cape-file)
;;               (add-to-list 'completion-at-point-functions #'cape-tex)
;;               (add-to-list 'completion-at-point-functions #'cape-dabbrev t)))

;;   (add-hook 'conf-mode-hook
;;             (defun +cape-set-up-conf-caps ()
;;               (add-to-list 'completion-at-point-functions #'cape-file)
;;               (add-to-list 'completion-at-point-functions #'cape-tex)
;;               (add-to-list 'completion-at-point-functions #'cape-dabbrev t))))

;; (use-package lsp-mode
;;   :when (featurep! :tools lsp)
;;   :custom
;;   (lsp-completion-provider :none) ;; we use Corfu!

;;   :init
;;   (defun my/orderless-dispatch-flex-first (_pattern index _total)
;;     (and (eq index 0) 'orderless-flex))

;;   (defun my/lsp-mode-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless)))

;;   ;; Optionally configure the first word as flex filtered.
;;   (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)

;;   ;; Optionally configure the cape-capf-buster.
;;   (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))

;;   :hook
;;   (lsp-completion-mode . my/lsp-mode-setup-completion))

;; ;; (use-package! corfu-history
;; ;;   :hook corfu-mode)

;; Corfu completion module


;; Reset lsp-completion provider
(add-hook 'doom-init-modules-hook
          (lambda ()
            (after! lsp-mode
              (setq lsp-completion-provider :none))))

;; Pad before lsp modeline error info
(add-hook 'lsp-mode-hook
          (lambda ()
            (setf (caadr
                   (assq 'global-mode-string mode-line-misc-info))
                  " ")))

;; Set orderless filtering for LSP-mode completions
(add-hook 'lsp-completion-mode-hook
          (lambda ()
            (setf (alist-get 'lsp-capf completion-category-defaults) '((styles . (orderless))))))

;; Set bindings
;; (map! :i "C-@" #'completion-at-point
;;       :i "C-SPC" #'completion-at-point
;;       (:prefix "C-x"
;;        :i "C-k" #'cape-dict
;;        :i "C-f" #'cape-file
;;        :i "s" #'cape-ispell
;;        :i "C-n" #'corfu-next
;;        :i "C-p" #'corfu-previous
;;        :i "C-s" #'dabbrev-completion))

;; Fallback cleanly to consult in TUI
(setq-default completion-in-region-function #'consult-completion-in-region)

(use-package corfu
  :custom
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-preview-current t)    ;; Disable current candidate preview
  (corfu-auto t)
  (corfu-on-exact-match nil)
  (corfu-quit-no-match 'separator)
  (corfu-preselect-first nil)
  :hook
  (doom-first-buffer . global-corfu-mode)
  :bind (:map corfu-map
         ("SPC" . corfu-insert-separator)
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("C-n" . corfu-next)
         ("C-p" . corfu-previous)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous)))

(use-package corfu-doc
  :hook
  (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
         ("M-n" . corfu-doc-scroll-down)
         ("M-p" . corfu-doc-scroll-up)
         ("M-d" . corfu-doc-toggle)))

(use-package orderless
  :when (featurep! +orderless)
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :defer t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(setq completion-cycle-threshold 1)

;; Enable indentation+completion using the TAB key.
;; Completion is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; Dirty hack to get c completion running
;; Discussion in https://github.com/minad/corfu/issues/34
(when (equal tab-always-indent 'complete)
  (map! :map c-mode-base-map
        :i [remap c-indent-line-or-region] #'completion-at-point))
