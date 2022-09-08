;;; lang/sql/config.el -*- lexical-binding: t; -*-

(defvar +sql/capitalize-keywords nil
  "Whether or not to capitalize keywords by default. Possible
options:

- nil for never
- 'interactive for only during interactive sessions
- 'editor for only during the editor
- t for everywhere")

(defvar +sql/capitalize-ignore '("name")
  "List of keywords to not capitalize, for example 'name' is
commonly used.")


(add-hook! sql-mode
  (when (modulep! +lsp)
    (add-hook 'sql-mode-local-vars-hook #'lsp!))
  (unless (file-directory-p (expand-file-name "sql/" doom-cache-dir))
    (mkdir (expand-file-name "sql/" doom-cache-dir) t))
  (setq-hook! 'sql-interactive-mode-hook
    sql-input-ring-file-name (expand-file-name (format "%s-history.sql" (symbol-name (symbol-value 'sql-product))) (expand-file-name "sql/" doom-cache-dir))))


(use-package! sqllint
  :when (modulep! :checkers syntax)
  :unless (modulep! +lsp)
  :after sql-mode)
