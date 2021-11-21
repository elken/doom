;;; lang/sql/autoload/sql.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +sql-connect (name)
  "Wrapper for `sql-connect' which also handles setting buffer
name."
  (interactive (list (sql-read-connection "Connection: " nil '(nil))))
  (let* ((sql-product (or (cadadr (assoc 'sql-product (cdr (assoc name sql-connection-alist))))
                          sql-product)))
    (sql-connect name name)))
