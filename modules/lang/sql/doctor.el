;;; lang/sql/doctor.el -*- lexical-binding: t; -*-

(when (featurep! :checkers syntax)
  (unless (executable-find "sqlint")
    (warn! "Couldn't find sqlint. Syntax checking will not work")))

(when (featurep! +lsp)
  (unless (executable-find "sqls")
    (error! "Couldn't find sqls. Needed for LSP")))

(dolist (app '(("psql" . "PostgreSQL")
               ("mysql" . "MySQL")
               ("sqlite" . "SQLite")
               ("solsql" . "Solid")
               ("sqlplus" . "SQL*Plus")
               ("dbaccess" . "Informix")
               ("isql" . "SyBase or Interbase")
               ("sql" . "Ingres")
               ("osql" . "MS SQL Server")
               ("db2" . "DB2")
               ("inl" . "RELEX")))
  (unless (executable-find (car app))
    (warn! "Couldn't find %s. %s won't work" (car app) (cdr app))))
