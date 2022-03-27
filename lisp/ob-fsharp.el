;;; ob-fsharp.el --- org-babel functions for fsharp evaluation

;; Copyright (C) Ellis Kenyo

;; Author: your name here
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file is not intended to ever be loaded by org-babel, rather it is a
;; fsharp for use in adding new language support to Org-babel. Good first
;; steps are to copy this file to a file named by the language you are adding,
;; and then use `query-replace' to replace all strings of "fsharp" in this
;; file with the name of your new language.

;; After the `query-replace' step, it is recommended to load the file and
;; register it to org-babel either via the customize menu, or by evaluating the
;; line: (add-to-list 'org-babel-load-languages '(fsharp . t)) where
;; `fsharp' should have been replaced by the name of the language you are
;; implementing (note that this applies to all occurrences of 'fsharp' in this
;; file).

;; After that continue by creating a simple code block that looks like e.g.
;;
;; #+begin_src fsharp

;; test

;; #+end_src

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(add-to-list 'org-babel-tangle-lang-exts '("fsharp" . "fs"))

(defvar org-babel-default-header-args:fsharp '())

(defgroup ob-fsharp nil
  "org-mode blocks for PHP."
  :group 'org)

(defcustom org-babel-fsharp-dotnet-command "dotnet"
  "The command to execute babel body code."
  :group 'ob-fsharp
  :type 'string)

;; This function expands the body of a source code block by doing things like
;; prepending argument definitions to the body, it should be called by the
;; `org-babel-execute:fsharp' function below. Variables get concatenated in
;; the `mapconcat' form, therefore to change the formatting you can edit the
;; `format' form.
(defun org-babel-expand-body:fsharp (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'inf-fsharp nil t)
  (let ((vars (org-babel--get-vars (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s=%S"
                (car pair) (org-babel-fsharp-var-to-fsharp (cdr pair))))
      vars "\n")
     "\n" body "\n")))

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.
(defun org-babel-execute:fsharp (body params)
  "Execute a block of Fsharp code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Fsharp source code block")
  (let* ((processed-params (org-babel-process-params params))
         ;; set the session if the value of the session keyword is not the
         ;; string `none'
         (session (unless (string= body "none")
                   (org-babel-fsharp-initiate-session
                    (cdr (assq :session processed-params)))))
         ;; variables assigned for use in the block
         (vars (org-babel--get-vars processed-params))
         (result-params (assq :result-params processed-params))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (assq :result-type processed-params))
         ;; expand the body with `org-babel-expand-body:fsharp'
         (full-body (org-babel-expand-body:fsharp
                     body params processed-params))
         (script-file (org-babel-temp-file "fsharp-src-" ".fsx"))
         (cmd (concat org-babel-fsharp-dotnet-command " fsi " script-file)))
    (with-temp-file script-file
      (insert full-body))
    (org-babel-eval cmd "")
    ;; actually execute the source-code block either in a session or
    ;; possibly by dropping it to a temporary file and evaluating the
    ;; file.
    ;;
    ;; for session based evaluation the functions defined in
    ;; `org-babel-comint' will probably be helpful.
    ;;
    ;; for external evaluation the functions defined in
    ;; `org-babel-eval' will probably be helpful.
    ;;
    ;; when forming a shell command, or a fragment of code in some
    ;; other language, please preprocess any file names involved with
    ;; the function `org-babel-process-file-name'. (See the way that
    ;; function is used in the language files)
    ))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:fsharp (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-fsharp-var-to-fsharp (var)
  "Convert an elisp var into a string of fsharp source code
specifying a var of the same value."
  (format "%S" var))

(defun org-babel-fsharp-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

(defun org-babel-fsharp-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")
    ))

(provide 'ob-fsharp)
;;; ob-fsharp.el ends here
