;;; ob-fsharp.el --- org-babel functions for fsharp evaluation

;; Copyright (C) Ellis Kenyo

;; Author: Ellis Kenyo <me@elken.dev>
;; Homepage: https://orgmode.org
;; Package-Requires: ((emacs "26.1") (fsharp-mode "1.10"))
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

;; printfn "Hello, world!"

;; #+end_src

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(add-to-list 'org-babel-tangle-lang-exts '("fsharp" . "fs"))

(defvar org-babel-default-header-args:fsharp '())

(defgroup ob-fsharp nil
  "org-mode blocks for F#."
  :group 'org)

(defcustom org-babel-fsharp-dotnet-command "dotnet"
  "The command to execute babel body code."
  :group 'ob-fsharp
  :type 'string)

(defcustom org-babel-fsharp-command-args '("fsi" "--readline-")
  "Arguments to pass to `org-babel-fsharp-dotnet-command' to create an inferior F# process."
  :group 'ob-fsharp
  :type 'list)

(defvar org-babel-fsharp-eoe-indicator "\"org-babel-fsharp-eoe\";;"
  "String to indicate that evaluation has completed.")
(defvar org-babel-fsharp-eoe-output "org-babel-fsharp-eoe"
  "String to indicate that evaluation has completed.")

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
  (let* ((processed-params (org-babel-process-params params))
         (session (org-babel-prep-session:fsharp
                   (cdr (assq :session params)) params))
         (result-params (assq :result-params processed-params))
         (full-body (org-babel-expand-body:generic
                     body params (org-babel-variable-assignments:fsharp params)))
         (raw (org-babel-fsharp-parse-body session full-body)))
    (string-match "val \\(.*\\): \\(.*\\) = \\(.*\\)" raw)

    (let ((output (match-string 1 raw))
          (type (match-string 2 raw))
          (value (match-string 3 raw)))
      (org-babel-reassemble-table
       (org-babel-result-cond result-params
         (cond
          ((member "verbatim" result-params) raw)
          ((member "output" result-params) output)
          (t raw))
         (if (and value type)
             (org-babel-fsharp-parse-output value type)
           raw))
       (org-babel-pick-name
        (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
       (org-babel-pick-name
        (cdr (assq :rowname-names params)) (cdr (assq :rownames params)))))))

(defun org-babel-fsharp-parse-body (session body)
  "Trim the BODY of the output."
  (let ((body (car
               (let ((re (regexp-quote org-babel-fsharp-eoe-output)) out)
                 (delq nil (mapcar (lambda (line)
                                     (if out
                                         (progn (setq out nil) line)
                                       (when (string-match re line)
                                         (progn (setq out t) nil))))
                                   (mapcar #'org-trim (reverse (org-babel-comint-with-output
                                                                   (session org-babel-fsharp-eoe-output nil body)
                                                                 (insert
                                                                  (concat
                                                                   (org-babel-fsharp-trim-body body) "\n"
                                                                   org-babel-fsharp-eoe-indicator))
                                                                 (comint-send-input))))))))))
    (when body
      (org-trim (org-babel-fsharp-remove-vals body)))))

(defun org-babel-fsharp-remove-vals (body)
  "Given a BODY, remove all lines that are variable definitions, except 'it'
  because that's usually our return value."
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    (flush-lines "val [^it]:")
    (flush-lines "val it: unit = ()")
    (buffer-string)))

(defun org-babel-fsharp-trim-body (body)
  "Given a BODY, remove all comments as this breaks the script execution."
  (org-babel-chomp
   (with-temp-buffer
     (fsharp-mode)
     (insert body)
     (comment-kill (count-lines (point-min) (point-max)))
     (save-excursion
       (goto-char (point-min))
       (while (< (forward-line) 1)
         (end-of-line)
         (insert ";;")))
     (buffer-string))))

(defun org-babel-fsharp-elisp-to-fsharp (val)
  "Return a string of fsharp code which evaluates to VAL."
  (if (listp val)
      (concat "[|" (mapconcat #'org-babel-fsharp-elisp-to-fsharp val "; ") "|]")
    (format "%S" val)))

(defun org-babel-fsharp-parse-output (value type)
  "Parse VALUE of TYPE."
  (cond
   ((string= "string" type)
    (org-babel-read value))
   ((or (string= "int" type)
        (string= "float" type))
    (string-to-number value))
   ((or (string-match "array" type)
        (string-match "list" type)
        (string-match "\\[\\]" type))
    (org-babel-fsharp-read-array value))
   ((or (string-match "Map" type)
        (string-match "Set" type))
    (org-babel-fsharp-read-map value))
   (t (message "Unrecognized type %s" type) value)))

(defun org-babel-fsharp-read-list (results)
  "Convert RESULTS into an elisp table or string."
  (org-babel-script-escape (replace-regexp-in-string ";" "," results)))

(defun org-babel-fsharp-read-array (results)
  "Convert RESULTS into an elisp table or string."
  (org-babel-script-escape
   (replace-regexp-in-string
    "\\[|" "[" (replace-regexp-in-string
                "|\\]" "]" (replace-regexp-in-string
                            "; " "," results)))))

(defun org-babel-fsharp-read-map (results)
  "Convert RESULTS from a Map to an elisp alist."
  (org-babel-script-escape
   (replace-regexp-in-string
    "\\(?:map\\|set\\) " ""
    (replace-regexp-in-string
     "\\[" "("
     (replace-regexp-in-string
      "\\]" ")"
      (replace-regexp-in-string
       "[;,]" "" results))))))

(defun org-babel-variable-assignments:fsharp (params)
  "Return list of F# statements assigning the block's variables."
  (mapcar
   (lambda (pair) (format "let %s = %s;;" (car pair)
                     (org-babel-fsharp-elisp-to-fsharp (cdr pair))))
   (org-babel--get-vars params)))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:fsharp (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-fsharp-initiate-session session))
         (var-lines (org-babel-variable-assignments:fsharp params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (insert var)
              (comint-send-input nil t)
              (org-babel-comint-wait-for-output session))
            var-lines))
    session))

(defun org-babel-fsharp-initiate-session (&optional session _params)
  "Initiate a session named SESSION according to PARAMS"
  (require 'inf-fsharp-mode)
  (let* ((inferior-fsharp-buffer-subname (if (and (not (string= session "none"))
                                                  (not (string= session "default"))
                                                  (stringp session))
                                             (format "inferior-fsharp-%s" session)
                                           inferior-fsharp-buffer-subname))
         (inferior-fsharp-buffer-name (concat "*" inferior-fsharp-buffer-subname "*")))
    (save-window-excursion
      (or (org-babel-comint-buffer-livep inferior-fsharp-buffer-name)
          (progn
            (fsharp-run-process-if-needed
             (concat org-babel-fsharp-dotnet-command " " (mapconcat #'identity org-babel-fsharp-command-args " ")))
            (set-marker comint-last-output-start (point))
            (get-buffer (current-buffer)))))))

(provide 'ob-fsharp)
;;; ob-fsharp.el ends here
