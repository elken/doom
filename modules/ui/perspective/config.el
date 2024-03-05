;;; ui/perspective/config.el -*- lexical-binding: t; -*-

(use-package! perspective
  :init (persp-mode)
  :hook (kill-emacs . persp-state-save)
  :custom
  (persp-sort 'created)
  (persp-suppress-no-prefix-key-warning t)
  (persp-show-modestring nil)
  (persp-state-default-file (expand-file-name "perspectives" doom-cache-dir))
  :config
  (defun +doom-dashboard--persp-detect-project-h ())

  (when (modulep! :completion vertico)
    (after! consult
      (consult-customize consult--source-buffer :hidden t :default nil)
      (add-to-list 'consult-buffer-sources persp-consult-source)))

  (defun persp-generate-id ()
    (format "#%d"
            (cl-loop for name in (persp-names)
                     when (string-match-p "^#[0-9]+$" name)
                     maximize (string-to-number (substring name 1)) into max
                     finally return (if max (1+ max) 1))))

  (defun persp-names (&optional asc)
    "Return a list of the names of all perspectives on the `selected frame'.

Optionall sort ASCending."
    (let ((persps (hash-table-values (perspectives-hash))))
      (cond ((eq persp-sort 'name)
             (sort (mapcar 'persp-name persps) (if asc #'string> #'string<)))
            ((eq persp-sort 'access)
             (mapcar 'persp-name
                     (sort persps (lambda (a b)
                                    (time-less-p (persp-last-switch-time (if asc b a))
                                                 (persp-last-switch-time (if asc a b)))))))
            ((eq persp-sort 'created)
             (mapcar 'persp-name
                     (sort persps (lambda (a b)
                                    (time-less-p (persp-created-time (if asc b a))
                                                 (persp-created-time (if asc a b))))))))))

  (map!
   :leader
   :desc "Switch buffer" "," #'persp-switch-to-buffer*
   (:prefix-map ("TAB" . "Workspaces")
    :desc "Switch to last workspace" "`" (cmd! (persp-last))
    :desc "New workspace"            "n" (cmd! (if current-prefix-arg
                                                   (persp-switch nil)
                                                 (persp-switch (persp-generate-id))))
    :desc "Load workspace from file" "l" #'persp-state-load
    :desc "Save workspace to file"   "s" #'persp-state-save
    :desc "Delete this workspace"    "d" (cmd! (persp-kill (persp-current-name)))
    :desc "Rename workspace"         "r" #'persp-rename
    :desc "Next workspace"           "]" #'persp-next
    :desc "Previous workspace"       "[" #'persp-prev
    :desc "Switch to 1st workspace"  "1" (cmd! (persp-switch-by-number 1))
    :desc "Switch to 2nd workspace"  "2" (cmd! (persp-switch-by-number 2))
    :desc "Switch to 3rd workspace"  "3" (cmd! (persp-switch-by-number 3))
    :desc "Switch to 4th workspace"  "4" (cmd! (persp-switch-by-number 4))
    :desc "Switch to 5th workspace"  "5" (cmd! (persp-switch-by-number 5))
    :desc "Switch to 6th workspace"  "6" (cmd! (persp-switch-by-number 6))
    :desc "Switch to 7th workspace"  "7" (cmd! (persp-switch-by-number 7))
    :desc "Switch to 8th workspace"  "8" (cmd! (persp-switch-by-number 8))
    :desc "Switch to 9th workspace"  "9" (cmd! (persp-switch-by-number 9))
    :desc "Switch to 10th workspace" "0" (cmd! (persp-switch-by-number 10))))

  ;; Inspired by <https://github.com/bbatsov/persp-projectile>
  (defadvice project-switch-project (around project-persp-switch-project (project) activate)
    "Switch to perspective for project."
    (interactive (list (project-prompt-project-dir)))
    (let* ((name (project-name (project-current nil project)))
           (persp (gethash name (perspectives-hash)))
           (command (if (symbolp project-switch-commands)
                        project-switch-commands
                      (project--switch-project-command)))
           (project-current-directory-override project))
      (persp-switch name)
      (unless (equal persp (persp-curr))
        (call-interactively command))))

  (defadvice persp-init-frame (after project-persp-init-frame activate)
    "Rename initial perspective to the project name when a new frame
is created in a known project."
    (with-selected-frame frame
      (when (projectile-project-p)
        (persp-rename (project-name (project-current)))))))
