;;; ui/tab-bar/config.el -*- lexical-binding: t; -*-

(declare-function "persp-names" 'perspective)

(defgroup lkn-tab-bar nil
  "Options related to my custom tab-bar."
  :group 'tab-bar)

(defgroup lkn-tab-bar-faces nil
  "Faces for the tab-bar."
  :group 'lkn-tab-bar)

(defface lkn-tab-bar-workspace-tab
  `((t :inherit default :family ,(face-attribute 'default :family)))
  "Face for a workspace tab."
  :group 'lkn-tab-bar-faces)

(defface lkn-tab-bar-selected-workspace-tab
  '((t :inherit (highlight lkn-tab-bar-workspace-tab)))
  "Face for a selected workspace tab."
  :group 'lkn-tab-bar-faces)

(defun lkn-tab-bar--workspaces ()
  "Return a list of the current workspaces."
  (nreverse
   (let ((show-help-function nil)
         (persps (persp-names))
         (persp (persp-current-name)))
     (when (< 1 (length persps))
       (seq-reduce
        (lambda (acc elm)
          (let* ((face (if (equal persp elm)
                           'lkn-tab-bar-selected-workspace-tab
                         'lkn-tab-bar-workspace-tab))
                 (pos (1+ (cl-position elm persps)))
                 (edge-x (get-text-property 0 'edge-x (car acc)))
                 (tab-id (format " %d" pos))
                 (tab-name (format " %s " elm)))
            (push
             (concat
              (propertize tab-id
                          'id pos
                          'name elm
                          'edge-x (+ edge-x (string-pixel-width tab-name) (string-pixel-width tab-id))
                          'face
                          `(:inherit ,face
                            :weight bold))
              (propertize tab-name 'face `,face))
             acc)
            acc))
        persps
        `(,(propertize (persp-current-name) 'edge-x 0 'invisible t)))))))

(customize-set-variable 'global-mode-string '((:eval (lkn-tab-bar--workspaces)) " "))
(customize-set-variable 'tab-bar-format '(tab-bar-format-global))
(customize-set-variable 'tab-bar-mode t)

;; These two things combined prevents the tab list to be printed either as a
;; tooltip or in the echo area
(defun tooltip-help-tips (_event)
  "Hook function to display a help tooltip.
This is installed on the hook `tooltip-functions', which
is run when the timer with id `tooltip-timeout-id' fires.
Value is non-nil if this function handled the tip."
  (let ((xf (lambda (str) (string-trim (substring-no-properties str)))))
    (when (and
           (stringp tooltip-help-message)
           (not (string= (funcall xf tooltip-help-message) (funcall xf (format-mode-line (lkn-tab-bar--workspaces))))))
      (tooltip-show tooltip-help-message (not tooltip-mode))
      t)))

(tooltip-mode)

(defun lkn-tab-bar--event-to-item (event)
  "Given a click EVENT, translate to a tab.

We handle this by using `string-pixel-width' to calculate how
long the tab would be in pixels and use that in the reduction in
`lkn-tab-bar--workspaces' to determine which tab has been
clicked."
  (let* ((posn (event-start event))
         (workspaces (lkn-tab-bar--workspaces))
         (x (car (posn-x-y posn))))
    (car (cl-remove-if (lambda (workspace)
                         (>= x (get-text-property 0 'edge-x workspace)))
                       workspaces))))

(defun lkn-tab-bar-mouse-1 (ws)
  "Switch to tabs by left clicking."
  (when-let ((id (get-text-property 0 'id ws)))
    (persp-switch-by-number id)))

(defun lkn-tab-bar-mouse-2 (ws)
  "Close tabs by clicking the mouse wheel."
  (when-let ((name (get-text-property 0 'name ws)))
    (persp-kill name)))

(defun lkn-tab-bar-click-handler (evt)
  "Function to handle clicks on the custom tab."
  (interactive "e")
  (when-let ((ws (lkn-tab-bar--event-to-item evt)))
    (pcase (car evt)
      ('mouse-1 (lkn-tab-bar-mouse-1 ws))
      ('mouse-2 (lkn-tab-bar-mouse-2 ws)))))

(keymap-set tab-bar-map "<mouse-1>" #'lkn-tab-bar-click-handler)
(keymap-set tab-bar-map "<mouse-2>" #'lkn-tab-bar-click-handler)
(keymap-set tab-bar-map "<wheel-up>" #'persp-prev)
(keymap-set tab-bar-map "<wheel-down>" #'persp-next)

(provide 'lkn-tab-bar)
