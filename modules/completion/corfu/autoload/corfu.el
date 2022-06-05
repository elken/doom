;;; completion/corfu/autoload/corfu.el -*- lexical-binding: t; -*-
;;;###if (featurep! :completion corfu +minibuffer)

;;;###autoload
(defun +corfu--enable-in-minibuffer ()
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (memq this-command '(evil-ex
                                   evil-ex-search-forward
                                   evil-ex-search-backward))
              (and (featurep! :completion helm)
                   (helm--alive-p))
              (corfu-mode +1))))
