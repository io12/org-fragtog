;;; org-latex-fragment-autotoggle.el --- Automatically toggle org latex fragments

;; Copyright (C) 2020 Benjamin Levy
;; Author: Benjamin Levy <blevy@protonmail.com>
;; Description: Automatically toggle org-mode latex fragment previews as the cursor enters and exits them
;; Homepage: https://github.com/io12/org-latex-fragment-autotoggle
;; Package-Requires: (org)

;;; Code:

;;;###autoload
(define-minor-mode org-fragtog-mode
  "Toggle Org Latex Fragment Autotoggle Mode, a minor mode that automatically toggles org-mode latex fragment previews as the cursor enters and exits them"
  nil nil nil
  (when (eq 'org-mode major-mode)
    (add-hook 'post-command-hook 'org-fragtog--post-cmd)))

(defun org-fragtog--post-cmd ()
  "This function runs in post-command-hook in org-latex-fragment-autotoggle-mode. It handles toggling fragments depending on whether the cursor entered or exited them."
  (when (org-fragtog--cursor-in-frag-p)
    (org-latex-preview)))

(defun org-fragtog--cursor-in-frag-p ()
  "Returns non-nil if the cursor is in a latex fragment and nil otherwise"
  (let
      ;; Type of element surrounding the cursor
      ((elem-type (car (org-element-context))))
    ;; Latex fragment or environment surrounding the cursor
    (member elem-type '(latex-fragment latex-environment))))

;;; org-latex-fragment-autotoggle.el ends here
