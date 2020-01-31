;;; org-fragtog.el --- Automatically toggle org latex fragments

;; Copyright (C) 2020 Benjamin Levy
;; Author: Benjamin Levy <blevy@protonmail.com>
;; Version: 0.1.0
;; Description: Automatically toggle org-mode latex fragment previews as the cursor enters and exits them
;; Homepage: https://github.com/io12/org-fragtog
;; Package-Requires: (org)

;;; Code:

;;;###autoload
(define-minor-mode org-fragtog-mode
  "Toggle Org Latex Fragment Autotoggle Mode, a minor mode that automatically
toggles org-mode latex fragment previews as the cursor enters and exits them"
  nil nil nil
  (if (and org-fragtog-mode (eq major-mode 'org-mode))
      (add-hook 'post-command-hook 'org-fragtog--post-cmd nil t)
    (remove-hook 'post-command-hook 'org-fragtog--post-cmd t)))

(make-variable-buffer-local
 (defvar org-fragtog--prev-frag
   nil
   "Previous fragment that surrounded the cursor, or nil if the cursor was not
on a fragment. This is used to track when the cursor leaves a fragment."))

(defun org-fragtog--post-cmd ()
  "This function runs in post-command-hook in org-fragtog-mode. It handles
toggling fragments depending on whether the cursor entered or exited them."
  (let*
      ;; Previous fragment
      ((prev-frag org-fragtog--prev-frag)
       ;; Current fragment
       (cursor-frag (org-fragtog--cursor-frag))
       ;; The current fragment didn't change
       (frag-same (equal
		   ;; Fragments are considered the same if they have the same
		   ;; start position
		   (car (org-fragtog--frag-pos cursor-frag))
		   (car (org-fragtog--frag-pos prev-frag))))
       ;; The current fragment changed
       (frag-changed (not frag-same)))

    ;; Only do anything if the current fragment changed
    (when frag-changed
      ;; Current fragment is the new previous
      (setq org-fragtog--prev-frag cursor-frag)
      ;; Enable fragment if cursor left it
      (when prev-frag
	(org-fragtog--enable-frag prev-frag))
      ;; Disable fragment if cursor entered it
      (when cursor-frag
	(org-fragtog--disable-frag cursor-frag)))))

(defun org-fragtog--cursor-frag ()
  "Returns the fragment currently surrounding the cursor, or nil if it does not
exist"
  (let*
      ;; Element surrounding the cursor
      ((elem (org-element-context))
       ;; Type of element surrounding the cursor
       (elem-type (car elem))
       ;; A latex fragment or environment is surrounding the cursor
       (elem-is-latex (member elem-type '(latex-fragment latex-environment))))

    (if elem-is-latex
	elem
      nil)))

(defun org-fragtog--enable-frag (frag)
  "Enable the org latex fragment preview for the specified fragment."

  ;; The fragment must be disabled before org-latex-preview, since
  ;; org-latex-preview only toggles, leaving no guarantee that it's enabled
  ;; afterwards.
  (org-fragtog--disable-frag frag)

  ;; Move to fragment and enable
  (save-excursion
    (goto-char (car
		(org-fragtog--frag-pos frag)))
    (org-latex-preview)))

(defun org-fragtog--disable-frag (frag)
  "Disable the org latex fragment preview for the specified fragment."
  (let
      ((pos (org-fragtog--frag-pos frag)))
    (org-clear-latex-preview (car pos)
			     (cdr pos))))

(defun org-fragtog--frag-pos (frag)
  "Get the position of a fragment. Returns a cons of the begin and end
positions."
  (cons
   (org-element-property :begin frag)
   (org-element-property :end frag)))

;;; org-fragtog.el ends here