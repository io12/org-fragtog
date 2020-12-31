;;; org-fragtog.el --- Auto-toggle Org LaTeX fragments -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Benjamin Levy - MIT/X11 License
;; Author: Benjamin Levy <blevy@protonmail.com>
;; Version: 0.3.1
;; Description: Automatically toggle Org mode LaTeX fragment previews as the cursor enters and exits them
;; Homepage: https://github.com/io12/org-fragtog
;; Package-Requires: ((emacs "24.3") (org "9.3.2"))

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This package automates toggling Org mode LaTeX fragment
;; previews.  Fragment previews are disabled for editing when
;; your cursor steps onto them, and re-enabled when the cursor
;; leaves.

;;; Code:

(require 'org)

(defgroup org-fragtog nil
  "Auto-toggle Org LaTeX fragments"
  :group 'org-latex)

(defcustom org-fragtog-ignore-predicates nil
  "List of predicates to determine whether to ignore a fragment.
For example, adding `org-at-table-p' will ignore fragments inside tables."
  :group 'org-fragtog
  :type 'hook
  :options '(org-at-table-p
             org-at-table.el-p
             org-at-block-p
             org-at-heading-p))

;;;###autoload
(define-minor-mode org-fragtog-mode
  "A minor mode that automatically toggles Org mode LaTeX fragment previews.
Fragment previews are disabled for editing when your cursor steps onto them,
and re-enabled when the cursor leaves."
  nil nil nil

  ;; Fix nil error in `org-element-context'
  ;; when using `org-fragtog' without Org mode.
  (setq org-complex-heading-regexp (or org-complex-heading-regexp ""))

  (if org-fragtog-mode
      (add-hook 'post-command-hook #'org-fragtog--post-cmd nil t)
    (remove-hook 'post-command-hook #'org-fragtog--post-cmd t)))

(defvar-local org-fragtog--prev-frag nil
  "Previous fragment that surrounded the cursor, or nil if the cursor was not
on a fragment. This is used to track when the cursor leaves a fragment.")

(defun org-fragtog--post-cmd ()
  "This function is executed by `post-command-hook' in `org-fragtog-mode'.
It handles toggling fragments depending on whether the cursor entered or exited them."
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
  "Return the fragment currently surrounding the cursor.
If there is none, return nil.
If the fragment is ignored from rules in `org-fragtog-ignore-predicates',
return nil."
  (let*
      ;; Element surrounding the cursor
      ((elem (org-element-context))
       ;; Type of element surrounding the cursor
       (elem-type (car elem))
       ;; A LaTeX fragment or environment is surrounding the cursor
       (elem-is-latex (member elem-type '(latex-fragment latex-environment)))
       ;; Whether the fragment should be ignored
       (should-ignore (run-hook-with-args-until-success
                       'org-fragtog-ignore-predicates)))

    (if (and elem-is-latex (not should-ignore))
        elem
      nil)))

(defun org-fragtog--enable-frag (frag)
  "Enable the Org LaTeX fragment preview for the fragment FRAG."

  ;; The fragment must be disabled before `org-latex-preview', since
  ;; `org-latex-preview' only toggles, leaving no guarantee that it's enabled
  ;; afterwards.
  (org-fragtog--disable-frag frag)

  ;; Move to fragment and enable
  (save-excursion
    (goto-char (car
                (org-fragtog--frag-pos frag)))
    (org-latex-preview)))

(defun org-fragtog--disable-frag (frag)
  "Disable the Org LaTeX fragment preview for the fragment FRAG."
  (let
      ((pos (org-fragtog--frag-pos frag)))
    (org-clear-latex-preview (car pos)
                             (cdr pos))))

(defun org-fragtog--frag-pos (frag)
  "Get the position of the fragment FRAG.
Return a cons of the begin and end positions."
  (cons
   (org-element-property :begin frag)
   (org-element-property :end frag)))

(provide 'org-fragtog)

;;; org-fragtog.el ends here
