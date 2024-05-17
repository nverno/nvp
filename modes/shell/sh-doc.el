;;; sh-doc.el --- documentation for sh functions -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; TODO:
;; - better check for doc block
;; - extend newline-dwim with doc-newline-dwim
;; - parse doc comments
;; - eldoc support
;;
;; References:
;; - font-locking:
;;   + cc-fonts: #<marker at 108257 in cc-fonts.el.gz>
;;     see `c-font-lock-doc-comments' in cc-fonts
;;   + highlight-doxygen library (no longer necessary since cc-mode does this)
;;     it uses `font-lock-extend-region-functions'
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'sh-script)
;; `font-lock-extend-region-functions'
(eval-when-compile
  (defvar font-lock-beg)
  (defvar font-lock-end))

(defgroup sh-doc nil
  "Support sh script documentation and highlighting."
  :group 'faces)

(defface sh-doc-comment
  '((((background light)) :inherit font-lock-doc-face :background "grey95")
    (((background dark))  :inherit font-lock-doc-face :background "grey30"))
  "The face used for Doxygen comment blocks."
  :group 'sh-doc)

;; (defface highlight-doxygen-code-block
;;   '((((background light)) :background "grey85")
;;     (((background dark))  :background "grey40"))
;;   "The face used to mark a code block."
;;   :group 'highlight-doxygen)

(eval-and-compile
  (defvar sh-doc-types '("brief" "param" "return" "note" "usage" "see")))
(defvar sh-doc-offset-column 16)

(defsubst sh-doc-active-p ()
  (save-excursion
    (or (nth 4 (syntax-ppss))
        (and (bolp) (looking-at-p "#")))))

(defun sh-doc-insert (type)
  (interactive (list (completing-read "doc: " sh-doc-types)))
  (and (bolp) (insert "# "))
  (insert "@" type)
  (indent-to-column sh-doc-offset-column))

;;;###autoload
(defun sh-doc-indent-dwim ()
  (interactive)
  (when (and (sh-doc-active-p)
             ;; indent documentation to offset column if necessary
             (or (not (eq (move-to-column sh-doc-offset-column)
                          sh-doc-offset-column))
                 (not (or (eolp)
                          (looking-at-p "\\s-*$")))))
    (beginning-of-line)
    (if (not (looking-at-p (eval-when-compile
                             (concat "#\\s-*@" (regexp-opt sh-doc-types)))))
        (progn (end-of-line)            ;no info keyword, go to end of line
               (call-interactively 'sh-doc-insert))
      (forward-word)
      (delete-horizontal-space)
      (indent-to-column sh-doc-offset-column))))

;;;###autoload
(defun sh-doc-newline-dwim ()
  (interactive)
  (end-of-line)
  (if (sh-doc-active-p)
      (progn
        (newline)
        (call-interactively 'sh-doc-insert))
    (newline)
    (insert "# ")))

(provide 'sh-doc)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; sh-doc.el ends here
