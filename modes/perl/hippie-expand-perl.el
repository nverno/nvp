(eval-when-compile
  (require 'cl-lib))
(require 'hippie-exp)

;;; Hippie Expand
;; https://github.com/jrockway/elisp/blob/master/_local/che-perl.el
(defvar he-perl-moose-prefixes '("_build_" "clear_" "has_"))

;; Return a list of Moose attributes declared in this class.
(defun he-perl-get-moose-attributes ()
  (save-excursion
    (goto-char (point-min))
    (let (result)
      (while
          (re-search-forward
           "^[[:space:]]*has[^[:space:]]*[[:space:]]+\\(['\"]?\\)\\([^'\"]+\\)"
           nil t)
        (setq result
              (cons
               (buffer-substring-no-properties
                (match-beginning 2) (match-end 2))
               result)))
      result)))

;; this algorithm is bad.
;; Return a list of possible Moose methods starting with TEXT for this class.
(defun he-perl-possible-moose-methods (text)
  (cl-delete-if-not
   (lambda (x) (string-match (concat "^" text) x))
   (cl-loop for attribute in (he-perl-get-moose-attributes)
      nconc (cl-loop for prefix in he-perl-moose-prefixes
               collect (concat prefix attribute)))))

;; A function for `hippie-expand-try-functions-list' that understands Moose.
;; Argument OLD is automatically provided by `hippie-expand'; nil
;; the first time the function is called, t every other time.
(defun try-expand-perl-moose-method (old)
  (unless old
    (he-init-string (he-lisp-symbol-beg) (point))
    (setf he-expand-list
          (he-perl-possible-moose-methods
           (buffer-substring-no-properties (he-lisp-symbol-beg) (point)))))
  (if (null he-expand-list)
      (ignore (he-reset-string))
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list (cdr he-expand-list))
    t))

;;;###autoload
(defun hippie-expand-perl-setup ()
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list
               #'try-expand-perl-moose-method t))

(provide 'hippie-expand-perl)
