;;; nvp-conf --- parse config file -*- lexical-binding: t; -*-
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(autoload 'substitute-env-vars "env")

;; Convert config of form:
;; [target]
;; key=value
;; ...
;; into alist => ((target1 (key1 . val1) (key2 . val2)) (target2 ...))
(defun nvp-conf-read (file)
  (let (res target vals)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^\\[\\(.*\\)\\]" nil 'move)
        (setq target (match-string 1))
        (forward-line)
        (beginning-of-line)
        (while (not (or (eobp) (eq (char-after) ?\[)))
          (when (and
                 (not (looking-at-p "\\s-*#"))
                 (looking-at "[ \t]*\\(.*\\)[ \t]*=[ \t]*\\([^#\n]+\\)[ \t]*$"))
            (push (cons (match-string 1) (substitute-env-vars (match-string 2)))
                  vals))
          (forward-line))
        (push (cons target vals) res)
        (setq vals nil)))
    res))

;; find target->value in conf if it exists, nil otherwise
(defun nvp-conf-value (conf target &optional value)
  (let ((res (cdr (cl-assoc target conf :test 'string=))))
    (if (not value)
        res
      (cdr (cl-assoc value res :test 'string=)))))

(defun nvp-conf-default-targets (conf)
  (split-string (nvp-conf-value conf "default" "targets") "[ \t,]" 'omit " "))

;; browse download location of target
(defun nvp-conf-visit-location (conf target)
  (when-let ((loc (nvp-conf-value conf target "loc")))
    (browse-url loc)))

(provide 'nvp-conf)
;;; nvp-conf.el ends here
