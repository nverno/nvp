;;; nvp-hook.el --- hooks -*- lexical-binding: t; -*-

;;; Commentary:
;; random hook functions
;;; Code:
(eval-when-compile (require 'nvp-macro))
(defvar time-stamp-pattern)
(nvp-auto "nvp-read" 'nvp-read-elisp-symbol 'nvp-read-elisp-function)

;;;###autoload
(defun nvp-hook-update-timestamp ()
  "Update buffer time stamps - `before-save-hook'."
  (require 'time-stamp)
  (let ((time-stamp-pattern 
         (or time-stamp-pattern
             (pcase major-mode
               (`org-mode "#\\+DATE: <%%>$")
               (`sh-mode "scriptversion=\"%%\";")
               (_ "Last modified: <%%>$")))))
    (time-stamp)))

;; Added to `find-file-not-found-functions'. Create new directories for new files.
;;;###autoload
(defun nvp-hook-find-create-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p 
		(format "directory `%s' does not exist! create it?" 
			parent-directory)))
      (make-directory parent-directory t))))

;; -------------------------------------------------------------------
;;; Cleanup hooks: `write-file-functions'

;; remove trailing whitespace and untabify buffer
;;;###autoload
(defun nvp-cleanup-ws-default ()
  (save-excursion
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace)))

;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-hook-add-or-remove (func hook-var hook-fn &optional append local)
  "Call FUNC to add or remove HOOK-FN from HOOK-VAR, locally when called \
interactively."
  (interactive
   (let* ((func (if current-prefix-arg 'remove-hook 'add-hook))
          (hook (nvp-read-elisp-symbol
                 (format "Hook variable to %s hook: "
                         (if (eq func 'remove-hook)
                             "remove from" "add to"))
                 nil "before-save-hook")))
     (list func hook
           (if (eq func 'remove-hook)
               (intern (completing-read
                        (format "Function to remove from %s: " hook)
                        (remq t (symbol-value hook))))
             (nvp-read-elisp-function (format "Function to add to %s: " hook)))
           nil t)))
  (if (eq func 'remove-hook)
      (remove-hook hook-var hook-fn local)
    (funcall func hook-var hook-fn append local)))

(provide 'nvp-hook)
;;; nvp-hook.el ends here
