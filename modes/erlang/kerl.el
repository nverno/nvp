;;; kerl.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (defvar eshell-path-env))

;; non-nil kerl speaks about changes
(defvar kerl-verbose t)

;; hooks run after [de]activation
(defvar kerl-after-activation-hook ())

;; -------------------------------------------------------------------
;;; Utils

;; alist of kerl installs . paths
(defun kerl-installs ()
  (let (res)
    (with-temp-buffer
      (call-process "kerl" nil t nil "list" "installations")
      (goto-char (point-min))
      (while (re-search-forward
              "\\(^[^ ]+\\)[ \t]+\\([^\n\r]+\\)" nil 'move)
        (push (cons (match-string 1) (match-string 2)) res)))
    res))

;; remove kerl paths from env, update with new
(defun kerl-update-path (&optional new)
  (ignore-errors
    (start-process "kerl_deactivate" nil "kerl_deactivate"))
  (let* ((installs (kerl-installs))
         (paths (mapcar #'(lambda (x) (expand-file-name "bin" (cdr x)))
                        installs))
         (path (split-string (getenv "PATH") path-separator)))
    (when paths
      (setq path
            (cl-remove-if #'(lambda (s) (cl-member s paths :test 'string=))
                          path)))
    (when new
      (setq path (cons (expand-file-name
                        "bin" (cdr (assoc-string new installs)))
                       path)))
    (setq exec-path path)
    (setenv "PATH" (mapconcat 'identity exec-path path-separator))
    ;; (setq eshell-path-env (getenv "PATH"))
    (run-hooks 'kerl-after-activation-hook)
    (when kerl-verbose
      (message "[kerl] %sctivated %s" (if new "A" "Dea") (or new "")))))

;; -------------------------------------------------------------------
;;; [De]Activate build

;;;###autoload
(defun kerl-activate (build)
  "Activate kerl build."
  (interactive
   (list (ido-completing-read "Activate: " (kerl-installs))))
  (and build (kerl-update-path build)))

;;;###autoload
(defun kerl-deactivate ()
  "Deactivate kerl build."
  (interactive)
  (kerl-activate nil))

(provide 'kerl)
;;; kerl.el ends here
