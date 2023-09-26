;;; nvp-hap-treesit.el --- become context aware -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'treesit)
(require 'nvp-hap)

(defvar nvp-hap-treesit-context
  '((nvp-hap-sh (dockerfile-ts-mode "shell"))))

;; set `nvp-hap--treesit-p' to `t' if buffer should be considered by treesit
;; context-aware backends
(defun nvp-hap-treesit-available-p ()
  (or nvp-hap--treesit-p
      (setq nvp-hap--treesit-p
            (or (ignore-errors
                  (and (treesit-available-p)
                       (treesit-language-at (point))))
                'fail))))

;; filter out any treesit backends that dont handle mode
(defun nvp-hap-treesit-init (&optional backends)
  (unless (memq (nvp-hap-treesit-available-p) '(fail done))
    (let (res)
      (dolist (backend (or backends nvp-help-at-point-functions))
        (--if-let (and (plistp backend) (plist-get backend :treesit))
            (when (or (eq t it)
                      (memq major-mode (if (listp it) it (list it))))
              (push backend res))
          (push backend res)))
      (setq nvp-hap--treesit-p 'done)
      (setq nvp-help-at-point-functions (nreverse res)))))

(defun nvp-hap-treesit-active-p (backend)
  (let ((context (plist-get backend :context)))
    (treesit-node-match-p (treesit-node-at (point)) context 'ignore-missing)))

;; (defun nvp-hap-treesit--init-backend (backend)
;;   (cl-destructuring-bind (&key name backend treesit context) backend
;;     (when (or (eq t treesit)
;;               (and (listp treesit) (memq major-mode treesit)))
;;       (treesit-node-match-p (treesit-node-at (point)) context)
      
;;       (condition-case nil
;;           (prog1 t
;;             )
;;         (treesit-invalid-predicate
;;          (push backend nvp-hap--disabled-backends))))))

(provide 'nvp-hap-treesit)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-hap-treesit.el ends here
