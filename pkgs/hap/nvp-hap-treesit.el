;;; nvp-hap-treesit.el --- become context aware -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'treesit)
(require 'nvp-hap)

(cl-defstruct (nvp--hap-ts (:constructor nvp-hap-ts-make))
  "Hap tree-sitter backend"
  parsers                             ; parsers where it should always be active
  context                             ; parser->query contexts to be active
  ts-modes                            ; supported treesit modes
  backend                             ; hap backend
  )

;; Map modes -> supported backends
(defvar nvp-hap-treesit-backends (make-hash-table))

;;;###autoload
(defun nvp-hap-add-backend (backend)
  (pcase-let (((cl-struct nvp--hap-ts ts-modes) backend))
    (dolist (mode ts-modes)
      (cl-pushnew backend (gethash mode nvp-hap-treesit-backends)))))

(defvar nvp-hap-treesit--sh-backend
  (nvp-hap-ts-make
   :parsers '(bash)
   :context '((dockerfile-ts-mode dockerfile "shell"))
   :ts-modes '(dockerfile-ts-mode bash-ts-mode)
   :backend #'nvp-hap-sh))

(cl-eval-when (load)
  (nvp-hap-add-backend nvp-hap-treesit--sh-backend))


;; list of local parsers at point
(defun nvp--hap-local-parsers (&optional point)
  (mapcar #'treesit-parser-language
          (treesit-local-parsers-at (or point (point)))))

(defun nvp--hap-parsers ()
  (mapcar #'treesit-parser-language (treesit-parser-list)))

;; set `nvp-hap--treesit-p' to `t' if buffer should be considered by treesit
;; context-aware backends
(defun nvp-hap-treesit-available-p ()
  (or nvp-hap--treesit-p
      (setq nvp-hap--treesit-p
            (or (ignore-errors
                  (and (treesit-available-p) (treesit-language-at (point))))
                'fail))))

;; filter out any treesit backends that dont handle mode
(defun nvp-hap-treesit-init (&optional backends)
  (unless (memq (nvp-hap-treesit-available-p) '(fail done))
    (let ((buffer-parsers (nvp--hap-parsers))
          global local)
      ;; nvp-help-at-point-functions
      (dolist (bend (or backends (gethash major-mode nvp-hap-treesit-backends)))
        (pcase-let (((cl-struct nvp--hap-ts backend parsers context) bend))
          (if (seq-find
               (lambda (parser) (seq-contains-p buffer-parsers parser)) parsers)
              (push backend global)
            (push (list ':query (cdr (assq major-mode context))
                        ':parsers parsers
                        ':backend backend)
                  local))))
      (setq nvp-hap--treesit-p 'done)
      ;; TODO: be able to choose order
      (setq nvp-help-at-point-functions
            (append local global nvp-help-at-point-functions)))))

(defun nvp--hap-ts-local-parser-p (parsers)
  (seq-find (lambda (p) (seq-contains-p parsers p)) (nvp--hap-local-parsers)))

(defun nvp--hap-ts-query-p (lang query)
  (treesit-node-match-p
   (treesit-node-at (point) lang) query 'ignore-missing))

;; return the hap backend if it should be active here
(defun nvp-hap-treesit-local-active-p (hap-backend)
  (cl-destructuring-bind (&key query parsers backend) hap-backend
    (when (or (nvp--hap-ts-local-parser-p parsers)
              (nvp--hap-ts-query-p (car query) (cadr query)))
      backend)))

;; non-nil if backend should be active here
(defun nvp-hap-treesit-active-p (backend)
  (cl-block nil
    (pcase-let (((cl-struct nvp--hap-ts parsers context) backend))
      (let ((local-parser (nvp--hap-local-parsers)))
        (dolist (parser local-parser)
          (when (memq parser parsers)
            (cl-return t)))
        (let* ((query (cdr (assq major-mode context)))
               (lang (prog1 (pop query)
                       (setq query (car query)))))
          ;; TODO: handle query, not just matching node
          (treesit-node-match-p
           (treesit-node-at (point) lang) query 'ignore-missing))))))

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
