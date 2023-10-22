;;; nvp-cmake-api.el --- cmake api queries -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'f)
(require 'nvp)

(defvar nvp-cmake-build-directory "build"
  "Path to cmake build directory. Either an absolute path, or a relative path
to build directory from project root.")

(defvar nvp-cmake-jq-queries
  '((targets "codemodel-*" "[.configurations[].targets[].name]")
    (variables "cache-*" "[.entries[] | [.name, .value]]"))
  "Cmake API jq queries: lists of NAME REPLY-GLOB JQ-QUERY JQ-ARGS.")

(eval-when-compile
  (require 'json)
  (defsubst nvp:json-read-buffer ()
    (json-parse-buffer :object-type 'hash-table
                       :array-type 'list
                       :null-object nil
                       :false-object :false))

  (defmacro nvp:with-api-reply (reply-glob &optional root &rest body)
    "Do BODY with \\='reply bound to reply files matching REPLY-GLOB in project
ROOT."
    (declare (indent 2))
    (nvp:with-syms (project build-dir)
      `(if-let* ((,project ,(or root '(nvp-project-root)))
                 (,build-dir (nvp-cmake--build-directory ,project))
                 (reply
                  (f-glob (concat ".cmake/api/v1/reply/" ,reply-glob) ,build-dir)))
           (progn ,@body)
         (user-error
          "Missing CMake api reply: project %S, builddir: %S"
          ,project ,build-dir))))

  (defmacro nvp:with-jq-query (type &rest body)
    "Do BODY with \\='jq-query, \\='jq-args, and \\='reply bound for query TYPE."
    (declare (indent defun))
    (nvp:with-syms (glob)
      `(pcase-let ((`(,,glob ,jq-query ,jq-args)
                    (cdr (assq ,type nvp-cmake-jq-queries))))
         (unless ,glob (user-error "Unrecognized query %S" ,type))
         (nvp:with-api-reply ,glob nil
           (let ((reply (if (length= reply 1) (car reply)
                          (cl-pushnew "-s" jq-args :test #'string=)
                          (mapconcat 'identity reply " "))))
             ,@body))))))

(defun nvp-cmake--build-directory (&optional root)
  "Return cmake build directory, optionally using project ROOT."
  (if (f-absolute-p nvp-cmake-build-directory)
      nvp-cmake-build-directory
    (f-expand nvp-cmake-build-directory (or root (nvp-project-root)))))

(defun nvp-cmake--query-jq (type)
  "Get results from jq query TYPE."
  (nvp:with-jq-query type
    (with-temp-buffer
      (call-process-shell-command
       (concat "jq " (mapconcat 'identity jq-args " ") " '" jq-query "' " reply)
       nil (current-buffer))
      (goto-char (point-min))
      (nvp:json-read-buffer))))

;;;###autoload
(defun nvp-cmake-completing-read (&optional prompt type)
  "Completing read for cmake targets, or TYPE, with PROMPT."
  (completing-read
   (or prompt "Target: ")
   (nvp-cmake--query-jq (or type 'targets))))

;;;###autoload
(defun nvp-cmake-list (type)
  "List computed cmake values of TYPE."
  (interactive
   (list (intern (completing-read "Type: " (mapcar #'car nvp-cmake-jq-queries)))))
  (nvp:with-jq-query type
    (nvp:with-results-buffer :title (format "Cmake %S" type)
      (call-process-shell-command
       (concat "jq -r " (mapconcat 'identity jq-args " ")
               " '" (substring jq-query 1 -1) "' " reply)
       nil (current-buffer) t))))

(provide 'nvp-cmake-api)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-cmake-api.el ends here
