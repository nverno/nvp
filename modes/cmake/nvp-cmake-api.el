;;; nvp-cmake-api.el --- cmake api queries -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'f)
(require 'nvp)
(require 'nvp-cmake)
(nvp:decls)

;; XXX: read reply files from index-*.json[reply][client-nvp][query.json][responses]
;; > jq '.reply["client-nvp"]["query.json"]["responses"]' <index-*.json>
;; where kind = [codemodel|cache|cmakeFiles]
(defvar nvp-cmake-queries
  '((target "codemodel-*" "[.configurations[].targets[].name]")
    (variable nvp-cmake--variables)
    (cache "cache-*" "[.entries[] | [.name, .value]]")
    (file "cmakeFiles-*" "[.inputs[] | select(.isGenerated != true and .isExternal != true) | .path]")
    (file-external "cmakeFiles-*" "[.inputs[] | select(.isExternal) | .path]")
    (file-generated "cmakeFiles-*" "[.inputs[] | select(.isGenerated) | .path]"))
  "Cmake API queries: lists of NAME [(REPLY-GLOB JQ-QUERY JQ-ARGS)|FUNCTION].")

(defvar nvp-cmake--api-buffer "*cmake-api*")

(defvar nvp-cmake--query-client "client-nvp")

(defvar nvp-cmake--dump-vars
  (expand-file-name "bin/dump-vars.cmake" (nvp:package-root nvp-cmake)))

(eval-when-compile
  (require 'json)
  (defsubst nvp:json-read-buffer ()
    (json-parse-buffer :object-type 'hash-table
                       :array-type 'list
                       :null-object nil
                       :false-object :false))

  (cl-defmacro nvp:with-api (&rest body &key root (init-api t) &allow-other-keys)
    "Do BODY with \\='default-directory bound to cmake api directory.
\\='it-project, \\='it-build and \\='it-api are bound to directories."
    (declare (indent defun))
    (nvp:skip-keywords body)
    `(if-let* ((it-project (or ,root (nvp-project-root)))
               (it-build (nvp-cmake--build-directory it-project))
               (it-api (expand-file-name ".cmake/api/v1/" it-build)))
         (if (or (f-directory-p it-api)
                 ,(when init-api
                     `(and (f-directory-p it-build)
                           (prog1 t (make-directory it-api 'parents)))))
             (let ((default-directory it-api))
               ,@body)
           (user-error "Missing CMake api directory: %S" it-api))
       (user-error
        "Missing CMake build dir: project %S, builddir: %S" it-project it-build)))
  
  (cl-defmacro nvp:with-reply (reply-glob &rest body &key root &allow-other-keys)
    "Do BODY with \\='reply bound to reply files matching REPLY-GLOB in project
ROOT."
    (declare (indent 1))
    (nvp:skip-keywords body)
    `(nvp:with-api :root ,root :init-api t
       (if-let ((reply (f-glob (concat "reply/" ,reply-glob))))
           (progn ,@body)
         (user-error "Missing cmake api reply: api-dir: %S" default-directory))))

  (cl-defmacro nvp:with-jq-query (type &rest body &key root &allow-other-keys)
    "Do BODY with \\='jq-query, \\='jq-args, and \\='reply bound for query TYPE."
    (declare (indent defun))
    (nvp:skip-keywords body)
    (nvp:with-syms (glob)
      `(pcase-let ((`(,,glob ,jq-query ,jq-args)
                    (cdr (assq ,type nvp-cmake-queries))))
         (unless jq-query (user-error "bad jq query %S" ,type))
         (nvp:with-reply ,glob
           :root ,root :init-api t
           (let ((reply (if (length= reply 1) (car reply)
                          (cl-pushnew "-s" jq-args :test #'string=)
                          (mapconcat 'identity reply " "))))
             ,@body))))))

(defun nvp-cmake--build (&optional root init)
  "Run cmake build to generate API responses."
  (let ((build-dir (nvp-cmake--build-directory root)))
    (and init (make-directory build-dir t))
    (if (f-exists-p build-dir)
        (nvp:with-process nvp-cmake-executable
          :proc-name "cmake-api"
          :proc-args (build-dir))
      (user-error "Missing cmake build directory: %S" build-dir))))

(defun nvp-cmake--query-json (&optional root rerun)
  "Generate JSON query request for CMake file API and build."
  (nvp:with-api :root root :init-api t
    (let* ((query-dir (expand-file-name "query/client-nvp/"))
           (query (expand-file-name "query.json" query-dir)))
      (make-directory query-dir t)
      (when (or rerun (not (file-exists-p query)))
        (with-temp-file query
          (insert "{
  \"requests\": [
    {\"kind\": \"codemodel\", \"version\": 2},
    {\"kind\": \"cache\", \"version\": 2},
    {\"kind\": \"cmakeFiles\", \"version\": 1}
  ]
}"))
        (let ((default-directory it-project))
          (nvp-cmake--build it-project))))))

(defun nvp-cmake--query-jq (type)
  "Get results from jq query TYPE."
  (nvp:with-jq-query type
    (with-temp-buffer
      (call-process-shell-command
       (concat "jq " (mapconcat 'identity jq-args " ") " '" jq-query "' " reply)
       nil (current-buffer))
      (goto-char (point-min))
      (nvp:json-read-buffer))))

(defun nvp-cmake--variables (&rest _)
  "Get variables and their values from generated cmake files."
  ;; when cmake is run with 'cmake -P', CMAKE_[SOURCE|BIN]_DIR set to current
  ;; working directory
  (let ((root (f-parent (nvp-cmake--build-directory))))
    (--when-let (append (nvp-cmake--query-jq 'file-generated)
                        ;; (nvp-cmake--query-jq 'file-external)
                        )
      (let ((default-directory root)
            (script (make-temp-file "cmake-api" nil ".cmake")))
        (with-temp-file script
          (dolist (file it)
            (when (cl-member (f-ext file) '("cmake" "txt") :test #'string=)
              (insert (format "include(\"%s\")\n" (expand-file-name file)))))
          (insert (f-read-text nvp-cmake--dump-vars)))
        (prog1 
            (with-current-buffer (get-buffer-create nvp-cmake--api-buffer)
              (erase-buffer)
              (call-process-shell-command
               (format "%s -P %s" nvp-cmake-executable script) nil (current-buffer))
              (goto-char (point-min))
              (seq-filter (lambda (e) (not (string-match-p "CMAKE_ARG" (car e))))
                          (read (current-buffer))))
          (when (file-exists-p script)
            (delete-file script)))))))

(defun nvp-cmake--query (type &optional query-fn)
  "Get results for query TYPE."
  (declare (indent 1))
  (pcase-let ((`(,name ,arg ,rest) (assq type nvp-cmake-queries)))
    (when (null name)
      (user-error "Unrecognized cmake api query: %S" type))
    (pcase arg
      ((pred functionp) (funcall arg rest))
      (_ (funcall (or query-fn #'nvp-cmake--query-jq) type)))))

;; -------------------------------------------------------------------
;;; Interface

(defun nvp-cmake-ensure-query (&optional root init rerun)
  "Ensure query is run in cmake project ROOT.
If INIT, run cmake build if it hasn't been run.
If RERUN, re-run cmake api query even if it has already run."
  (--if-let (nvp-cmake-project-p root)
      (let ((build-dir (nvp-cmake--build-directory it)))
        (unless (file-exists-p build-dir)
          (and init (nvp-cmake--build it 'init)))
        (nvp-cmake--query-json it rerun))
    (user-error "Not in a recognized cmake project")))

;;;###autoload
(defun nvp-cmake-completing-read (&optional type prompt rerun)
  "Completing read for cmake target, or TYPE, with PROMPT.
TYPE is entry from `nvp-cmake-queries'."
  (nvp-cmake-ensure-query nil 'init rerun)
  (let* ((vals (nvp-cmake--query (or type 'target)))
         (choice
          (completing-read
           (or prompt (concat (capitalize (symbol-name type)) ": ")) vals)))
    (assoc-string choice vals)))

;;;###autoload
(defun nvp-cmake-list (&optional rerun)
  "List computed cmake values, prompting for type.
With prefix arg, RERUN cmake query beforehand."
  (interactive "P")
  (nvp-cmake-ensure-query nil 'init rerun)
  (let* ((type (intern (completing-read "Type: " (mapcar #'car nvp-cmake-queries))))
         (res (nvp-cmake--query type)))
    (nvp:with-results-buffer :title (format "Cmake %S" type)
      (dolist (v res)
        (pp v)))))

(provide 'nvp-cmake-api)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-cmake-api.el ends here
