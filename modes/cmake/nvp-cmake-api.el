;;; nvp-cmake-api.el --- cmake api queries -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'f)
(require 'nvp)
(require 'nvp-cmake)

;; XXX: read reply files from index-*.json[reply][client-nvp][query.json][responses]
;; > jq '.reply["client-nvp"]["query.json"]["responses"]' <index-*.json>
;; where kind = [codemodel|cache|cmakeFiles]
(defvar nvp-cmake-queries
  '((targets "codemodel-*" "[.configurations[].targets[].name]")
    (variables nvp-cmake--variables)
    (cache "cache-*" "[.entries[] | [.name, .value]]")
    (files "cmakeFiles-*" "[.inputs[] | select(.isGenerated != true and .isExternal != true) | .path]")
    (files-external "cmakeFiles-*" "[.inputs[] | select(.isExternal) | .path]")
    (files-generated "cmakeFiles-*" "[.inputs[] | select(.isGenerated) | .path]"))
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

  (cl-defmacro nvp:with-api (&rest body &key root &allow-other-keys)
    "Do BODY with \\='default-directory bound to cmake api directory."
    (declare (indent defun))
    (nvp:skip-keywords body)
    (nvp:with-syms (project build-dir api-dir)
      `(if-let* ((,project (or ,root (nvp-project-root)))
                 (,build-dir (nvp-cmake--build-directory ,project))
                 (,api-dir (expand-file-name ".cmake/api/v1/" ,build-dir)))
           (if (f-directory-p ,api-dir)
               (let ((default-directory ,api-dir))
                 ,@body)
             (user-error "Missing CMake api directory: %S" ,api-dir))
         (user-error
          "Missing CMake build dir: project %S, builddir: %S" ,project ,build-dir))))
  
  (cl-defmacro nvp:with-reply (reply-glob &rest body &key root &allow-other-keys)
    "Do BODY with \\='reply bound to reply files matching REPLY-GLOB in project
ROOT."
    (declare (indent 1))
    (nvp:skip-keywords body)
    `(nvp:with-api :root ,root
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
           :root ,root
           (let ((reply (if (length= reply 1) (car reply)
                          (cl-pushnew "-s" jq-args :test #'string=)
                          (mapconcat 'identity reply " "))))
             ,@body))))))

(defun nvp-cmake--build-directory (&optional root)
  "Return cmake build directory, optionally using project ROOT."
  (if (f-absolute-p nvp-cmake-build-directory)
      nvp-cmake-build-directory
    (f-expand nvp-cmake-build-directory (or root (nvp-project-root)))))

(defun nvp-cmake--build (&optional root init)
  "Run cmake build to generate API responses."
  (let ((build-dir (nvp-cmake--build-directory root)))
    (and init (make-directory build-dir t))
    (if (f-exists-p build-dir)
        (nvp:with-process nvp-cmake-executable
          :proc-name "cmake-api"
          :proc-args (build-dir))
      (user-error "Missing cmake build directory: %S" build-dir))))

(defun nvp-cmake--query-json (&optional root)
  "Generate JSON query request for CMake file API and build."
  (nvp:with-api :root root
    (let* ((query-dir (expand-file-name "query/client-nvp/"))
           (query (expand-file-name "query.json" query-dir)))
      (make-directory query-dir t)
      (unless (file-exists-p query)
        (with-temp-file query
          (insert "{
  \"requests\": [
    {\"kind\": \"codemodel\", \"version\": 2},
    {\"kind\": \"cache\", \"version\": 2},
    {\"kind\": \"cmakeFiles\", \"version\": 1}
  ]
}"))
        (nvp-cmake--build root)))))

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
  (let ((root (f-parent (nvp-cmake--build-directory))))
    (--when-let (nvp-cmake--query-jq 'files-generated)
      (let ((script (make-temp-file "cmake-api" nil ".json")))
        (with-temp-file script
          (dolist (file it)
            (insert (format "include(\"%s\")\n" (expand-file-name file root))))
          (insert (f-read-text nvp-cmake--dump-vars)))
        (with-current-buffer (get-buffer-create nvp-cmake--api-buffer)
          (erase-buffer)
          (call-process-shell-command
           (format "%s -P %s" nvp-cmake-executable script) nil (current-buffer))
          (goto-char (point-min))
          (seq-filter (lambda (e) (not (string-match-p "CMAKE_ARG" (car e))))
                      (read (current-buffer))))))))

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

(defun nvp-cmake-project-p (&optional root)
  "Return project root if it is a cmake project."
  (let ((root (or root (nvp-project-root))))
    (when (file-exists-p (expand-file-name "CMakeLists.txt" root))
      root)))

(defun nvp-cmake-ensure-query (&optional root init)
  "Ensure query is run in cmake project ROOT.
If INIT, run cmake build if it hasn't been run."
  (--if-let (nvp-cmake-project-p)
      (let ((build-dir (nvp-cmake--build-directory it)))
        (unless (file-exists-p build-dir)
          (and init (nvp-cmake--build it 'init)))
        (nvp-cmake--query-json root))
    (user-error "Not in a recognized cmake project")))

;;;###autoload
(defun nvp-cmake-completing-read (&optional prompt type)
  "Completing read for cmake targets, or TYPE, with PROMPT.
TYPE is entry from `nvp-cmake-queries'."
  (let* ((vals (nvp-cmake--query (or type 'targets)))
         (choice (completing-read (or prompt "Target: ") vals)))
    (assoc-string choice vals)))

;;;###autoload
(defun nvp-cmake-list (type)
  "List computed cmake values of TYPE."
  (interactive
   (list (intern (completing-read "Type: " (mapcar #'car nvp-cmake-queries)))))
  (nvp-cmake-ensure-query nil 'init)
  (let ((res (nvp-cmake--query type)))
    (nvp:with-results-buffer :title (format "Cmake %S" type)
      (dolist (v res)
        (pp v)))))

(provide 'nvp-cmake-api)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-cmake-api.el ends here
