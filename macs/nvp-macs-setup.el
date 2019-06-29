;;; nvp-macs-setup.el --- setup/config macros -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(require 'cl-lib)
(require 'subr-x)
(eval-when-compile (require 'nvp-macs-common))

;; default suffix appended when defining package location variables
(defvar nvp-package-root-suffix "--dir")

;; -------------------------------------------------------------------
;;; Define 

(defmacro nvp-defvar (&rest var-vals)
  "Define VAR and eval VALUE during compile."
  (declare (indent 0))
  (macroexp-progn
   (cl-loop for (var value) on var-vals by #'cddr
      collect `(progn (defvar ,var (eval-when-compile ,value))))))

(defmacro nvp-setq (&rest var-vals)
  "Define VAR and eval VALUE during compile."
  (declare (indent 0))
  `(progn
     (,@(cons 'eval-when-compile
              (cl-loop for (var _value) on var-vals by #'cddr
                 collect `(defvar ,var))))
     ,@(cl-loop for (var value) on var-vals by #'cddr
          collect `(setq ,var (eval-when-compile ,value)))))


;; -------------------------------------------------------------------
;;; Programs

(defvar nvp-program-search-paths
  '("~/bin/" "~/.asdf/shims/" "~/.local/bin/" "/usr/local/bin/")
  "Default paths to search for executables.")

(defmacro nvp-w32-program (name)
  "Name of cached program on shitty w32.e"
  (and (symbolp name) (setq name (symbol-name name)))
  `(intern (concat "nvp-" ,name "-program")))

;; PATH can contain environment variables to expand
;; if NO-COMPILE is defined the name is determined at runtime
(cl-defmacro nvp-program (name &key path (default t) w32)
  "Try to find program NAME at compile time.
If PATH is non-nil, append to default search paths.
If DEFAULT is non-nil, use NAME if other methods fail.
If W32 is non-nil, on windows, use to find program instead of default.
If program is not found at compile time, fallback to runtime search."
  (declare (indent defun) (debug t))
  (let* ((name (cond
                ((symbolp name) (symbol-name name))
                ((consp name)
                 (pcase name
                   (`(quote ,sym)
                    (symbol-name sym))
                   (_ name)))
                ((stringp name) name)
                (t (user-error "%S unmatched")))))
    `(progn
       (nvp-decl nvp-setup-program)
       (or (eval-when-compile
             (nvp-with-gnu/w32
                 (let ((exec-path (delq nil (cons ,path ',nvp-program-search-paths))))
                   (executable-find ,name))
               ,(if w32 `,w32
                  `(bound-and-true-p (intern (concat "nvp-" ,name "-program")))))
             ;; otherwise try entire PATH
             (executable-find ,name))
           ;; fallback to runtime search
           (when (require 'nvp-setup nil t)
             (nvp-setup-program ,name ,path))
           ,(if default `,name)))))


;; -------------------------------------------------------------------
;;; Paths

(defmacro nvp-mode-config-path (mode &optional ensure-string)
  "Create path for MODE config file."
  `(expand-file-name
    (concat "nvp-" ,(if ensure-string (nvp-stringify mode) `,mode) "-config.el")
    nvp/config))

(defmacro nvp-cache-file-path (filename)
  "Create cache path for FILENAME."
  `(expand-file-name ,(nvp-stringify filename) nvp/cache))


;; -------------------------------------------------------------------
;;; Mode bind

(defmacro nvp-mode-bind-1 (&optional mode &rest bindings)
  "Attach BINDINGS globally to MODE."
  (declare (indent defun) (debug bindings))
  (or mode (setq mode (quote major-mode)))
  (unless (equal 'quote (car-safe mode)) (setq mode `',mode))
  `(if (not (get ,mode 'nvp))
       (put ,mode 'nvp ,@bindings)
     (put ,mode 'nvp
          (cl-delete-duplicates
           (append (get ,mode 'nvp) ,@bindings) :key #'car))))

(defmacro nvp-mode-bind (&optional modes &rest bindings)
  "Attach BINDINGS globally to MODES."
  (declare (indent defun) (debug t))
  ;; Allow for some various forms
  (unless (null (cadr (car-safe bindings)))
    (and (equal 'quote (car-safe modes))
         (setq modes (cadr modes)))                       ; '(a b c)
    (unless (listp modes) (setq modes (cons modes nil)))  ; some-mode
    (setq modes (remq 'quote modes))                      ; ('mode-1 'mode-...)
    (macroexp-progn
     (cl-loop for mode in modes
        collect `(nvp-mode-bind-1 ,mode ,@bindings)))))


;; -------------------------------------------------------------------
;;; Package

(defsubst nvp-package--root (prefix)
  (concat prefix nvp-package-root-suffix))

(defmacro nvp-package-root (&optional name)
  "Expand to the default package directory with default prefix or NAME."
  (let ((prefix
         (if name
             (if (symbolp name) (symbol-name name) name)
           (file-name-nondirectory
            (directory-file-name
             (file-name-directory (nvp-load-file-name)))))))
    (intern (nvp-package--root prefix))))

(cl-defmacro nvp-package-define-root (&key name snippets dirs after-load)
  "Define package root directory with default prefix as directory name or NAME.
If SNIPPETS is non-nil, setup snippet loading for directory.
If DIRS is non-nil it should be a list of variables to define as directories
relative to the project root directory as symbols 'prefix--dir'.
AFTER-LOAD is a form to execute after file is loaded during which the root
directory is bound to `root' and all `dirs' are let-bound to their symbols."
  (declare (indent 0) (debug t))
  (let* ((file (nvp-load-file-name))
         (root-val (file-name-directory file))
         (base (file-name-nondirectory (directory-file-name root-val)))
         (prefix (if name (if (symbolp name) (symbol-name name) name) base))
         (root (intern (nvp-package--root prefix)))
         (dirs (mapcar (lambda (d)
                         (and (symbolp d) (setq d (symbol-name d)))
                         (list (intern d)
                               (intern (concat prefix "--" d))
                               (expand-file-name d root-val)))
                       dirs))
         (mappings (cons `(root ,root) (mapcar 'butlast dirs))))
    `(progn
       (eval-and-compile
         (defconst ,root ,root-val))
       ,(when snippets `(nvp-package-load-snippets ,root))
       ,(when dirs
          `(progn
             ,@(cl-loop for (_orig-sym dir-sym dir-val) in dirs
                  collect `(defconst ,dir-sym ,dir-val))))
       ,(when after-load
          `(with-eval-after-load ,file
             (let ,mappings
               ,after-load))))))

(defmacro nvp-package-load-snippets (dir)
  "Add packages snippet directory to `yas-snippet-dirs' after loading
`yasnippet'."
  `(progn
     (eval-when-compile (defvar yas-snippet-dirs))
     (declare-function yas-load-directory "yasnippet")
     (with-eval-after-load 'yasnippet
       (let ((snippet-dir (expand-file-name "snippets" ,dir))
             (dirs (or (and (consp yas-snippet-dirs) yas-snippet-dirs)
                       (cons yas-snippet-dirs ()))))
         (unless (member snippet-dir dirs)
           (setq yas-snippet-dirs (delq nil (cons snippet-dir dirs))))
         (yas-load-directory snippet-dir)))))


;; -------------------------------------------------------------------
;;; Other modes

;;-- Hydras
(defmacro nvp-hydra-set-property (hydra-name &rest props)
  "Apply PROPS to HYDRA-NAME after `hydra' is loaded.
PROPS defaults to setting :verbosity to 1."
  (declare (indent 1))
  (unless props (setq props (list :verbosity 1)))
  `(progn
     (declare-function hydra-set-property "hydra")
     (with-eval-after-load 'hydra
      ,@(cl-loop for (k v) on props by #'cddr
           collect `(hydra-set-property ,hydra-name ,k ,v)))))

;;-- Smartparens
(cl-defmacro nvp-sp-local-pairs (&rest pairs &key modes &allow-other-keys)
  (declare (indent defun) (debug defun))
  (while (keywordp (car pairs))
    (setq pairs (cdr (cdr pairs))))
  `(progn
     (eval-when-compile (require 'smartparens))
     (declare-function sp-local-pair "smartparens")
     ,(cond
       (modes
        `(sp-with-modes ,modes
           ,@pairs))
       ((equal 'quote (caar pairs)) `(sp-local-pair ,@pairs))
       (t
        (macroexp-progn
         (cl-loop for pair in pairs
            collect `(sp-local-pair ,@pair)))))))

(defmacro nvp-diminish (&rest modes)
  "Diminish MODES in modeline.
MODES is of form (feature . mode)."
  (declare (indent 0))
  `(progn
     (declare-function diminish "diminish")
     (eval-when-compile ,@(mapcar (lambda (f) `(defvar ,(cdr f))) modes))
     ,(macroexp-progn
       (cl-loop for (feat . mode) in modes
          collect `(eval-after-load ',feat '(diminish ',mode))))))


;; -------------------------------------------------------------------
;;; Setup / Build init

;;-- Setup helper functions
;; Find locations for init constants
(defun nvp--setup-normalize-locs (locs &optional defaults)
  "Ensure LOCS is a list.
If LOCS is nil, use DEFAULTS.  If it is a symbol/function (list) get its value(s)."
  (if (null locs)
      (or defaults (nvp-with-gnu/w32 '("~/") '("~/" "d:/" "c:/")))
    (if (and (consp locs) (functionp (car locs)))
        (list (eval locs))
      (and (not (listp locs)) (setq locs (cons locs nil)))
      (mapcar (lambda (l)
                (cond
                 ((consp l) l)
                 ((symbolp l) (symbol-value l))
                 (t l)))
              locs))))

(defun nvp--setup-find-loc (locs &optional places file)
  "Find first existing location in LOCS."
  (let ((locs (nvp--setup-normalize-locs locs nil))
        (places (nvp--setup-normalize-locs places)))
    (cl-loop for loc in locs
       return (cl-loop for place in places
                 as root = (if (symbolp place) (symbol-value place) place)
                 as loc-name = (expand-file-name loc root)
                 when (file-exists-p loc-name)
                 return (if file (directory-file-name loc-name)
                          (file-name-as-directory loc-name))))))

(defun nvp--setup-subdirs (root &optional ignored)
  (setq root
        (cond
         ((symbolp root) (setq root (symbol-value root)))
         ((consp root) (eval root))))
  (cl-remove-if
   (lambda (f)
     (or (not (file-directory-p f))
         (cl-member (file-name-nondirectory f) ignored :test 'string=)))
   (directory-files root t "^[^.]")))

;;-- setup macros
(cl-defmacro nvp-add-to-alist (&rest items
                                     &key
                                     (alist 'auto-mode-alist)
                                     (test 'equal)
                                     &allow-other-keys)
  "Add ITEMS, a list of cons cells, to ALIST using TEST to check if item \
is already present."
  (declare (indent defun) (debug t))
  (while (keywordp (car items))
    (setq items (cdr (cdr items))))
  (macroexp-progn
   (cl-loop for (k . v) in items
      with lst = (if (consp alist) alist (symbol-value alist))
      unless (cl-member (cons k (quote v)) lst :test test)
      collect `(push (cons ,k ',v) ,alist))))

(defmacro nvp-setup-consts (&rest vars)
  "Define consts in init."
  (declare (indent 0) (debug t))
  (macroexp-progn
   (cl-loop for (v dir places file) in vars
      as loc = (nvp--setup-find-loc dir places file)
      do (eval `(defconst ,v ,loc)) ;so subsequent vars can use
      collect `(defconst ,v ,loc))))

(defmacro nvp-setup-load-files (&rest files)
  (declare (indent 0))
  (macroexp-progn
   (cl-loop for f in files
      collect `(load ,f 'noerror 'nomessage))))

(defmacro nvp-setup-load-paths (&rest paths)
  (declare (indent 0))
  (macroexp-progn
   (cl-loop for p in paths
      collect `(add-to-list 'load-path ,p))))

(defmacro nvp-setup-hooks (hook &rest modes)
  "Add HOOK to all MODES hooks."
  (declare (indent 1))
  (when (eq 'quote (car-safe hook))
    (setq hook (eval hook)))
  (macroexp-progn
   (cl-loop for mode in modes
      as mode-hook = (nvp--normalize-hook mode)
      collect `(add-hook ',mode-hook #',hook))))

(defmacro nvp-setup-add-subdir-load-paths (root &optional ignored)
  "Add subdirs under ROOT to `load-path', ingnoring those listed in IGNORED."
  (declare (indent defun))
  (let* ((ignored (or ignored '("." ".." "unused" "ignored" "old")))
         (dirs (nvp--setup-subdirs root ignored)))
    (macroexp-progn
     (cl-loop for d in dirs
        collect `(add-to-list 'load-path ,d)))))

(defmacro nvp-setup-cache (var filename)
  "Set cache FILENAME location."
  `(nvp-setq ,var (expand-file-name ,filename nvp/cache)))

(provide 'nvp-macs-setup)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-setup.el ends here
