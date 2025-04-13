;;; nvp-mode.el --- Mode menu -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(require 'nvp)
(nvp:decls :p (flycheck) :v (flycheck-mode) :f (-map flycheck-mode))
(nvp:auto "nvp-dev" nvp-dev-describe-mode)
(nvp:auto "flycheck" flycheck-list-errors)

(defvar nvp-mode-verbose t)

(defun nvp-mode-message (fmt &rest args)
  (and nvp-mode-verbose (apply #'message (concat "[mode] " fmt) args)))

(defun nvp-mode--choose (var &optional default)
  (let ((val (eval var)))
    (and (functionp val) (setq val (funcall val)))
    (if (null val)
        (if (null default)
            (user-error "No '%S' registered" (symbol-name var))
          (nvp-mode-message "%S default: %S" var default)
          default)
      (let* ((val (cl-remove-duplicates (delq nil (cons default val))))
             (fn (if (length= val 1) (car val)
                   (intern (completing-read
                             (concat (symbol-name var) ": ") val nil t)))))
        (nvp-mode-message "%S using: %S" var fn)
        fn))))

(eval-when-compile
  (cl-defmacro nvp:define-mode-function
      (sym &rest body
       &key
       fn-name
       (default t)
       &allow-other-keys)
    (declare (indent defun) (debug t))
    (nvp:skip-keywords body)
    (let* ((name (symbol-name sym))
           (fn (or fn-name (intern (concat "nvp-" name))))
           (hook (intern (concat "nvp-" name "-functions")))
           (default-fn (if (eq default t)
                           (intern (concat "nvp-" name "-default-function"))
                         default)))
      `(defun ,fn ()
         (interactive)
         (setq prefix-arg current-prefix-arg)
         (condition-case ,(unless body 'err)
             (call-interactively
              (nvp-mode--choose
               ',hook
               ,(when (and default-fn (boundp `,default-fn))
                  `,default-fn)))
           (user-error
            ,@(if body `,@body '((user-error (error-message-string err))))))))))

;;;###autoload(autoload 'nvp-check-buffer "nvp-mode")
(nvp:define-mode-function check-buffer :default nil
  (unless flycheck-mode
    (flycheck-mode 1))
  (call-interactively nvp-check-buffer-default-function))

;;;###autoload(autoload 'nvp-format-buffer "nvp-mode")
(nvp:define-mode-function format-buffer)

;;;###autoload(autoload 'nvp-mode-compile "nvp-mode")
(nvp:define-mode-function compile :fn-name nvp-mode-compile)

;;;###autoload(autoload 'nvp-debug "nvp-mode")
(nvp:define-mode-function debug)

;;;###autoload(autoload 'nvp-test "nvp-mode")
(nvp:define-mode-function test)

;;;###autoload(autoload 'nvp-tag "nvp-mode")
(nvp:define-mode-function tag)

;;;###autoload(autoload 'nvp-disassemble "nvp-mode")
(nvp:define-mode-function disassemble)

;;;###autoload(autoload 'nvp-abbrev "nvp-mode")
(nvp:define-mode-function abbrev)

;;;###autoload(autoload 'nvp-toggle "nvp-mode")
(nvp:define-mode-function toggle)

;;;###autoload(autoload 'nvp-docs "nvp-mode")
(nvp:define-mode-function docs)

;;;###autoload(autoload 'nvp-run "nvp-mode")
(nvp:define-mode-function run)

;;;###autoload(autoload 'nvp-configure "nvp-mode")
(nvp:define-mode-function configure)

;;;###autoload(autoload 'nvp-profile "nvp-mode")
(nvp:define-mode-function profile)

;;;###autoload(autoload 'nvp-jump "nvp-mode")
(nvp:define-mode-function jump)

;;;###autoload(autoload 'nvp-edit "nvp-mode")
(nvp:define-mode-function edit)

;;;###autoload(autoload 'nvp-insert "nvp-mode")
(nvp:define-mode-function insert)

;;;###autoload
(defun nvp-install ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (when nvp-install-functions
    (let ((targs (--group-by (stringp it) nvp-install-functions)))
      (--if-let (cdr (assq nil targs))
          (let ((fn (intern (completing-read "Install: " it nil t))))
            (call-interactively fn))
        (--> (completing-read "Install: " (cdr (assq t targs)) nil t)
             (user-error "unimplemented: %s" it))))))

;; -------------------------------------------------------------------
;;; Menu

(nvp:transient-define-vars nvp--menu (nvp-mode-verbose . t))

(defun nvp-mode-clear-cache (&optional all)
  (interactive "P")
  (if all (clrhash nvp-mode-cache)
    (if nvp-mode-name
        (remhash nvp-mode-name nvp-mode-cache)
      (user-error "`nvp-mode-name' isnt defined"))))

;;;###autoload(autoload 'nvp-mode-menu "nvp-mode" nil t)
(transient-define-prefix nvp-mode-menu ()
  ;; TODO: with some prefix, prompt to choose mode
  "Mode"
  [["Run"
    ("r" "Run" nvp-run)
    ("c" "Compile" nvp-mode-compile)
    ("t" "Test" nvp-test)]
   ["Editing"
    ("e" "Edit" nvp-edit)
    ("i" "Insert" nvp-insert :if-non-nil nvp-insert-functions)
    ("a" "Abbrev" nvp-abbrev :if-non-nil nvp-abbrev-functions)
    ("q" "Toggle" nvp-toggle :if-non-nil nvp-toggle-functions)
    ("f" "Format buffer" nvp-format-buffer)]
   ["Debug"
    ("l" "Check/Lint" nvp-check-buffer)
    ("p" "Profile" nvp-profile :if-non-nil nvp-profile-functions)
    ("d" "Debug" nvp-debug :if-non-nil nvp-debug-functions)
    ("D" "Disassemble" nvp-disassemble :if-non-nil nvp-disassemble-functions)]]
  [["Actions"
    ("j" "Jump" nvp-jump :if-non-nil nvp-jump-functions)
    ("T" "Tag" nvp-tag)
    ("C" "Configure" nvp-configure)
    ("I" "Install" nvp-install)]
   ["Help"
    ("s" "Search docs" nvp-docs :if-non-nil nvp-docs-functions)
    ("M-?" "Describe mode" nvp-dev-describe-mode :transient nil)]
   ["Settings"
    (":v" "Toggle verbose" nvp--menu-mode-verbose)
    (":r" "Clear config" nvp-mode-clear-cache)]])

(provide 'nvp-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-mode.el ends here
