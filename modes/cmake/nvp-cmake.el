;;; nvp-cmake.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls :p (cmake-mode cmake-ts) :f (nvp-cmake-completing-read))

(defvar nvp-cmake--dir
  (file-name-as-directory
     (directory-file-name
      (file-name-directory
       (cond (load-in-progress load-file-name)
             ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
              byte-compile-current-file)
             (t (buffer-file-name)))))))

;; prefer over miniconda's cmake
(defvar nvp-cmake-executable (nvp:program "cmake" :path "/usr/bin"))

(defvar nvp-cmake-build-directory "build"
  "Path to cmake build directory. Either an absolute path, or a relative path
to build directory from project root.")

(defvar nvp-cmake-default-args
  (delq nil (list (nvp:with-w32 "-G \"MSYS Makefiles\"")
                  ;; "-DCMAKE_CXX_COMPILER=g++"
                  ;; "-DCMAKE_C_COMPILER=gcc"
                  "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"))
  "Default cmake arguments.")

(defun nvp-cmake--build-directory (&optional root)
  "Return cmake build directory, optionally using project ROOT."
  (if (file-name-absolute-p nvp-cmake-build-directory)
      nvp-cmake-build-directory
    (expand-file-name nvp-cmake-build-directory (or root (nvp-project-root)))))

(defun nvp-cmake-project-p (&optional root)
  "Return project root if it is a cmake project."
  (let ((root (or root (nvp-project-root))))
    (when (file-exists-p (expand-file-name "CMakeLists.txt" root))
      root)))

;;; Compile/run

(defun nvp-cmake-run-script ()
  "Compile current file as cmake script (cmake -P)."
  (interactive)
  (let ((compile-command
         (format "%s -P %s" nvp-cmake-executable (buffer-file-name))))
    (funcall-interactively #'compile compile-command)))

;;;###autoload
(defun nvp-cmake-compile (&optional prefix)
  "Compile using CMake.
Target is read with `completing-read'.
With PREFIX, prompt for CMake arguments."
  (interactive "P")
  (--if-let (nvp-cmake-project-p)
      (let* ((build-dir (nvp-cmake--build-directory it))
             (args (if prefix (list (read-from-minibuffer
                                     "CMake args: "
                                     (mapconcat 'identity nvp-cmake-default-args " ")))
                     nvp-cmake-default-args))
             (target (nvp-cmake-completing-read 'target))
             (compile-command
              (format "%s %s %s %s"
                      nvp-cmake-executable it
                      (mapconcat 'identity (delq nil args) " ")
                      (if target (concat " && make " target) ""))))
        (make-directory build-dir t)
        (let ((default-directory build-dir))
          (funcall-interactively #'compile compile-command)))
    (user-error "Not a cmake project")))

(provide 'nvp-cmake)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-cmake.el ends here
