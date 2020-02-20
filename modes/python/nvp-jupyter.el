;;; nvp-jupyter.el --- jupyter -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'nvp-python)
(require 'python)
(declare-function nvp-setup-local "nvp-setup")
(declare-function w32-shell-execute "w32")
(declare-function conda-env-activate "conda-env")

;; -------------------------------------------------------------------
;;; Commands

;; switch between python/julia/js and load those tools
(defun nvp-jupyter-switch-mode (mode)
  (interactive
   (list (ido-completing-read "Set local variables for: "
                              '("python" "julia" "js"))))
  (pcase mode
    ("python"
     (nvp-setup-local "python-tools" :abbr-table "python-mode"))
    ("julia"
     (if (require 'julia-tools nil t)
         (nvp-setup-local "julia-tools" :abbr-table "julia-mode")
       (user-error "julia-tools not installed.")))
    ("js"
     (if (require 'js-tools nil t)
         (nvp-setup-local "js-tools")
       (user-error "js-utils not installed")))))

;; kill jupyter processes
(defun nvp-jupyter-kill-processes ()
  (interactive)
  (nvp-with-gnu/w32
      (start-process "pkill" nil "pkill" "jupyter")
    (w32-shell-execute
     "runas" "powershell" "-c gps jupyter* | stop-process")))

;; return processes in lisp form ((proc1 . id1) (proc2 . id2) ...)
(defun nvp-jupyter-get-process (arg)
  (interactive "P")
  (let* ((proc (if arg (read-string "Process Name: ") "jupyter"))
         (res
          (nvp-with-gnu/w32
              (mapcar (lambda (s)
                        (apply 'format "(%s . %s)" (nreverse (split-string s))))
                      (process-lines "pgrep" "-l" proc))
            (process-lines
             "powershell" "-c"
             (format "gps *%s* | %%{\"($($_.ProcessName) . $($_.Id))\"}"
                     proc)))))
    (message "%s (pname . pid): %s" proc (or res "No matching processes found."))
    (car (read-from-string (format "%s" res)))))

;; -------------------------------------------------------------------
;;; Setup 

(defun nvp-jupyter-start-notebook ()
  ;; start inotebook so can load .ipynb instead of json file
  (and (executable-find "jupyter")
       (start-process "jupyter" "*jupyter*" "jupyter" "notebook"))
  (add-hook 'kill-buffer-hook 'nvp-jupyter-kill-processes nil 'local))

;; shared setup stuff for ein hooks, if START is non-nil then setup
;; proper python and start jupyter server
(defmacro nvp-jupyter-setup-common (&optional start)
  `(progn
     (declare-function conda-env-activate "conda-env")
     (declare-function nvp-setup-local "nvp")
     (nvp-setup-local "python-tools" :abbr-table "python-mode")
     (nvp-with-w32
       ;; setups up local variables and adds directories containing
       ;; python and ipython programs to exec-path
       (nvp-python-add-paths (nvp-program "python") (nvp-program "ipython")))
     ,@(when start
         (list
          ;; sci has matplotlib/pyplot etcetc.
          '(conda-env-activate "sci")
          '(nvp-jupyter-start-notebook)))))

(provide 'nvp-jupyter)
;;; nvp-jupyter.el ends here
