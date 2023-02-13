;;; nvp-jupyter.el --- jupyter -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'nvp-python)
(require 'python)

;; -------------------------------------------------------------------
;;; Commands

;; switch between python/julia/js and load those tools
(defun nvp-jupyter-switch-mode (mode)
  (interactive
   (list (ido-completing-read "Set local variables for: "
                              '("python" "julia" "js"))))
  (pcase mode
    ("python" (nvp-setup-local "python" :abbr-table "python-mode"))
    ("julia" (nvp-setup-local "julia" :abbr-table "julia-mode"))
    ("js" (nvp-setup-local "js"))))

;; kill jupyter processes
(defun nvp-jupyter-kill-processes ()
  (interactive)
  (nvp:with-gnu/w32
      (start-process "pkill" nil "pkill" "jupyter")
    (w32-shell-execute
     "runas" "powershell" "-c gps jupyter* | stop-process")))

;; return processes in lisp form ((proc1 . id1) (proc2 . id2) ...)
(defun nvp-jupyter-get-process (arg)
  (interactive "P")
  (let* ((proc (if arg (read-string "Process Name: ") "jupyter"))
         (res
          (nvp:with-gnu/w32
              (mapcar (lambda (s)
                        (apply 'format "(%s . %s)" (nreverse (split-string s))))
                      (process-lines "pgrep" "-l" proc))
            (process-lines
             "powershell" "-c"
             (format "gps *%s* | %%{\"($($_.ProcessName) . $($_.Id))\"}"
                     proc)))))
    (message "%s (pname . pid): %s" proc (or res "No matching processes found."))
    (car (read-from-string (format "%s" res)))))

(defun nvp-jupyter-current-notebook ()
  (ein:$notebook-notebook-path (ein:get-notebook)))

;; (defun nvp@jupyter-sync (orig-fn notebook &rest args)
;;   (apply orig-fn notebook args)
;;   (message "[jupytext] %s"
;;            (shell-command-to-string
;;             (format "jupytext --sync %s"
;;                     (ein:$notebook-notebook-path notebook)))))
;; (advice-add 'ein:notebook-save-notebook-success :around #'nvp@jupyter-sync)

;; -------------------------------------------------------------------
;;; Setup 

(defun nvp-jupyter-start-notebook ()
  ;; start inotebook so can load .ipynb instead of json file
  (and (executable-find "jupyter")
       (start-process "jupyter" "*jupyter*" "jupyter" "notebook" "--no-browser"))
  ;; (add-hook 'kill-buffer-hook 'nvp-jupyter-kill-processes nil 'local)
  )

;; shared setup stuff for ein hooks, if START is non-nil then setup
;; proper python and start jupyter server
(defun nvp-jupyter-setup-common (&optional start)
  (nvp-setup-local "python" :abbr-table "python-mode")
  (nvp:with-w32
    ;; setups up local variables and adds directories containing
    ;; python and ipython programs to exec-path
    (nvp-python-add-paths (nvp:program "python") (nvp:program "ipython")))
  (when start
    ;; sci has matplotlib/pyplot etcetc.
    (add-hook 'conda-env-after-activate-hook #'nvp-jupyter-start-notebook nil t)
    (conda-env-activate "sci")
    ;; (nvp-jupyter-start-notebook)
    ))

(provide 'nvp-jupyter)
;;; nvp-jupyter.el ends here
