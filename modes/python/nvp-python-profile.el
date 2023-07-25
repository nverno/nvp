;;; nvp-python-profile.el --- profile python code -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'compile)
(nvp:decls)

(define-compilation-mode pyprofile-mode "PyProfile"
  "Compilation mode to view python profile output from cProfile,
line_profiler, and memory_profiler."
  ;; XXX: filter out matches to non-local libraries
  (setq-local compilation-error-regexp-alist-alist
              '((cprofile "\\([^< 0-9][^: \n]+[^>]\\):\\([0-9]+\\)" 1 2 nil 0)
                (lprofile "File: \\([^\n]+\\)\n[^\n]+line \\([0-9]+\\)" 1 2 nil 0)))
  (setq-local compilation-error-regexp-alist '(cprofile lprofile)))

;;;###autoload
(defun nvp-python-profile (profiler &optional prompt)
  "Profile buffer with output to compilation buffer."
  (interactive
   (list (completing-read "Profiler"
                          '("cProfile" "line_profiler" "memory_profiler") nil t)
         current-prefix-arg))
  (let ((compile-command
         (concat
          (pcase profiler
            ("cProfile" "python -m cProfile -s tottime")
            ("line_profiler" "kernprof -l -v")
            ("memory_profiler" "python -m memory_profiler"))
          " " (buffer-file-name))))
    (compilation-start
     (if (or prompt compilation-read-command)
         (compilation-read-command compile-command)
       compile-command)
     'pyprofile-mode)))

;;;###autoload
(defun nvp-python-callgraph ()
  "Generate call graph for current buffer."
  (interactive)
  (unless (and (executable-find "pycallgraph")
               (executable-find "dot"))
    (user-error "Missing pycallgraph and/or dot programs"))
  (let ((exit-status
         (call-process-shell-command
          (concat "pycallgraph graphviz -- ./" (nvp:bfn)))))
    (if (zerop exit-status)
        (find-file-other-window "./pycallgraph.png")
      (user-error "pycallgraph exit status: %s" exit-status))))

(provide 'nvp-python-profile)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-python-profile.el ends here
