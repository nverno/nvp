;;; nvp-wasm.el --- webassembly -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls)

(defvar nvp-wasm-js-modes '(typescript-mode typescript-ts-mode js-mode js-ts-mode))

;;;###autoload
(defun nvp-wasm-compile (&optional save)
  "Compile to wasm/wat.
With prefix SAVE, dont cleanup wasm/wat files."
  (interactive (list current-prefix-arg))
  (let* ((src (buffer-file-name))
         (base (f-base src))
         (wasm (concat base ".wasm"))
         (wat (concat base ".wat"))
         (bufname (format "*wasm-compile[%s]*" (f-filename base)))
         (save save)
         (compile-command
          (cond
           ((memq major-mode nvp-wasm-js-modes)
            (format "asc %s -o %s -t %s" src wasm wat))
           (t (user-error "Unimplemented for %S" major-mode)))))
    (when compile-command
      (cl-labels ((cleanup-wasm (&optional not-buf force)
                    (when (or force (not save))
                      (dolist (file (list wat wasm))
                        (and (file-exists-p file)
                             (delete-file file))))
                    (unless not-buf
                      (kill-buffer bufname))))
        (condition-case err
            (let ((buf (with-current-buffer (get-buffer-create bufname)
                         (erase-buffer)
                         (compilation-minor-mode))))
              (if (and (zerop
                        (call-process-shell-command compile-command nil buf nil))
                       (file-exists-p wat))
                  (with-current-buffer (find-file-other-window wat)
                    (add-hook 'kill-buffer-hook #'cleanup-wasm nil t))
                (cleanup-wasm 'not-buf)
                (pop-to-buffer bufname)))
          (error (cleanup-wasm nil 'force)
                 (error (error-message-string err))))))))

(eval-when-compile
  (cl-defmacro nvp:with-treesit-sexp (node &rest body
                                           &key bounds (type "instr")
                                           &allow-other-keys)
    (declare (indent defun))
    (nvp:skip-keywords body)
    `(when-let (it (treesit-parent-until ,node (lambda (n) (treesit-node-match-p n ,type))))
       (,@(if bounds `(let ((beg (treesit-node-start it))
                            (end (treesit-node-end it))))
            '(progn))
        ,@body))))

(defun nvp-wat-wrap-instruction (node)
  "Wrap instruction NODE with parens."
  (interactive (list (treesit-node-at (point))))
  (nvp:with-treesit-sexp node :bounds t
    (save-excursion
      (goto-char end)
      (insert ")")
      (goto-char beg)
      (insert "("))))

(defun nvp-wat-splice (node)
  "Splice parens surround expression NODE."
  (interactive (list (treesit-node-at (point))))
  (nvp:with-treesit-sexp node :bounds t :type "expr\\'"
    (save-excursion (goto-char (1- end))
                    (delete-char 1)
                    (goto-char beg)
                    (delete-char 1))))

(provide 'nvp-wasm)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-wasm.el ends here
