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

(provide 'nvp-wasm)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-wasm.el ends here
