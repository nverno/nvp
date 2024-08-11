;;; nvp-c-install.el --- install -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-c)
(nvp:decls :p (irony))

(nvp:package-define-root :name nvp-c)

(defconst nvp-c-gen-vars-script
  (nvp:program "gen-c-vars.sh" :path (expand-file-name "emacs" nvp/bin)))

;; Generate site-specific include paths, flags, etc.
(defun nvp-c-gen-site-vars (&optional arg)
  (let ((includes (expand-file-name "nvp-c-vars.el" nvp-c--dir))
        (prog nvp-c-gen-vars-script))
    (when (or arg
              (not (file-exists-p includes))
              (nvp:file-older-than-days includes 20))
      (nvp:with-process "bash"
        :proc-buff " *c-vars*"
        :proc-args (prog "-o" includes)
        :callback (lambda (p _m)
                    (when (zerop (process-exit-status p))
                      (load includes)
                      (message "loaded %s" includes))
                    (kill-buffer (process-buffer p)))))))

;; Make includes.el and install dependencies or dont with NODEPS
;; Force includes.el refresh with ARG
;;;###autoload
(defun nvp-c-install (command &optional force)
  (interactive (list 'vars current-prefix-arg))
  (cl-case command
    (vars (nvp-c-gen-site-vars force))
    (irony (when (require 'irony nil t)
             (call-interactively #'irony-install-server)))
    (t)))

(provide 'nvp-c-install)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-c-install.el ends here
