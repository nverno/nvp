;;; nvp-dockerfile.el --- dockerfile mods -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls :p (dockerfile-ts treesit))

;;; Font-locking

(defface nvp-dockerfile-shell-face `((t :inherit (nvp-block-face) :extend t))
  "Face for \\='RUN shell commands.")

;; XXX(9/25/23): Update when/if builtin mode is updated to account for grammar
;; change
(with-eval-after-load 'dockerfile-ts-mode
  (dolist (c '(?$ ?/))
    (modify-syntax-entry c "." dockerfile-ts-mode--syntax-table))

  ;; Note: `variable', `shell', `escape-sequence' features not defined in
  ;; dockerfile-ts, so need to add them to `treesit-font-lock-feature-list' in
  ;; hook
  (let ((cur-rules
         (seq-filter
          (lambda (e)
            (not (memq (caddr e) '(string image-spec variable shell escape-sequence))))
          dockerfile-ts-mode--font-lock-settings))
        (new-rules
         (treesit-font-lock-rules
          :language 'dockerfile
          :feature 'string
          '([(double_quoted_string) (single_quoted_string) (json_string)]
            @font-lock-string-face)

          :language 'dockerfile
          :feature 'image-spec
          '((image_spec) @font-lock-type-face
            (image_alias) @font-lock-variable-name-face)
          
          :language 'dockerfile
          :feature 'variable          ; feature not defined, need to add in hook
          '((env_pair
             name: (_) @font-lock-variable-name-face
             value: (_) @font-lock-constant-face))

          :language 'dockerfile
          :feature 'shell
          '((shell_command) @nvp-dockerfile-shell-face)

          :language 'dockerfile
          :feature 'escape-sequence
          :override t
          '((escape_sequence) @font-lock-escape-face))))
    
    (setq dockerfile-ts-mode--font-lock-settings (append new-rules cur-rules))))


(provide 'nvp-dockerfile)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-dockerfile.el ends here