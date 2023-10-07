;;; nvp-dockerfile.el --- dockerfile mods -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls :p (dockerfile-ts treesit))

;;; Font-locking

(defface nvp-dockerfile-shell-face `((t :inherit (nvp-block-face)))
  "Face for \\='RUN shell commands.")

(defvar dockerfile-ts--treesit-range-rules
  (when (treesit-available-p)
    (treesit-range-rules
     :embed 'bash
     :host 'dockerfile
     '((shell_command) @bash))))

(defun dockerfile-ts--treesit-language-at-point (point)
  (let ((node (treesit-node-at point 'dockerfile)))
    (if (equal (treesit-node-type node) "shell_command")
        'bash
      'dockerfile)))

;; Inject bash parser for shell commands
(defun dockerfile-ts--setup-bash ()
  (when (treesit-ready-p 'bash)
    (require 'sh-script)
    (treesit-parser-create 'bash)
    (setq-local treesit-language-at-point-function
                #'dockerfile-ts--treesit-language-at-point)
    (setq-local treesit-range-settings dockerfile-ts--treesit-range-rules)
    (setq-local treesit-font-lock-settings
                (append
                 sh-mode--treesit-settings
                 treesit-font-lock-settings))
    (setq-local treesit-font-lock-feature-list
                (--zip-with (seq-uniq (append it other))
                            treesit-font-lock-feature-list
                            '(( comment function)
                              ( command declaration-command keyword string)
                              ( builtin-variable constant heredoc number
                                string-interpolation variable)
                              ( bracket delimiter misc-punctuation operator))))))

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
             value: (_) @font-lock-constant-face)
            (arg_instruction
             name: (_) @font-lock-variable-name-face)
            (label_pair
             key: (_) @font-lock-property-name-face)
            ;; (param
            ;;  "--" _ @font-lock-variable-name-face)
            ;; (mount_param
            ;;  "mount" _ @font-lock-variable-name-face)
            ;; (mount_param_param _ @font-lock-variable-name-face)
            )

          :language 'dockerfile
          :feature 'shell
          :override 'append
          '((shell_command) @nvp-dockerfile-shell-face)

          :language 'dockerfile
          :feature 'escape-sequence
          :override 'append
          '((escape_sequence) @font-lock-escape-face
            (line_continuation) @nvp-line-escape-face))))
    
    (setq dockerfile-ts-mode--font-lock-settings (append new-rules cur-rules))))


(provide 'nvp-dockerfile)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-dockerfile.el ends here
