;;; nvp-dockerfile.el --- dockerfile mods -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(when (treesit-ready-p 'dockerfile)
  (require 'dockerfile-ts-mode))
(nvp:decls :p (dockerfile-ts treesit))

(defface nvp-dockerfile-shell-face
  `((t (:inherit (nvp-block-face) :extend t)))
  "Face for \\='RUN shell commands.")

(defun dockerfile-ts--treesit-language-at-point (point)
  (let ((node (treesit-node-at point 'dockerfile)))
    (if (equal (treesit-node-type node) "shell_command")
        'bash
      'dockerfile)))

(defvar dockerfile-ts--treesit-range-rules
  (treesit-range-rules
   :embed 'bash
   :host 'dockerfile
   :local t
   '((shell_command) @bash)))

;;; Font-locking
;; XXX(9/25/23): Update when/if builtin mode is updated to account for grammar
;; change
(defvar nvp-dockerfile-ts--embedded
  (treesit-font-lock-rules
   :language 'dockerfile
   :feature 'shell
   :override 'append
   '((shell_command) @nvp-dockerfile-shell-face)))

(defvar nvp-dockerfile-ts--font-settings
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
   :feature 'escape-sequence
   :override 'append
   '((escape_sequence) @font-lock-escape-face
     (line_continuation) @nvp-line-escape-face)))

(defvar nvp-dockerfile-ts--features
  (cons 'shell (mapcar (lambda (e) (nth 2 e)) nvp-dockerfile-ts--font-settings)))

;; override `dockerfile-ts-mode--font-lock-settings'
(defvar sh-mode--treesit-settings)
(with-eval-after-load 'dockerfile-ts-mode
  (setq nvp-dockerfile-ts--font-settings
        (seq-uniq
         (append nvp-dockerfile-ts--font-settings
                 (seq-filter
                  (lambda (e) (not (memq (nth 2 e) nvp-dockerfile-ts--features)))
                  dockerfile-ts-mode--font-lock-settings))))

  (when (treesit-ready-p 'bash)
    (require 'sh-script)
    (setq nvp-dockerfile-ts--font-settings
          (append nvp-dockerfile-ts--font-settings sh-mode--treesit-settings))))

(defun nvp-dockerfile-ts-enable ()
  (when (treesit-ready-p 'dockerfile)
    (when (treesit-ready-p 'bash)
      (setq-local treesit-language-at-point-function
                  #'dockerfile-ts--treesit-language-at-point)
      (setq-local treesit-range-settings dockerfile-ts--treesit-range-rules))

    (setq-local treesit-font-lock-feature-list
                `(( function string comment ,@nvp-dockerfile-ts--features)
                  ( keyword command declaration-command string-interpolation)
                  ( builtin-variable constant heredoc variable assignment function)
                  ( number bracket delimiter operator misc-punctuation)))
    (setq-local treesit-font-lock-settings
                (append nvp-dockerfile-ts--font-settings nvp-dockerfile-ts--embedded))
    ;; update enabled features
    (treesit-font-lock-recompute-features)))

(provide 'nvp-dockerfile)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-dockerfile.el ends here
