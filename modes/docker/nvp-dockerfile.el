;;; nvp-dockerfile.el --- dockerfile mods -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'dockerfile-ts-mode nil t)
(nvp:decls :p (dockerfile-ts treesit))


(defface nvp-dockerfile-shell-face
  `((t (:inherit (nvp-block-face) :extend t)))
  "Face for \\='RUN shell commands."
  :group 'nvp)

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


;;; Bash Embedded Parser

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

;; From `bash-ts-mode' - not defined as variable
(defvar bash-ts-mode-feature-list
  '(( comment function)
    ( command declaration-command keyword string)
    ( builtin-variable constant heredoc number
      string-interpolation variable)
    ( bracket delimiter misc-punctuation operator)))
(defvar sh-mode--treesit-settings)

(defun nvp-dockerfile-ts-maybe-enable-bash ()
  (when (treesit-ready-p 'bash)
    (setq-local treesit-language-at-point-function
                #'dockerfile-ts--treesit-language-at-point)
    (setq-local treesit-range-settings dockerfile-ts--treesit-range-rules)))


(nvp:treesit-add-rules dockerfile-ts-mode
  :mode-fonts dockerfile-ts-mode--font-lock-settings
  :new-fonts nvp-dockerfile-ts--font-settings
  :extra-features '(shell)

  ;; Add bash parser stuff - runs once after first `dockerfile-ts-mode'
  ;; Note: `nvp-dockerfile-ts--embedded' should come last
  (let ((new-fonts nvp-dockerfile-ts--embedded))
    (when (treesit-ready-p 'bash)
      (require 'sh-script)
      (setq new-fonts (append sh-mode--treesit-settings new-fonts))

      ;; Merge in all bash features
      (cl-loop for x in bash-ts-mode-feature-list
               for y in treesit-font-lock-feature-list
               for i upto 4
               do (setf (nth i treesit-font-lock-feature-list)
                        (seq-uniq (append x y)))))

    (setq dockerfile-ts-mode--font-lock-settings
          (append dockerfile-ts-mode--font-lock-settings new-fonts))))


(provide 'nvp-dockerfile)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-dockerfile.el ends here
