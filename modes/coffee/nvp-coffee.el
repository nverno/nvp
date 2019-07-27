;;; nvp-coffee.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;; - https://github.com/purcell/flymake-coffee
;; - https://github.com/jashkenas/coffeescript
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))

;; -------------------------------------------------------------------
;;; Utils

;; FIXME: replace
(defun nvp-coffee-temporary-keybinding (key cmd &optional func)
  "Temporarily bind KEY to CMD to alternate back to '_'."
  (set-transient-map
   (let ((tmap (make-sparse-keymap)))
     (define-key tmap (kbd key) cmd)
     tmap)
   (or func nil)))

;; -------------------------------------------------------------------
;;; Interactive

;;; Lint

(defun nvp-coffee-lint ()
  (interactive)
  (if (executable-find "coffeelint")
      (let ((compile-command
             (concat "coffeelint --color=never " buffer-file-name))
            (compilation-read-command nil))
        (call-interactively 'compile))
    (user-error "coffeelint not fount, need to install")
    ;; (nvp-coffee-install)
    ))

;;; Arrow

;; alternate between '_' and ' -> '
(defun nvp-coffee-arrow-undo ()
  "Insert \"_\" instead."
  (interactive)
  (delete-char -4)
  (insert "_"))

(defun nvp-coffee-arrow-dwim ()
  "Insert '-> ' instead of '_' when not in string or comment.
If '_' is hit again, insert '_' instead (like in `ess-mode')."
  (interactive)
  (save-restriction
    (let ((s (syntax-ppss)))
      (if (and (not (or (nth 3 s) (nth 4 s))))
          (progn
            (insert " -> ")
            (nvp-coffee-temporary-keybinding "_" #'nvp-coffee-arrow-undo))
        (insert "_")))))

(provide 'nvp-coffee)
;;; nvp-coffee.el ends here
