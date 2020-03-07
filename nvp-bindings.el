;;; nvp-bindings.el --- overriding bindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Overriding global and buffer-local bindings
;; refs: evil, general, allout.el, modalka
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)

;; -------------------------------------------------------------------
;;; Override bindings

;; see general.el
(defvar nvp-override-mode-map (make-sparse-keymap)
  "Keymap to take priority over other minor mode keymaps.")

(defvar-local nvp-override-local-mode-map nil
  "Local keymap to take priority over `nvp-override-mode-map'.")
(put 'nvp-override-local-mode-map 'permanent-local t)

;; `company-emulation-alist' #<marker at 34809 in company.el>
;; `global-override-mode' in bind-key
(define-minor-mode nvp-override-mode
  "Minor mode to enable overriding key bindings."
  :lighter ""
  :global t
  :keymap nvp-override-mode-map)

;; -------------------------------------------------------------------
;;; Auto unbind prefix keys
;; problems - when multiple keys being defined with shared prefix they all
;; get overwritten except the last one

;; https://github.com/noctuid/general.el/blob/master/general.el
;; see `general-unbind-non-prefix-key'
(defun nvp-unbind-non-prefix-key (define-key keymap key def)
  (and (stringp key) (setq key (string-to-vector key)))
  (while (numberp (lookup-key keymap key))
    (setq key (cl-subseq key 0 -1)))
  (funcall define-key keymap key nil)
  (funcall define-key keymap key def))

;; see `general-auto-unbind-keys'
(defun nvp-auto-unbind-keys (&optional undo)
  "Advices `define-key' to auto unbind keys that are subsequences of new
bindings, avoiding errors from key sequences starting with non-prefix keys."
  (if undo
      (nvp-unadvise-commands #'nvp-unbind-non-prefix-key #'define-key)
    (nvp-advise-commands #'nvp-unbind-non-prefix-key :around #'define-key)))

;; -------------------------------------------------------------------
;;; Local bindings functions - probably get rid of these 

;;;###autoload
(defun nvp-buffer-local-set-key (key cmd)
  "Like `local-set-key', but don't modify KEY in other buffers of same mode."
  (let ((lmap (make-sparse-keymap)))
    (set-keymap-parent lmap (current-local-map))
    (define-key lmap key cmd)
    (use-local-map lmap)))

;; https://stackoverflow.com/a/14769115/2415684
;;;###autoload
(defun nvp-local-set-minor-mode-key (mode key def)
  "Override a minor mode keybinding for the local buffer only using \
`minor-mode-overriding-map-alist'."
  (let* ((oldmap (cdr (assoc mode minor-mode-map-alist)))
         (newmap (or (cdr (assoc mode minor-mode-overriding-map-alist))
                     (let ((map (make-sparse-keymap)))
                       (set-keymap-parent map oldmap)
                       (push `(,mode . ,map) minor-mode-overriding-map-alist)))))
    (define-key newmap key def)))

(provide 'nvp-bindings)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-bindings.el ends here
