;;; nvp-go-help.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;; help-at-point for go source
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-go)
(nvp:decls :v (godoc-command)
           :f (godoc-and-godef godoc-at-point godoc-gogetdoc godoc--read-query))

;; if non-nil use gtk tooltips
(defvar nvp-go-use-gtk nil)

;; -------------------------------------------------------------------
;;; Utils

(eval-when-compile
  ;; toggle help popup with pos-tip
  (cl-defmacro with-toggled-tip (&key help-doc type highlight)
    (declare (indent defun) (debug t))
    `(let ((x-gtk-use-system-tooltips nvp-go-use-gtk))
       (or (x-hide-tip)
           (let ((str ,help-doc))
             ;; FIXME: pos-tip can't display font-lock? or losing
             ;;        properties somehow
             ;; if HIGHLIGHT: when TYPE is non-nil and matches STR,
             ;; then propertize the match in STR
             ,(when highlight
                `(when ,type (string-match ,type str)
                       (add-text-properties
                        (match-beginning 0) (match-end 0)
                        (list 'face 'font-lock-warning-face) str)))
             (nvp-pos-tip-show str nil nil nil 10))))))

;; -------------------------------------------------------------------
;;; Help at point

;;; godoc functions
;; redefine so they dont pop to buffers
;; hijack `godoc-gogetdoc'
(defun nvp-go-help-gogetdoc-at-point (point)
  (interactive "d")
  (cl-letf (((symbol-function 'display-buffer)
             #'(lambda (&rest _ignored)
                 (replace-regexp-in-string "[\r\n]+" "" (buffer-string)))))
    (with-toggled-tip :help-doc (godoc-gogetdoc point))))

;; call godoc synchronously for popups
(defun nvp-go-help--godoc (query command)
  (interactive (list (godoc--read-query) godoc-command))
  (call-process-shell-command
   (concat command " " query) nil "*go-help*")
  (with-current-buffer "*go-help*"
    (prog1 (replace-regexp-in-string "[\n\r]+$" "" (buffer-string))
      (kill-buffer))))

(defun nvp-go-help-godoc-at-point (point)
  (interactive "d")
  (cl-letf (((symbol-function 'go--godoc) 'nvp-go-help--godoc))
    (with-toggled-tip :help-doc (godoc-and-godef point))))

;; temporarily capture `package.function' as symbol
(defvar nvp-go-help-symbol-syntax
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "_" st)
    st))

(defvar nvp-go-type-re "")

;;;###autoload
(defun nvp-go-help-at-point (arg)
  (interactive "P")
  (or (x-hide-tip)
      (with-syntax-table nvp-go-help-symbol-syntax
        (let* ((bnds (bounds-of-thing-at-point 'symbol))
               (sym (thing-at-point 'symbol)))
          (cond
           ((null sym)
            (call-interactively 'godoc))
           ;; Formatted strings
           ;; ((nth 3 (syntax-ppss))
           ;;  (save-excursion
           ;;    ;; back out of string and function call
           ;;    (ignore-errors (up-list -2 t))
           ;;    ;; if we in string in formatting function
           ;;    (and (string-match-p
           ;;          "\\(?:fmt\\|log\\).*f$" (thing-at-point 'symbol))
           ;;         (nvp-go-help-string-format))))
           ((string-match-p nvp-go-type-re sym))
           ;; call popup versions of godoc or gogetdoc
           ;; use point at end of symbol so fm|t.Printf returns help for
           ;; Printf
           (t
            (if arg
                (funcall-interactively
                 #'nvp-go-help-gogetdoc-at-point (1- (cdr bnds)))
              (funcall-interactively
               #'nvp-go-help-godoc-at-point (1- (cdr bnds))))))))))

(provide 'go-help)
;;; nvp-go-help.el ends here
