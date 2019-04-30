;;; nvp-makefile.el --- make helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; make -q foo => exit 0 if foo is up-to-date

;; TODO:
;; - font-lock-doc-face for info/warning/error?
;; - incorporate semantic stuff?
;; - use info-completition-at-point function
;;   #<marker at 25312 in info-look.el.gz>
;; - align rules => similar to sh-mode rules

;; FIXME:
;; - beginning/end of defun functions don't work
;; - collect remote info async

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (require 'subr-x))
(require 'nvp)
(require 'make-mode)
(nvp-decls)

;; -------------------------------------------------------------------
;;; Things-at-point

(defun nvp-makefile-bounds-of-macro-at-point ()
  (save-excursion
    (skip-chars-backward "^$)}:#= \t\n" ())))

;; -------------------------------------------------------------------
;;; Font-lock
;; TODO:
;; - info/warn/error
;; - remove string fontification in #define blocks where it is incorrect.

;; ------------------------------------------------------------
;;; Web topics

;; FIXME: run async
;; collect matches from url
(defun nvp-makefile-collect-topics (url regex)
  (let (res)
    (nvp-while-scanning-url url regex
      (push (match-string-no-properties 1) res))
    res))

(nvp-define-cache-runonce nvp-makefile-special-targets ()
  "List of special make targets."
 ;; propertize :manual (concat url (match-string 1))
 (nvp-makefile-collect-topics
  "https://www.gnu.org/software/make/manual/html_node/Special-Targets.html"
  "dt[>< ]+code[<> ]+\\([.A-Za-z]+\\)"))

;; -------------------------------------------------------------------
;;; General mode variables

;; FIXME: these don't work correctly at all
(defun nvp-makefile-beginning-of-defun-function (&optional arg)
  "See `beginning-of-defun'.
Treats target blocks as defuns."
  (when (or (null arg) (= 0 arg)) (setq arg 1))
  (while (and (not (= 0 arg))
              (makefile-previous-dependency))
    (setq arg (if (> arg 0) (1- arg) (1+ arg)))))

(defun nvp-makefile-end-of-defun-function ()
  "See `end-of-defun'.
Skips to end of tabbed block."
  (forward-line 1)                   ;called when point is at beginning of block
  (while (looking-at-p "^\t")
    (forward-line 1)))

;; -------------------------------------------------------------------
;;; Indent 

(defvar nvp-makefile-indent-offset 2)

;; indent ifeq ... endif regions
(defun nvp-makefile-indent ()
  (save-excursion
    (goto-char (point-min))
    ;; get first rule
    (let ((end (save-excursion
                 (progn
                   (re-search-forward makefile-dependency-regex nil t)
                   (point)))))
      (while (search-forward "ifeq" end 'move)
        ;; indent if block
        (forward-line 1)
        (let ((close (save-excursion
                       (search-forward "endif")
                       (line-number-at-pos))))
          (while (< (line-number-at-pos) close)
            (beginning-of-line)
            (unless (looking-at-p "\\s-*else")
              (delete-horizontal-space)
              (indent-to nvp-makefile-indent-offset))
            (forward-line 1)))))))

;; -------------------------------------------------------------------
;;; Font-locks
;; `makefile-dependency-regex' => note this doesn't take into account quoting
;; `makefile-macroassign-regex' => doesn't handle #defines

;; (let ((io-funs
;;        (concat "\\${\\s-*" (regexp-opt '("info" "warn" "error"))
;;                "\\(.*\\)}")))
;;   `(nvp-font-lock-add-defaults 'makefile-mode
;;      ("\\${\\s-*}")
;;     ))

;; ------------------------------------------------------------
;;; Hooks

;; cleanup buffer before save
(defun nvp-makefile-cleanup-buffer ()
  (unless (or buffer-read-only (not (buffer-modified-p)))
    ;; fixup indent
    (nvp-makefile-indent)
    ;; align [?:]= before first rule
    ;; (align (point-min) (point-max))     ;use builtin align rules for now
    ;; (let ((end (save-excursion
    ;;              ;; find first rule
    ;;              (progn (goto-char (point-min))
    ;;                     (re-search-forward "^[^ ]+:" nil t)
    ;;                     (point)))))
    ;;   (align-regexp (point-min) end (nvp-concat
    ;;                                  "\\(?:[^<?]\\)\\(\\s-*\\)"
    ;;                                  "\\(=\\|[:?+]=\\)")
    ;;                 1))
    ;; align trailing '\'
    ;; (align-regexp (point-min) (point-max) "\\(\\s-*\\)\\\\\\s-*$")
    ))

(provide 'nvp-makefile)
;;; nvp-makefile.el ends here
