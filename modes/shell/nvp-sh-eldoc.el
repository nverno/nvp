;;; nvp-sh-eldoc.el --- eldoc for bash/sh  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Notes:
;; - help strings are cached
;; - bash builtins run synchronously
;; - others run async, so help will show up after moving point
;;   in region more than once
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-shell-common)
(nvp:req 'nvp-shell 'subrs)
(require 'nvp-sh-help)
(require 'eldoc)

(defvar nvp-sh-eldoc-cache (make-hash-table :test 'equal))

;; return formatted doc string for bash builtins
(defun nvp-sh-eldoc-builtin-string (cmd)
  (or (gethash cmd nvp-sh-eldoc-cache)
      (let ((str (nvp-sh-bash-builtin-help-sync cmd 'synopsis)))
        ;; remove 'cmd: ' and trailing newline
        (setq str (substring str (+ 2 (length cmd)) (1- (length str))))
        ;; propertize CMD
        (add-text-properties
         0 (length cmd)
         (list 'face 'font-lock-function-name-face) str)
        (puthash cmd str nvp-sh-eldoc-cache))))

;; get synopsis from man output asynchronously and cache it
(defun nvp-sh-eldoc--man (cmd)
  (nvp-sh:with-man-help cmd nil
    (goto-char (point-min))
    (ignore-errors
      (when (search-forward "SYNOPSIS")
        (while (not (looking-at-p (concat "\\([ \t]+" cmd "\\|^[^ \t]\\)")))
          (forward-line))
        (skip-chars-forward " \t")
        ;; put result in cache
        (puthash
         cmd 
         (concat
          (propertize cmd 'face 'font-lock-function-name-face) ": "
          (and (looking-at "[^ \t]+[ \t]+\\([^\n]+\\)")
               (match-string 1))
          ;; (buffer-substring
          ;;  (+ (length cmd) (point)) (point-at-eol))
          )
         nvp-sh-eldoc-cache)
        (erase-buffer)))))

;; get doc string from man
(defun nvp-sh-eldoc-man-string (cmd)
  (or (gethash cmd nvp-sh-eldoc-cache)
      (ignore (nvp-sh-eldoc--man cmd))))

;;;###autoload
(defun nvp-sh-eldoc-function ()
  "Return eldoc string for bash functions (builtins and those avaliable from
`man %s'."
  (let ((func (nvp-sh-current-command)))
    (and func
         (nvp-sh:with-bash/man func
             (nvp-sh-eldoc-builtin-string func) ;; synchronously
           (nvp-sh-eldoc-man-string func))))) ;; async

(provide 'nvp-sh-eldoc)
;;; nvp-sh-eldoc.el ends here
