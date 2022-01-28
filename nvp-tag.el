;;; nvp-tag.el --- tagging utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Various options:
;; - etags bundled w/ emacs (basically just elisp/C/C++)
;; - ctags
;; - universal-ctags (better, actively maintained ctags fork, all languages)
;; - Python pygments parsers - for all languages
;; - GNU global: ***Best option (3/6/20)***
;;   can act as frontend using both universal-ctags + pygments
;;
;; Bugs (3/6/20):
;; - In configuration of global (~/.globalrc) -- I've found that using
;;   pygments-parser w/ custom ctags (added langdefs in universal-ctags parser)
;;   gives a warning about unknown langdef, but then still works -- see
;;   ~/dotfiles/code/.globalrc for my reasoning about why the warning occurs
;; - ggtags.el: tags starting with regex characters cause errors, eg.
;;   ggtags-process-string("global" "-c" "$0") =>
;;   global error: "only name char is allowed with -c option"
;;   Maintainer of ggtags.el says this should be fixed in global, see issue:
;;   https://github.com/leoliu/ggtags/issues/191
;;   Workaround `nvp-ggtags-bounds-of-tag-function' for now.
;;
;; XXX: is this of any interest anymore?
;; - https://github.com/jixiuf/ctags-update
;;
;; TODO:
;; - might be work adding support for something like dumb-jump/smart-jump for
;;   situations where global/universal-ctags aren't available
;; - Generic function to tag language specific source directories that may be
;;   outside of the current project root (system libraries, source installs, etc)
;; - Integrate better with projectile tagging
;;
;; References for developing customizable templated commands:
;; - grep #<marker at 25028 in grep.el.gz>
;; - format-spec.el (builtin)
;; - transient (used by magit/rg)
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)

;; workaround ggtags.el/global not accepting regex chars
(defun nvp-ggtags-bounds-of-tag-function ()
  (-when-let ((beg . end) (find-tag-default-bounds))
    (save-excursion
      (goto-char beg)
      (skip-chars-forward "?$*+^\\." end) ; so no idents prefixed with '$' or '.'
      (cons (point) end))))

;;; FIXME: none of this is currently used
(cl-defgeneric nvp-tag-command ()
  "Command to generate tags.")

;; find-tag default functions - see subr.el: #<marker at 125677 in subr.el.gz>
(defun nvp-tag-get-default ()
  (or (and (region-active-p)
           (/= (point) (mark))
           (buffer-substring-no-properties (point) (mark)))
      (funcall (or find-tag-default-function
                   (get major-mode 'find-tag-default-function)
                   'find-tag))))

;;; ctags
;;;###autoload
(defun nvp-tag-list-decls (&optional lang kinds file)
  "List decls defined by language LANG of type KINDS from current buffer or
FILE if non-nil."
  (nvp:defq file (nvp:path 'bf) lang "all" kinds "*")
  (->> (process-lines (nvp:program "ctags") "-x"
                      (if lang (format "--kinds-%s=%s" lang kinds) "--kinds-all")
                      file)
       (mapcar (lambda (s)
                 (string-trim-left
                  (replace-regexp-in-string
                   "[ \t;{]*$" ""
                   (cadr (split-string s file t " "))))))))

;; -------------------------------------------------------------------
;;; Using imenu

;; FIXME: What was this used for? Just remote tagging?
;; ;;;###autoload
;; (defun nvp-tag-directory-imenu (dir tagfile)
;;   "Use imenu regexp to call find .. | etags .. in shell command."
;;   (interactive "DDirectory to tag:
;; GTags file (default TAGS): ")
;;   (when (or (eq (length (file-name-nondirectory tagfile) 0))
;;             (file-directory-p tagfile))
;;     (setq tagfile (concat (file-name-as-directory tagfile) "TAGS")))
;;   (when (file-remote-p tagfile)
;;     (require 'tramp)
;;     (setq tagfile (with-parsed-tramp-file-name tagfile foo foo-localname)))
;;   (when (file-remote-p dir)
;;     (require 'tramp)
;;     (setq dir (with-parsed-tramp-file-name dir foo foo-localname)))
;;   (unless imenu-generic-expression
;;     (error "No `imenu-generic-expression' defined for %s" major-mode))
;;   (let* ((find-cmd
;;           (format "find %s -type f -size 1M \\( -regex \".*\\.\"")))))

;; (defun nvp-xref-find-etags ()
;;   (interactive)
;;   (let* ((xref-backend-functions '(etags--xref-backend))
;;          (thing (xref-backend-identifier-at-point 'etags)))
;;     (xref-find-definitions-other-window thing)))

(provide 'nvp-tag)
;;; nvp-tag.el ends here
