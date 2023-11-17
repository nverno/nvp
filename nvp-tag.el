;;; nvp-tag.el --- tagging utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Various options:
;; - etags bundled w/ emacs (basically just elisp/C/C++)
;; - ctags
;; - universal-ctags
;; - Python pygments parsers - many languages
;; - GNU global w/ universal-ctags + pygments
;;
;; Bugs (3/6/20):
;; - ggtags.el: tags starting with regex characters cause errors, eg.
;;   ggtags-process-string("global" "-c" "$0") =>
;;   global error: "only name char is allowed with -c option"
;;   Maintainer of ggtags.el says this should be fixed in global, see issue:
;;   https://github.com/leoliu/ggtags/issues/191
;;   Workaround `nvp-ggtags-bounds-of-tag-function' for now.
;;
;; References for developing customizable templated commands:
;; - grep #<marker at 25028 in grep.el.gz>
;; - format-spec.el (builtin)
;; - transient (used by magit/rg)
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(require 'nvp)
(nvp:decls :p (ggtags))

(defvar nvp-tags-ctags-program (nvp:program "ctags"))

;; workaround ggtags.el/global not accepting regex chars
(defun nvp-ggtags-bounds-of-tag-function ()
  (-when-let ((beg . end) (find-tag-default-bounds))
    (save-excursion
      (goto-char beg)
      (skip-chars-forward "?$*+^\\." end) ; so no idents prefixed with '$' or '.'
      (cons (point) end))))

;;;###autoload
(defun nvp-tag-list-decls (&optional lang kinds file force)
  "List decls defined by language LANG of type KINDS from current buffer or
FILE if non-nil. If FORCE, force interpretation as LANG."
  (nvp:defq file (nvp:path 'bf) kinds "*")
  (->> (apply #'process-lines
              (delq nil
                    (list
                     nvp-tags-ctags-program "-x"
                     (and force lang (not (string= lang "all"))
                          (format "--language-force=%s" lang))
                     (and lang kinds (format "--kinds-%s=%s" lang kinds))
                     file)))
       (mapcar (lambda (s)
                 (--when-let (cadr (split-string s file t " "))
                   (string-trim-left (replace-regexp-in-string "[ \t;{]*$" "" it)))))
       (delq nil)))

(defun nvp-tag-list-language-config (lang)
  "List ctags language configuration for LANG."
  (interactive
   (list (completing-read
          "Language: "
          (process-lines nvp-tags-ctags-program "--list-languages") nil t)))
  (nvp:with-results-buffer :buffer (concat "*Help[ctags-" lang "]*")
    :action :none
    (dolist (cmd '("maps" "kinds-full" "fields" "features" "extras"
                   "roles" "params" "aliases" "subparsers"))
      (insert (propertize cmd 'face 'bold-italic))
      (insert "\n")
      (save-excursion
        (call-process nvp-tags-ctags-program nil (current-buffer) nil
                      (concat "--list-" cmd "=" lang)))
      (add-text-properties (point) (point-max) '(face 'font-lock-comment-face))
      (goto-char (point-max)))
    (pop-to-buffer (current-buffer))))

(nvp:decl tags-reset-tags-tables projectile-regenerate-tags)

;;;###autoload(autoload 'nvp-tag-menu "nvp-tag")
(transient-define-prefix nvp-tag-menu ()
  [ :if-non-nil tags-file-name
    "Tags"
    ("a" "Apropos" tags-apropos)
    ("s" "Search" tags-search)
    ("f" "List file tags" list-tags)
    ("q" "Query replace regexp" tags-query-replace)]
  [["Tables"
    ("l" "Load table" visit-tags-table)
    ("c" "Choose table" select-tags-table :if-non-nil tags-table-set-list)
    ("R" "Regen project tags" projectile-regenerate-tags)
    ("K" "Reset tables" tags-reset-tags-tables :if-non-nil tags-file-name)]
   ["Help"
    ("hl" "List language config" nvp-tag-list-language-config)]]
  ;; settings
  ;; `tags-apropos-verbose' list filenames
  )

(provide 'nvp-tag)
;;; nvp-tag.el ends here
