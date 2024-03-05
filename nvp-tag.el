;;; nvp-tag.el --- tagging utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(eval-when-compile (require 'nvp-macro))
(require 'nvp)                          ; `nvp-tags-ctags-program'
(require 'transient)
(nvp:decls :p (ggtags tags projectile) :v (nvp-tags-ctags-program))

;;;###autoload
(defun nvp-tag-list-decls (&optional lang kinds file force)
  "List decls defined by language LANG of type KINDS from current buffer or
FILE if non-nil. If FORCE, force interpretation as LANG."
  (nvp:defq file (nvp:path 'bf) kinds "*")
  (->> (apply #'process-lines
              (delq nil (list
                         nvp-tags-ctags-program "-x"
                         (and force lang (not (string= lang "all"))
                              (format "--language-force=%s" lang))
                         (and lang kinds (format "--kinds-%s=%s" lang kinds))
                         file)))
       (mapcar (lambda (s)
                 (--when-let (cadr (split-string s file t " "))
                   (string-trim-left
                    (replace-regexp-in-string "[ \t;{]*$" "" it)))))
       (delq nil)))

(defun nvp-tag-show-language-config (lang)
  "Show Ctags configuration for LANG."
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

;; Apply FN to project excludes / tags file in project root
(defun nvp-tag--run-with-project (fn)
  (let ((default-directory (projectile-acquire-root))
        (tags-exclude (projectile-tags-exclude-patterns))
        (project-tags-file (expand-file-name projectile-tags-file-name)))
    (funcall fn project-tags-file tags-exclude)))

(defun nvp-tag-show-file-languages ()
  "Show ctags guessed language for all files in project."
  (interactive)
  (nvp:with-tabulated-list
    :name "ctags-languages"
    :format [("File" 50 t) ("Language" 15 t)]
    :entries
    (nvp-tag--run-with-project
     (lambda (file excludes)
       (let ((cmd (format projectile-tags-command
                          file (concat " --print-language " excludes) "."))
             res)
         (with-temp-buffer
           (unless (zerop (process-file-shell-command cmd nil (current-buffer)))
             (user-error
              (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
           (goto-char (point-min))
           (while (re-search-forward "^\\([^:]+\\): \\(\\S-+\\)" nil t)
             (push (list (match-string 1) `[,(match-string 1) ,(match-string 2)])
                   res)
             (forward-line 1))
           res))))
    (setq tabulated-list-sort-key '("Language" . nil))))

;;;###autoload(autoload 'nvp-tag-menu "nvp-tag" nil t)
(transient-define-prefix nvp-tag-menu ()
  [ :if-non-nil tags-file-name
    "Tags"
    ("a" "Apropos" tags-apropos)
    ("s" "Search" tags-search)
    ("f" "List file tags" list-tags)
    ("q" "Query replace regexp" tags-query-replace)]
  [["Tables"
    ("l" "Load table" visit-tags-table)
    ("c" "Choose table" select-tags-table)
    ("R" "Re/gen project tags" projectile-regenerate-tags)
    ("K" "Reset tables" tags-reset-tags-tables :if-non-nil tags-file-name)]
   ["Help"
    ("hg" "Show ctags language for files" nvp-tag-show-file-languages)
    ("hl" "Show ctags language config" nvp-tag-show-language-config)]])
;; settings
;; `tags-apropos-verbose' list filenames

(provide 'nvp-tag)
;;; nvp-tag.el ends here
