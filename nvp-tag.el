;;; nvp-tag.el --- tagging utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(eval-when-compile (require 'nvp-macro))
(require 'nvp)                          ; `nvp-tags-ctags-program'
(require 'transient)
(nvp:decls :p (ggtags tags projectile) :v (nvp-tags-ctags-program))

;;;###autoload
(defun nvp-tag-find (&optional etags)
  "Call `projectile-find-tag' or `nvp-tag-find-etag' with prefix ETAGS."
  (interactive "P")
  (call-interactively (if etags #'nvp-tag-find-etag #'projectile-find-tag)))

;;;###autoload
(defun nvp-tag-find-etag ()
  "Call `xref-find-definitions' using etags backend."
  (interactive)
  (let ((xref-backend-functions '(etags--xref-backend))
        (xref-prompt-for-identifier nil)
        (thing (xref-backend-identifier-at-point 'etags)))
    (xref-find-definitions thing)))

;; -------------------------------------------------------------------
;;; Ctags

(defsubst nvp-tag--read-ctag-language ()
  (completing-read
    "Language: "
    (process-lines nvp-tags-ctags-program "--list-languages") nil t))

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

;;;###autoload
(defun nvp-tag-show-ctags (lang kinds file &optional force)
  "Show Ctags for LANG KINDS in FILE.
FORCE language to be LANG when non-nil."
  (interactive
   (if current-prefix-arg
       (list (nvp-tag--read-ctag-language)
             (read-string "Kinds: " "*" nil "*")
             (let ((file (buffer-file-name)))
               (read-file-name
                "File: " nil file nil
                (and file (file-name-nondirectory file)))))
     (list nil "*" (buffer-file-name))))
  (let ((decls (--filter (not (string-empty-p it))
                         (nvp-tag-list-decls lang kinds file force))))
    (nvp:with-results-buffer :buffer (concat "*ctags[" (or lang "-") "]*")
      :title (format "Ctags for language=%s kinds=%s" lang kinds)
      :action :none
      (dolist (dec decls)
        (insert dec "\n"))
      (pop-to-buffer (current-buffer)))))

(defun nvp-tag-show-ctags-language-config (lang)
  "Show Ctags configuration for LANG."
  (interactive (list (nvp-tag--read-ctag-language)))
  (nvp:with-results-buffer :buffer (concat "*ctags-config[" lang "]*")
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

(defun nvp-tag-show-ctags-languages ()
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


;; -------------------------------------------------------------------
;;; Tag repos

(defun nvp-tag--repo-sentinel (proc source-dir &optional tag-file no-continue next)
  (set-process-sentinel
   proc (let ((source-dir source-dir)
              (tag-file tag-file)
              (no-continue no-continue)
              (next next))
          (lambda (p m)
            (unwind-protect
                (let ((msg (replace-regexp-in-string "\n" "" m)))
                  (if (eq 0 (process-exit-status p))
                      (funcall (or next #'nvp-tag-repo)
                               source-dir tag-file no-continue)
                    (error "%s: %s" (process-name p) msg)))
              (kill-buffer (process-buffer p)))))))

;;;###autoload
(defun nvp-tag-repo (source-repo source-dir &optional tag-file no-continue)
  "Clone SOURCE-REPO to SOURCE-DIR if it doesn't exist.
Create tags in TAGS-FILE unless it exists and load tags."
  (interactive
   (let ((default-directory nvp/devel))
     (list (read-string "Repo: ")
           (read-directory-name "Source direcory: "))))
  (unless no-continue
    (unless tag-file (setq tag-file "TAGS"))
    (let ((tags (expand-file-name tag-file source-dir)))
      (cond ((not (file-exists-p source-dir))
             (message "Cloning %s to %s..." source-repo source-dir)
             (nvp-tag--repo-sentinel
              (start-process
               "nvp-tags" "*nvp-tags*"
               "git" "clone" "--depth=1" source-repo source-dir)
              source-dir tag-file no-continue))
            ((not (file-exists-p tags))
             (message "Creating TAGS for %s" source-dir)
             ;; Dont let `projectile-regenerate-tags' load tags
             ;; so tags can be loaded locally with `etags--xref-backend' added
             (nvp:with-letf #'visit-tags-table #'ignore
               (let ((default-directory source-dir)
                     (projectile-tags-file-name tag-file))
                 (projectile-regenerate-tags)))
             (nvp-tag-repo source-repo source-dir tag-file))
            (t
             (add-to-list 'xref-backend-functions #'etags--xref-backend t)
             (visit-tags-table tags 'local))))))


;; -------------------------------------------------------------------
;;; Menu

(nvp:decl dired-file-name-at-point)

;;;###autoload
(defun nvp-tag-list-tags (file)
  "Display list of tags in FILE.
Wrapper around `list-tags' that:
 - Prompts for filename with prefix arg or when FILE isnt found in tags files.
 - In `dired-mode' uses `dired-file-name-at-point'"
  (interactive
   (if current-prefix-arg
       (list (completing-read
              "List file tags: " 'tags-complete-tags-table-file nil t))
     (let* ((src-file (cond ((eq 'dired-mode major-mode)
                             (dired-file-name-at-point))
                            (t (buffer-file-name))))
            (tag-dir (save-excursion
                       (visit-tags-table-buffer)
                       (file-name-directory (buffer-file-name)))))
       (list (and src-file tag-dir
                  (file-relative-name src-file tag-dir))))))
  (save-excursion
    (visit-tags-table-buffer)
    (unless (and file (try-completion file (tags-table-files)))
      (setq file (completing-read "List file tags: "
                                  'tags-complete-tags-table-file nil t))))
  (funcall #'list-tags file))

;;;###autoload(autoload 'nvp-tag-menu "nvp-tag" nil t)
(transient-define-prefix nvp-tag-menu ()
  [ :if-non-nil tags-file-name
    "Tags"
    ("a" "Apropos" tags-apropos)
    ("s" "Search" tags-search)
    ("f" "List file tags" nvp-tag-list-tags)
    ("q" "Query replace regexp" tags-query-replace)
    ("e" "Find using etags" nvp-tag-find-etag)]
  [["Tables"
    ("l" "Load table" visit-tags-table)
    ("t" "Choose table" select-tags-table)
    ("R" "Re/gen project tags" projectile-regenerate-tags)
    ("K" "Reset tables" tags-reset-tags-tables :if-non-nil tags-file-name)]
   ["CTags"
    ("cc" "Show tags" nvp-tag-show-ctags)
    ("cf" "Show languages for files" nvp-tag-show-ctags-languages)
    ("cl" "Show language config" nvp-tag-show-ctags-language-config)]])
;; settings
;; `tags-apropos-verbose' list filenames

(provide 'nvp-tag)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-tag.el ends here
