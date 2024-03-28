;;; nvp-r.el --- r helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'ess-site nil t)
(require 'ess-inf nil t)
(nvp:decls :p (ess) :v (inferior-R-program))
(nvp:auto "s" 's-matched-positions-all)

;; add comment continuation when in roxy block
(nvp:defmethod nvp-newline-dwim-comment (_syntax arg)
  :modes (ess-r-mode r-ts-mode)
  (save-match-data
    (--if-let (and nvp-newline-comment-continue
                   (save-excursion
                     (beginning-of-line)
                     (and (looking-at "^\\(#+'\\)")
                          (match-string 1))))
        (dotimes (_ (or arg 1))
          (insert ?\n it))
      (newline-and-indent arg))))

;;; TODO: REPL
;; (with-eval-after-load 'nvp-repl
;;   (nvp-repl-add '(inferior-ess-r-mode)
;;     :name 'R
;;     :modes '(ess-r-mode r-ts-mode)
;;     :init
;;     ;; :help-cmd
;;     ;; :pwd-cmd
;;     ;; :cd-cmd
;;     ))

;;; Abbrev
;; dont expand in comments/strings or in symbols with '.', eg. is.null
(defun nvp-r-abbrev-expand-p ()
  (and (not (string-match-p "\\." (thing-at-point 'symbol t)))
       (not (nvp:ppss 'soc))))

;;; Roxy
(defun nvp-r-roxy ()
  "Convert regular comments to roxygen."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (while (re-search-forward "^#+\'?" (region-end) 'move)
          (replace-match "##\'"))
      (goto-char (point-min))
      (while (re-search-forward "^#+\'?" nil t)
        (replace-match "##\'")))))

(defun nvp-r-roxy-preview (type)
  (interactive (list (nvp-completing-read "Preview: " '("Rd" "HTML" "text"))))
  (funcall-interactively (intern (concat "ess-roxy-preview-" type))))

;;; Tables
;; Guess where the column breaks are located (assumes right-justified).
(defun nvp-r-guess-column-breaks (str)
  (-->
   (--map
    (--map (cdr it) (s-matched-positions-all "\'.*?\'\\|[-0-9.A-Za-z]+" it))
    (split-string str "\n"))
   (cl-remove-if-not (lambda (x) (> (length x) 0)) it)
   (nreverse (nvp:list-intersection it))))

(defun nvp-r-table-insert-commas (str &optional beg end)
  "Insert commas into space separated table (assume right-justified)."
  (interactive
   (-let (((beg . end) (nvp:tap 'bdwim 'paragraph :pulse t)))
     (list (and beg end (buffer-substring-no-properties beg end))
           beg end t)))
  (let* ((pos (nvp-r-guess-column-breaks str))
         (table
          (--> (--map (with-temp-buffer
                        (insert it)
                        (mapc (lambda (y) (goto-char (+ y (nth (1- y) pos)))
                                (insert ","))
                              (number-sequence 1 (length pos)))
                        (buffer-string))
                      (split-string str "\n" 'omit "\t "))
               (mapconcat 'identity it "\n"))))
    (if (and beg end)
        (save-excursion
          (delete-region beg end)
          (goto-char beg)
          (insert table))
      table)))

(defvar nvp-r-datetime-regex
  (nvp:concat "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} "
              "[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)"))

(defun nvp-r-table-quote-datetime (&optional str beg end)
  "Quote date times to use with \\='read.table."
  (interactive (list nil (nvp:tap 'bdwim 'paragraph :pulse t)))
  (if str (replace-regexp-in-string nvp-r-datetime-regex "\'\\1\'" str)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward nvp-r-datetime-regex end t)
        (replace-match "\'\\1\'")))))

;; ------------------------------------------------------------
;;; Tags

(defvar nvp-r-source-dir (expand-file-name "R/r-source" (getenv "DEVEL")))
(defvar nvp-r-package-src (expand-file-name "R/src" (getenv "DEVEL")))
(defvar nvp-r-source-repo "http://www.github.com/wch/r-source")

(defun nvp-r-tag-dir (&optional directory pattern)
  "Create tags file [default c tags] for directory.
Return process object."
  (interactive)
  (let ((dir (or directory (read-directory-name "Tag directory: ")))
	(patt (or pattern ".*\\\\.[RchCH][xx|pp]?$")))
    (start-process-shell-command
     "r-tags" "*R-tags*"
     (format (concat "\"%s\" \"%s\" -type f -regextype posix-extended -regex \"%s\""
                     " | etags - -o \"%s\"")
             (or find-program "find") dir patt (expand-file-name "TAGS" dir)))))

;; find . -name ".*[chCH]" -print | etags -
(defun nvp-r-tag-source (&optional noretry)
  "Load tags table for R source.
Create tags if they dont exist."
  (interactive)
  (let* ((tags (expand-file-name "TAGS" nvp-r-source-dir))
	 (no-src (not (file-exists-p nvp-r-source-dir)))
	 (no-tags (not (file-exists-p tags))))
    (unless noretry
      (cond
       (no-src (message "Cloning R source repo")
               (nvp-r--tag-sentinel
                (start-process "r-tags" "*R-tags*" "git" "clone" nvp-r-source-repo
                               "--depth=1" nvp-r-source-dir)
                nil))
       (no-tags (message "Creating R source TAGS"))
       (t (visit-tags-table tags))))))

(defun nvp-r--tag-sentinel (proc &optional noretry)
  (set-process-sentinel
   proc (lambda (p m)
          (message "%s: %s" (process-name p) (replace-regexp-in-string "\n" "" m))
          (when (eq 0 (process-exit-status p))
            (nvp-r-tag-source noretry)))))

(defun nvp-r-rtags (pkg &optional noretry)
  "Load/make R tags for package PKG."
  (interactive)
  (let ((pkg (or pkg
                 (and current-prefix-arg
                      (read-directory-name "Package directory: "))
                 (expand-file-name
                  (read-from-minibuffer "Package name: ") nvp-r-package-src)))
        (tags (expand-file-name "RTAGS" pkg)))
    (cond
     ((not (file-exists-p pkg))
      (user-error "Directory %s doesn't exist" pkg))
     ((not (file-exists-p tags))
      (unless noretry
        (let ((rtags (format "rtags(path=\"%s\", ofile=\"%s\", recursive=TRUE)"
                             pkg tags)))
          (set-process-sentinel
           (start-process "r-tags" "*R-tags*" inferior-R-program "-e" rtags)
           #'(lambda (_p m)
               (message "%s: %s" "r-tags" m)
               (nvp-r-rtags pkg t))))))
     (t (visit-tags-table tags)))))

;; list libraries ahead of other options when in "require|library"
(defun nvp-r-company-setup (&optional backends)
  (let ((comps (cl-remove-if
                   (lambda (x)
                     (memq (or (and (listp x) (car x)) x)
                           '(company-R-args company-R-objects company-R-library)))
                   company-backends)))
    (push (or backends
              '(company-R-args :with company-R-library company-R-objects :separate))
          comps)
    (setq-local company-backends comps)
    (delq 'company-capf company-backends)))


(provide 'nvp-r)

;;; nvp-r.el ends here
