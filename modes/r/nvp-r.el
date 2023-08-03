;;; nvp-r.el --- r helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'ess-site)
(require 'ess-inf)
(require 'nvp)
(nvp:decls)
(nvp:auto "s" 's-matched-positions-all)

(defvar nvp-r-source-dir (expand-file-name "R/r-source" (getenv "DEVEL")))
(defvar nvp-r-package-src (expand-file-name "R/src" (getenv "DEVEL")))
(defvar nvp-r-source-repo "http://www.github.com/wch/r-source")

;; add comment continuation when in roxy block
(cl-defmethod nvp-newline-dwim-comment
  (_syntax arg &context (major-mode ess-r-mode))
  (save-match-data
    (--if-let (and nvp-newline-comment-continue
                   (save-excursion
                     (beginning-of-line)
                     (and (looking-at "^\\(#+'\\)")
                          (match-string 1))))
        (dotimes (_ (or arg 1))
          (insert ?\n it))
      (newline-and-indent arg))))

;; dont expand in comments/strings or in symbols with '.', eg. is.null
(defun nvp-r-abbrev-expand-p ()
  (and (not (string-match-p "\\." (symbol-name (ess-symbol-at-point))))
       (not (nvp:ppss 'soc))))

;; Guess where the column breaks are located (assumes right-justified).
(defun nvp-r-guess-column-breaks (str)
  (-->
   (--map
    (--map (cdr it) (s-matched-positions-all "\'.*?\'\\|[-0-9.A-Za-z]+" it))
    (split-string str "\n"))
   (cl-remove-if-not (lambda (x) (> (length x) 0)) it)
   (nreverse (nvp:list-intersection it))))

;; ------------------------------------------------------------
;;; Commands

(defun nvp-r-dev-off ()
  (interactive)
  (let ((proc (ess-get-process)))
    (ess-send-string proc "dev.off()")))

;; Align single hashtags for end-of-line R comments. Must be a single
;; '#' preceded by space, ignores the '##' so they align with code.
(defun nvp-r-comment-align (beg end)
  "Align single hashtag comments as end-of-line."
  (interactive "*r")
  (let (indent-tabs-mode)
    (align-regexp beg end "\\(\\s-*\\)[^#]#" -1 1)))

;;; REPL

(defun nvp-r-redirect-output (command &optional buffer process)
  "Redirect REPL output to temp buffer."
  (interactive (list (read-from-minibuffer "Command: ")))
  (let ((buff (get-buffer-create
               (or buffer
                   (and current-prefix-arg
                        (concat "*"
                                (read-from-minibuffer "Output buffer: ")
                                "*"))
                   "*r-output*")))
        ;; `ess-get-process' defaults to process local to current
        ;; buffer, so to call from anywhere default to "R"
        (proc (ess-get-process (or process "R"))))
    ;; send a trailing newline to process
    (unless (string-match-p "\n$" command)
      (setq command (concat command "\n")))
    (ess-command command buff 'sleep nil nil proc)
    (with-current-buffer buff
      ;; process stuff
      (pop-to-buffer buff))))


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
;;; FIXME: remove / fix tag stuff

;; Create tags file [default c tags] for directory. Return process object.
(defun nvp-r-tag-dir (&optional directory pattern)
  (interactive)
  (let ((dir (or directory (read-directory-name "Tag directory: ")))
	(patt (or pattern ".*\\\\.[RchCH][xx|pp]?$")))
    (start-process-shell-command
     "r-tags" "*R-tags*"
     (format (concat "\"%s\" \"%s\" -type f -regextype posix-extended -regex \"%s\""
                     " | etags - -o \"%s\"")
             (or find-program "find") dir patt (expand-file-name "TAGS" dir)))))

;; Loads tags table for r-source when it exists, otherwise tag or try to
;; download source windows: find . -name ".*[chCH]" -print | etags -
;; storing in ~/R/r-source/TAGS
(defun nvp-r-tag-source (&optional noretry)
  (interactive)
  (let* ((tags (expand-file-name "TAGS" nvp-r-source-dir))
	 (no-src (not (file-exists-p nvp-r-source-dir)))
	 (no-tags (not (file-exists-p tags))))
    (unless noretry
      (cond
       (no-src
        (message "Cloning R source repo")
        (nvp-r-tag-sentinel
         (start-process "r-tags" "*R-tags*" "git" "clone" nvp-r-source-repo
                        "--depth=1" nvp-r-source-dir)
         nil))
       (no-tags
        (message "Creating R source TAGS")
        ;; (tag-utils-tag-dir nvp-r-source-dir :language "r"
        ;;                    :program (nvp:program ctags))
        ;; (nvp-r-tag-sentinel
        ;;  (nvp-r-tag-dir nvp-r-source-dir)
        ;;  t)
        )
       (t (visit-tags-table tags))))))

(defun nvp-r-tag-sentinel (proc &optional noretry)
  (set-process-sentinel
   proc #'(lambda (p m)
            (message "%s: %s" (process-name p)
                     (replace-regexp-in-string "\n" "" m))
            (when (eq 0 (process-exit-status p))
              (nvp-r-tag-source noretry)))))

;; Load/make R tags for package PKG.
(defun nvp-r-rtags (pkg &optional noretry)
  (interactive)
  (let ((pkg (or pkg
                 (and current-prefix-arg
                      (read-directory-name "Package directory: "))
                 (expand-file-name (read-from-minibuffer "Package name: ")
                                   nvp-r-package-src)))
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

;; -------------------------------------------------------------------
;;; Setup

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

(defun nvp-r-locals ()
  ;; code blocks in spin docs
  (setq-local nvp-mode-header-regex "\\s-*##\\(?:-\\|+\\)+")
  ;; comment headers in rmd/spin
  (nvp-imenu-setup
   :headers '((nil "\\s-*##'\\s-+#\\s-*\\(.*\\)\\s-*$" 1))
   :headers-1 '(("Headers" "\\s-*##'\\s-+#\\s-*\\(.*\\)\\s-*$" 1))
   :headers-2 '(("Sub-headers" "\\s-*##'\\s-+##\\s-*\\(.*\\)\\s-*$" 1))))

(provide 'nvp-r)

;;; nvp-r.el ends here
