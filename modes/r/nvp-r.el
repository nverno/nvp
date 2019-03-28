;;; nvp-r.el --- r helpers -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/r-tools
;; Last modified: <2019-03-27 15:43:34>
;; Created: 27 September 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (require 'nvp-hap))
(require 'nvp)
(require 'ess-site)
(require 'ess-inf)

(defvar nvp-r-rterm-program
  (expand-file-name
   (format "Rterm%s" (nvp-with-gnu/w32 "" ".exe"))
   (file-name-directory inferior-R-program)))

(defvar nvp-r-source-dir (expand-file-name "R/r-source" (getenv "DEVEL")))
(defvar nvp-r-package-src (expand-file-name "R/src" (getenv "DEVEL")))
(defvar nvp-r-source-repo "http://www.github.com/wch/r-source")

;; -------------------------------------------------------------------
;;; Utils
(autoload 'nvp-list-intersection "nvp-util")
(autoload 'nvp-s-match-positions "nvp-string")

;; Guess where the column breaks are located (assumes right-justified).
(defun nvp-r-guess-column-breaks (str)
  (let* ((res (mapcar
               #'(lambda (x)
                   (mapcar
                    #'cdr (nvp-s-match-positions "\'.*?\'\\|[-0-9.A-Za-z]+" x)))
               (split-string str "\n")))
         (res (cl-remove-if-not #'(lambda (x)
                                    (> (length x) 0))
                                res)))
    (nvp-list-intersection res)))

;;; FIXME: remove
(eval-when-compile
  (defmacro nvp-r-str-or-region (name &optional doc &rest body)
    (declare (indent defun))
    (let* ((fn (intern (symbol-name name))))
      `(defun ,fn (str &optional from to)
         ,doc
         (interactive
          (if (region-active-p)
              (list nil (region-beginning) (region-end))
            (let ((bds (bounds-of-thing-at-point 'paragraph)))
              (list nil (car bds) (cdr bds)))))
         (let* ((input (or str (buffer-substring-no-properties from to)))
                output)
           ,@body
           (if str output
             (save-excursion
               (delete-region from to)
               (goto-char from)
               (insert output))))))))

;; ------------------------------------------------------------
;;; Commands

;; Align single hashtags for end-of-line R comments. Must be a single
;; '#' preceded by space, ignores the '##' so they align with code.
(defun nvp-r-comment-align (beg end)
  (interactive "*r")
  (let (indent-tabs-mode)
    (align-regexp beg end "\\(\\s-*\\)[^#]#" -1 1)))

;; Add extra newline between brackets. Insert roxy starter in roxy comments.
(nvp-newline nvp-r-newline-dwim nil
  :pairs (("\\[" "\\]") ("(" ")") ("{" "}"))
  :comment-re ("##'")
  :comment-start "##' ")

;;; REPL

;; redirect repl output to temp buffer
(defun nvp-r-redirect-output (command &optional buffer process)
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

;;; string or regions

(defvar nvp-r-header-str
  (eval-when-compile
    (mapconcat #'(lambda (x) (or (and (consp x) (make-string (car x) (cadr x))) x))
               '("## " (77 ?-) "\n##\n## %s\n##\n## " (77 ?-)) "")))

;; Surround STR or region in header box.
(nvp-r-str-or-region nvp-r-header-box nil
  (let* ((header (replace-regexp-in-string "^[#\s]+\\|\n" "" input)))
    (setq output
          (format nvp-r-header-str
                  (concat (make-string (- (/ 74 2) (/ (length header) 2)) ?\s)
                          header)))))

;; Insert commas into table (assumes right-justified).
(nvp-r-str-or-region nvp-r-insert-commas-table nil
  (let* ((_pos (nvp-r-guess-column-breaks input))
         (raw (split-string input "\n"))
         (_lines (cl-remove-if-not (lambda (x) (> (length x) 0)) raw))
         (new-lines #'(mapcar (lambda (x)
                                (with-temp-buffer
                                  (insert x)
                                  (mapc
                                   (lambda (y) (goto-char (+ y (nth (- y 1) _pos)))
                                     (insert ","))
                                   (number-sequence 1 (- (length _pos) 1)))
                                  (buffer-string)))
                              _lines)))
    (setq output (mapconcat 'identity new-lines "\n"))))

(defvar nvp-r-datetime-regex
  (eval-when-compile
    (concat "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} "
            "[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)")))

;; Quote R date times to use with `read.table'.
(nvp-r-str-or-region nvp-r-quote-datetime nil
  (setq output (replace-regexp-in-string nvp-r-datetime-regex "\'\\1\'" input)))

;; -------------------------------------------------------------------
;;; Imenu/headers
(eval-when-compile
  (defvar nvp-imenu-comment-headers-re)
  (defvar nvp-imenu-comment-headers-re-1)
  (defvar nvp-imenu-comment-headers-re-2)
  (defvar nvp-mode-header-regex))
(declare-function nvp-imenu-setup "nvp-imenu")

(defun nvp-r-setup-headers ()
  ;; code blocks in spin docs
  (setq nvp-mode-header-regex "\\s-*##\\(?:-\\|+\\)+")
  ;; comment headers in rmd/spin
  (nvp-imenu-setup
   :headers '((nil "\\s-*##'\\s-+#\\s-*\\(.*\\)\\s-*$" 1))
   :headers-1 '(("Headers" "\\s-*##'\\s-+#\\s-*\\(.*\\)\\s-*$" 1))
   :headers-2 '(("Sub-headers" "\\s-*##'\\s-+##\\s-*\\(.*\\)\\s-*$" 1))))

;; -------------------------------------------------------------------
;;; Help

;; toggle help at point in popup
(defun nvp-r-help-at-point (&optional command)
  (interactive)
  (let ((sym (ess-symbol-at-point)))
    (when sym
      (let* ((object (symbol-name sym))
             (hb-name (concat "*help["
                              ess-current-process-name
                              "](" (replace-regexp-in-string
                                    "^\\?\\|`" "" object) ")*"))
             (old-hb-p (get-buffer hb-name))
             (tbuffer (get-buffer-create hb-name))
             (str
              (if (or (not old-hb-p)
                      current-prefix-arg
                      (ess--help-get-bogus-buffer-substring old-hb-p))
                  (ess-with-current-buffer tbuffer
                    (setq ess-help-object object ess-help-type 'help)
                    (ess--flush-help-into-current-buffer object command)
                    (buffer-substring-no-properties (point-min) (point-max)))
                (with-current-buffer tbuffer
                  (buffer-substring-no-properties (point-min) (point-max))))))
        (nvp-with-toggled-tip str
          :help-fn (function
                    (lambda ()
                      (interactive)
                      (x-hide-tip)
                      (pop-to-buffer tbuffer))))))))

;; -------------------------------------------------------------------
;;; Roxy 

;;Convert regular comments to roxygen prefixes.
(defun nvp-r-roxy ()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (while (re-search-forward "^#+\'?" (region-end) 'move)
          (replace-match "##\'"))
      (goto-char (point-min))
      (while (re-search-forward "^#+\'?" nil t)
        (replace-match "##\'")))))

(defun nvp-r-roxy-preview (type)
  (interactive
   (list (nvp-completing-read "Preview: " '("Rd" "HTML" "text"))))
  (funcall-interactively (intern (concat "ess-roxy-preview-" type))))

;; ------------------------------------------------------------
;;; Tags

(declare-function tag-utils-tag-dir "tag-utils")

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
        (tag-utils-tag-dir nvp-r-source-dir :language "r"
                           :program (nvp-program ctags))
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
           (start-process "r-tags" "*R-tags*" nvp-r-rterm-program "-e" rtags)
           #'(lambda (_p m)
               (message "%s: %s" "r-tags" m)
               (nvp-r-rtags pkg t))))))
     (t (visit-tags-table tags)))))

;; -------------------------------------------------------------------
;;; Abbrevs

;; dont expand in comments/strings or in symbols with '.', eg. is.null
(defun nvp-r-abbrev-expand-p ()
  (and (not (string-match-p "\\." (symbol-name (ess-symbol-at-point))))
       (let ((ppss (syntax-ppss)))
         (not (or (elt ppss 3) (elt ppss 4))))))

;; -------------------------------------------------------------------
;;; Completion

(eval-when-compile
  (defvar company-backends))

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
    (set (make-local-variable 'company-backends) comps)))

(provide 'nvp-r)

;;; nvp-r.el ends here
