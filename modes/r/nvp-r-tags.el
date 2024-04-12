;;; nvp-r-tags.el --- Tag R source -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls)

(defvar inferior-R-program (nvp:program "R"))
(defvar nvp-r-source-dir (expand-file-name "R/r-source" (getenv "DEVEL")))
(defvar nvp-r-package-src (expand-file-name "R/src" (getenv "DEVEL")))
(defvar nvp-r-source-repo "http://www.github.com/wch/r-source")

;; find . -name ".*[chCH]" -print | etags -
;;;###autoload
(defun nvp-r-tag-source (source-repo source-dir &optional tag-file no-continue)
  "Load tags table for R source.
Create tags if they dont exist."
  (interactive (list nvp-r-source-repo nvp-r-source-dir "TAGS"))
  (nvp-tag-repo source-repo source-dir tag-file no-continue))

;;;###autoload
(defun nvp-r-tag-dir (&optional directory pattern)
  "Create tags file [default c tags] for directory.
Return process object."
  (interactive)
  (let ((dir (or directory (read-directory-name "Tag directory: ")))
	(patt (or pattern ".*\\\\.[RchCH][xx|pp]?$")))
    (start-process-shell-command
     "r-tags" "*R-tags*"
     (format
      (concat "\"%s\" \"%s\" -type f -regextype posix-extended -regex \"%s\""
              " | etags - -o \"%s\"")
      (or find-program "find") dir patt (expand-file-name "TAGS" dir)))))

;;;###autoload
(defun nvp-r-rtags (pkg &optional no-continue)
  "Load/make R tags for package PKG."
  (interactive)
  (unless pkg
    (setq pkg (or (and current-prefix-arg
                       (read-directory-name "Package directory: "))
                  (expand-file-name
                   (read-from-minibuffer "Package name: ") nvp-r-package-src))))
  (let ((tags (expand-file-name "RTAGS" pkg)))
    (cond ((not (file-exists-p pkg))
           (user-error "Directory %s doesn't exist" pkg))
          ((not (file-exists-p tags))
           (unless no-continue
             (let ((rtags
                    (format "rtags(path=\"%s\", ofile=\"%s\", recursive=TRUE)"
                            pkg tags)))
               (set-process-sentinel
                (start-process "r-tags" "*R-tags*" inferior-R-program "-e" rtags)
                #'(lambda (_p m)
                    (message "%s: %s" "r-tags" m)
                    (nvp-r-rtags pkg t))))))
          (t (visit-tags-table tags)))))


(provide 'nvp-r-tags)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-r-tags.el ends here
