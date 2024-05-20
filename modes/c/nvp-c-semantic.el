;;; nvp-c-semantic.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Get help from local sources, fallback to online lookup when those fail.
;;
;; Local Methods:
;; - man
;; - info (glibc)
;; - semantic scraping in code documentation
;;
;; Remote:
;; - just tries to determine the function at hand and plugs it into the url
;;
;;; Code:

(eval-when-compile
  (require 'nvp-macro)
  (require 'semantic/bovine/c))
(nvp:decls :p (semantic nvp-hap) :f (semantic-tag-name semantic-tag-p))


(defsubst nvp-c--check-semantic ()
  (unless (and (fboundp 'semantic-active-p)
               (semantic-active-p))
    (user-error "Semantic not active.")))

;;; Sources
;; sources determined by source file paths
(defvar nvp-c-semantic-online-sources
  (let ((uri
         "https://pubs.opengroup.org/onlinepubs/9699919799/functions/%s.html"
         ;; "http://en.cppreference.com/mwiki/index.php?title=Special:Search&search=%s"
         ))
    (cl-loop for p in semantic-c-dependency-system-include-path
       collect (cons p uri))))

;; if filename prefix is member of car, apply cadr to (format cddr)
;; use man 2 for system call type stuff, otherwise man 3
(defvar nvp-c-semantic-local-sources
  `((("/usr/include/unistd" "/usr/include/fcntl"
      "sys/time" "sys/wait" "sys/resource"
      "/usr/include/signal")
     . (man "2 %s"))
    (,semantic-c-dependency-system-include-path . (man "3 %s"))))

(defsubst nvp-c--find-source-online (file)
  (cl-some (lambda (src) (and (string-prefix-p (car src) file) src))
           nvp-c-semantic-online-sources))

(defsubst nvp-c--find-source-local (file)
  (cdr-safe
   (cl-find-if
    (lambda (entry) (cl-some (lambda (e) (string-match-p e file)) (car entry)))
    nvp-c-semantic-local-sources)))

;; -------------------------------------------------------------------
;;; Util

;; return name of function at point and if it is static
(defun nvp-c-semantic-function-at-point ()
  (ignore-errors
    (let ((tag (semantic-current-tag)))
      (when (and tag (eq (cadr tag) 'function))
        (let ((mods (alist-get :typemodifiers (cdr tag))))
          (if (and mods (member "static" (car mods)))
              (list (car tag) 'static)
            (list (car tag))))))))

;; -------------------------------------------------------------------
;;; Hap backend

(defsubst nvp-c--parse-tag (tag)
  (if (semantic-tag-p tag)
      (list (semantic-tag-name tag) (semantic-tag-file-name tag))
    (list tag)))

(defun nvp-c--man-next-section (cmd)
  (let ((num (substring cmd 0 1)))
    (concat
     (pcase num
       (`"1" "2")
       (`"2" "3")
       (`"3" "2"))
     (substring cmd 1))))

;;;###autoload
(defun nvp-hap-semantic-c (command &optional arg &rest _args)
  (cl-case command
    (init (nvp-hap-semantic-init))
    (thingatpt (nvp-hap-semantic-tag-at (point)))
    (doc-buffer
     (pcase-let ((`(,tag-name ,file ,_args) (nvp-c--parse-tag arg)))
       (--when-let (and (stringp file)
                        (nvp-c--find-source-local file))
         (pcase-let ((`(,cmd ,fmt ,args) it))
           (pcase cmd
             ('man
              (let ((topic (format fmt tag-name)))
                (or
                 (nvp-hap-man-buffer topic)
                 (nvp-hap-man-buffer (nvp-c--man-next-section topic)))))
             (_ (apply cmd (format fmt tag-name) args)))))))
    (search-remote
     (pcase-let ((`(,tag-name ,file ,_args) (nvp-c--parse-tag arg)))
       (--when-let (and (stringp file)
                        (cdr (nvp-c--find-source-online file)))
         (format it tag-name))))))

;; associated header file name
(defsubst nvp-c--header-file-name (&optional buffer ext)
  (concat (nvp:no-ext buffer) (or ext ".h")))

;;;###autoload
(defun nvp-c-semantic-jump-to-function-header ()
  "Jump to function in header file."
  (interactive)
  (nvp-c--check-semantic)
  (let ((func (nvp-c-semantic-function-at-point))
        ;; FIXME: use semanticdb to get include
        (header (nvp-c--header-file-name)))
    ;; don't try for static functions
    (if (and func (not (cdr func)) header)
        (progn (find-file-other-window header)
               (goto-char (point-min))
               (search-forward (car func) nil 'move))
      (message "function %s is static" (car func)))))

(provide 'nvp-c-semantic)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-c-semantic.el ends here
