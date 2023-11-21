;;; nvp-c-help.el ---  -*- lexical-binding: t; -*-

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
(eval-when-compile (require 'nvp-macro))
(require 'nvp-hap-semantic)
(require 'nvp-c)
(nvp:req 'nvp-c 'subrs)
(nvp:auto "nvp-hap-man" 'nvp-hap-man-buffer)
(nvp:decls :v (semantic-c-dependency-system-include-path))

;; -------------------------------------------------------------------
;;; Sources

;; sources determined by source file paths
(defvar nvp-c-help-online-sources
  (let ((uri
         "https://pubs.opengroup.org/onlinepubs/9699919799/functions/%s.html"
         ;; "http://en.cppreference.com/mwiki/index.php?title=Special:Search&search=%s"
         ))
    (cl-loop for p in semantic-c-dependency-system-include-path
       collect (cons p uri))))

;; if filename prefix is member of car, apply cadr to (format cddr)
;; use man 2 for system call type stuff, otherwise man 3
(defvar nvp-c-help-local-sources
  `((("/usr/include/unistd" "/usr/include/fcntl"
      "sys/time" "sys/wait" "sys/resource"
      "/usr/include/signal")
     . (man "2 %s"))
    (,semantic-c-dependency-system-include-path . (man "3 %s"))))

;; -------------------------------------------------------------------
;;; Util

;; return name of function at point and if it is static
(defun nvp-c-help-function-at-point ()
  (ignore-errors
    (let ((tag (semantic-current-tag)))
      (when (and tag (eq (cadr tag) 'function))
        (let ((mods (alist-get :typemodifiers (cdr tag))))
          (if (and mods (member "static" (car mods)))
              (list (car tag) 'static)
            (list (car tag))))))))

(eval-when-compile
  (defmacro nvp:c-help-find-source (type file)
    "Find help location for TYPE as determined by FILE."
    (pcase type
      (`'online
       `(cl-some (lambda (src)
                   (and (string-prefix-p (car src) ,file)
                        src))
                 nvp-c-help-online-sources))
      (_
       `(cdr-safe
         (cl-find-if
          (lambda (entry)
            (cl-some (lambda (e) (string-match-p e ,file)) (car entry)))
          nvp-c-help-local-sources))))))

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
(defun nvp-hap-c (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (nvp-semantic-tag-at (point)))
    (doc-buffer
     (pcase-let ((`(,tag-name ,file ,_args) (nvp-c--parse-tag arg)))
       (--when-let (and (stringp file)
                        (nvp:c-help-find-source 'local file))
         (pcase-let ((`(,cmd ,fmt ,args) it))
           (pcase cmd
             ('man
              (let ((topic (format fmt tag-name)))
                (or
                 (nvp-hap-man-buffer topic)
                 (nvp-hap-man-buffer (nvp-c--man-next-section topic)))))
             (_ (apply cmd (format fmt tag-name) args)))))))
    (url
     (pcase-let ((`(,tag-name ,file ,_args) (nvp-c--parse-tag arg)))
       (--when-let (and (stringp file)
                        (cdr (nvp:c-help-find-source 'online file)))
         (format it tag-name))))))


;; -------------------------------------------------------------------
;;; Commands

;; TODO: index and search
;; ;;;###autoload
;; (defun nvp-c-help-std ()
;;   (interactive)
;;   (browse-url "http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf"))

;; jump to function in header file
;;;###autoload
(defun nvp-c-help-jump-to-function-header ()
  (interactive)
  (nvp:c-check-semantic)
  (let ((func (nvp-c-help-function-at-point))
        ;; FIXME: use semanticdb to get include
        (header (nvp:c--header-file-name)))
    ;; don't try for static functions
    (if (and func (not (cdr func)) header)
        (progn
          (find-file-other-window header)
          (goto-char (point-min))
          (search-forward (car func) nil 'move))
      (message "function %s is static" (car func)))))

(provide 'nvp-c-help)
;;; nvp-c-help.el ends here
