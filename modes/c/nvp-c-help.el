;;; nvp-c-help.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (defvar semantic-c-dependency-system-include-path))
(require 'semantic/analyze)
(require 'nvp-c)
(nvp-decls)

;; -------------------------------------------------------------------
;;; Sources

;; sources determined by source file paths
(defvar nvp-c-help-online-sources
  (let ((uri
         "http://pubs.opengroup.org/onlinepubs/9699919799/functions/%s.html"
         ;; "http://en.cppreference.com/mwiki/index.php?title=Special:Search&search=%s"
         ))
    (cl-loop for p in semantic-c-dependency-system-include-path
       collect (cons p uri))))

;; if filename prefix is member of car, apply cadr to (format cddr)
;; use man 2 for system call type stuff, otherwise man 3
(defvar nvp-c-help-local-sources
  `((("/usr/include/unistd" "/usr/include/fcntl"
      "sys/time" "sys/wait" "sys/resource"
      "/usr/include/signal") . (man "2 %s"))
    (,semantic-c-dependency-system-include-path . (man "3 %s"))))

;; -------------------------------------------------------------------
;;; Util

;; semantic tag at point
(defsubst nvp-c-help-tag-at (point)
  (condition-case nil
      (car (reverse (oref (semantic-analyze-current-context point) prefix)))
    (error (message "No symbol found at point"))))

;; return name of function at point and if it is static
(defsubst nvp-c-help-function-at-point ()
  (ignore-errors
    (let ((tag (semantic-current-tag)))
      (when (and tag (eq (cadr tag) 'function))
        (let ((mods (alist-get :typemodifiers (cdr tag))))
          (if (and mods (member "static" (car mods)))
              (list (car tag) 'static)
            (list (car tag))))))))

(eval-when-compile
  (defmacro nvp-c-help-find-source (type file)
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

;; TODO: if 'man 2' doesn't work, try 'man 3', eg. execvp in unistd
;;      - how to hook into Man to know if there was a problem?
;;        it creates the buffer no matter what, and runs async
;; (defun nvp-c-help-Man-cooked-hook ()
;;   (and (eq 0 (buffer-size))
;;        (throw 'no-dice nil)))
;; (setq Man-cooked-hook 'nvp-c-help-Man-cooked-hook)
(defun nvp-c-help-get-man-help (cmd)
  (let ((buf (man cmd)))
    (sit-for 0.1)                      ;FIXME
    (unless (buffer-live-p buf)
      (let ((num (substring cmd 0 1)))
        (man (concat
              (pcase num
                (`"1" "2")
                (`"2" "3")
                (`"3" "2"))
              (substring cmd 1)))))))

;; -------------------------------------------------------------------
;;; Commands

(nvp-declare "xref" xref-pop-marker-stack xref-push-marker-stack)

;; wrapper function: use xref marker stack before `semantic-ia-fast-jump'
;;;###autoload
(defun nvp-c-help-semantic-ia-fast-jump (point)
  (interactive "d")
  (xref-push-marker-stack)
  (condition-case nil
      (semantic-ia-fast-jump point)
    (error (xref-pop-marker-stack))))

;; get semantic-ia snarfed doc
(declare-function semanticdb-includes-in-table "semantic/db-ref")
(eval-when-compile (defvar semanticdb-current-table))
(defun nvp-c-help-semantic-ia-doc (point)
  (when-let* ((ctxt (semantic-analyze-current-context point))
              (pf (reverse (oref ctxt prefix)))) ; 'prefix
    (when (semantic-tag-p pf)
      (or (semantic-documentation-for-tag (car pf))
          ;; TODO: try includes
          (when-let* ((tab semanticdb-current-table)
                      (inc (semanticdb-includes-in-table tab))))))))

;;;###autoload
(defun nvp-c-help-semantic-ia-popup-doc (point)
  (interactive "d")
  (let ((doc (nvp-c-help-semantic-ia-doc point)))
    (when doc
      (nvp-with-toggled-tip doc))))

;; Lookup info in man or online for thing at point
;;;###autoload
(defun nvp-c-help-at-point (point &optional online)
  (interactive "d")
  (let* ((tag (nvp-c-help-tag-at point))
         (file (and (semantic-tag-p tag)
                    (semantic-tag-file-name tag))))
    (if (or online current-prefix-arg)
        (let ((ref (and (stringp file)
                        (nvp-c-help-find-source 'online file))))
          (if (not ref)
              (message "No documentation source found for %S" tag)
            (browse-url (format (cdr ref) (semantic-tag-name tag)))))
      (let ((action (and (stringp file)
                         (nvp-c-help-find-source 'local file)))
            (tag-name (or (and (semantic-tag-p tag)
                               (semantic-tag-name tag))
                          tag)))
        (when action
          (pcase (car action)
            ('man (nvp-c-help-get-man-help (format (cadr action) tag-name)))
            (_ (apply (car action)
                      (format (cadr action) tag-name)
                      (cddr action)))))))))

;; TODO: index and search
;;;###autoload
(defun nvp-c-help-std ()
  (interactive)
  (browse-url "http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf"))

;; jump to function in header file
;;;###autoload
(defun nvp-c-help-jump-to-function-header ()
  (interactive)
  (let ((func (nvp-c-help-function-at-point))
        ;; FIXME: use semanticdb to get include
        (header (nvp-c--header-file-name)))
    ;; don't try for static functions
    (if (and func (not (cdr func)) header)
        (progn
          (find-file-other-window header)
          (goto-char (point-min))
          (search-forward (car func) nil 'move))
      (message "function %s is static" (car func)))))

(provide 'nvp-c-help)
;;; nvp-c-help.el ends here
