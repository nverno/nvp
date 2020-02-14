;;; nvp-c++-help.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;; XXX: semantic needs some tweeks to be more effective at jumping to
;;      references
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'semantic/analyze)

;;; TODO:
;; - index/jump to std
;; - local sources
;; - help determined by filepath

;; https://en.cppreference.com/w/
(defvar nvp-c++-help-online-sources
  '(("std::" .
     "http://en.cppreference.com/mwiki/index.php?title=Special:Search&search=%s")
    ("boost::" . "http://google.com/search?q=site:boost.org%%20%s")))

;; Use semantic to determine the fully namespace-qualified type of the symbol at
;; POINT.
;; https://github.com/alexott/emacs-configs/rc/emacs-rc-ccmode.el
(defun nvp-c++-help-type-at (point)
  (let* ((ctxt (semantic-analyze-current-context point))
	 (pf (reverse (oref ctxt prefix)))
	 (lastname (pop pf))
	 (tag (if (semantic-tag-p lastname) lastname (caar pf)))
	 (names (append
		 (when (semantic-tag-p tag)
		   (save-excursion
		     (when (semantic-tag-with-position-p tag)
		       (set-buffer (semantic-tag-buffer tag))
		       (semantic-go-to-tag tag)
		       (mapcar 'semantic-tag-name
                               (semantic-analyze-scope-nested-tags (point) nil)))))
		 (list (if (semantic-tag-p lastname) (semantic-tag-name lastname)
                         lastname)))))
    (concat (mapconcat 'concat names "::"))))

;; -------------------------------------------------------------------
;;; Commands

;; https://github.com/alexott/emacs-configs/rc/emacs-rc-ccmode.el
;;;###autoload
(defun nvp-c++-help-at-point (point)
  "Browse the documentation for the C++ symbol at POINT."
  (interactive "d")
  (let* ((cpptype (nvp-c++-help-type-at point))
	 (ref (when (stringp cpptype)
		(car (cl-member-if (lambda (S) (string-prefix-p (car S) cpptype))
				   nvp-c++-help-online-sources)))))
    (if ref	
	(browse-url (format (cdr ref) cpptype))
      (message "No documentation source found for %s" cpptype))))

;;;###autoload
(defun nvp-c++-help (point)
  (interactive "d")
  (let ((cpptype (nvp-c++-help-type-at point)))
    (when cpptype
      (funcall-interactively 'manual-entry cpptype))))

(provide 'nvp-c++-help)
;;; nvp-c++-help.el ends here
