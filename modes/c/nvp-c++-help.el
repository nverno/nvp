;;; nvp-c++-help.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; XXX: semantic needs some tweeks to be more effective at jumping to
;;      references
;;; TODO:
;; - index/jump to std
;; - local sources
;; - help determined by filepath
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'semantic/analyze nil t)
(nvp:req 'nvp-c 'subrs)
(nvp:decls :f (semantic-analyze-current-context
               semantic-tag-p semantic-tag-with-position-p
               semantic-go-to-tag semantic-tag-name
               semantic-analyze-scope-nested-tags))


;; https://en.cppreference.com/w/
(defvar nvp-c++-online-docrefs
  '(("std::" .
     "http://en.cppreference.com/mwiki/index.php?title=Special:Search&search=%s")
    ("boost::" . "http://google.com/search?q=site:boost.org%%20%s")))


;; -------------------------------------------------------------------
;;; Semantic

;; stolen from https://github.com/alexott/emacs-configs/rc/emacs-rc-ccmode.el
(defun nvp-c++-semantic-tag (point)
  "Use semantic to determine the fully namespace-qualified type of the symbol at
POINT."
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

;; https://github.com/alexott/emacs-configs/rc/emacs-rc-ccmode.el
;;;###autoload
(defun nvp-c++-semantic-browse-docs (point)
  "Browse the documentation for the C++ symbol at POINT."
  (interactive "d")
  (let* ((cpptype (and (semantic-active-p)
                       (nvp-c++-semantic-tag point)))
	 (ref (when (stringp cpptype)
		(car (cl-member-if (lambda (S) (string-prefix-p (car S) cpptype))
				   nvp-c++-online-docrefs)))))
    (if ref	
	(browse-url (format (cdr ref) cpptype))
      (message "No documentation source found for %s" cpptype))))


;;;###autoload
(defun nvp-c++-man (point)
  (interactive "d")
  (let ((cpptype (and (semantic-active-p)
                      (nvp-c++-semantic-tag point))))
    (when cpptype
      (funcall-interactively 'manual-entry cpptype))))

(provide 'nvp-c++-help)
;;; nvp-c++-help.el ends here
