;;; nvp-imenu.el --- imenu helpers  -*- lexical-binding: t; -*-

;;; Commentary:

;; imenu extensions:
;; - add support for mode specific headers
;; - wrapper function to call imenu/idomenu with additional options
;; - utilities to manipulate index-alist

;;; Code:
(eval-when-compile
  (require 'nvp-macro))
(require 'nvp)
(require 'idomenu nil t)
(declare-function idomenu "idomenu")
(nvp-decl nvp-comment-start)
(autoload 'nvp-flatten-tree "nvp-util")

;;; Local variables

;; top-level header regexp
(defvar-local nvp-imenu-comment-headers-re nil "Imenu top-level header regexp.")

;; header regexp nested under "Headers"
(defvar-local nvp-imenu-comment-headers-re-1 nil
  "Imenu header regexp nested under 'Headers'")

;; sub-headers
(defvar-local nvp-imenu-comment-headers-re-2 nil "Imenu sub-header regexp.")

(defvar nvp-imenu-default-filter-regex (regexp-opt '("Headers" "Sub-Headers"))
  "Regex to match sublist headers to filter out of cleaned imenu alist.")

;; -------------------------------------------------------------------
;;; Util
;; #<marker at 12739 in which-func.el.gz>
(eval-when-compile
  (defmacro ido/imenu ()
    (if (featurep 'idomenu) '(idomenu) '(imenu))))

(defun nvp-imenu-filter-regex (regex &optional alist)
  "Remove entries from ALIST matching REGEX."
  (cl-remove-if (apply-partially #'string-match-p regex) alist :key #'car))

(defun nvp-imenu-sort-relative-positions (marker alist)
  "Sort imenu entries so those closest in the buffer are first."
  (cl-sort alist (apply-partially #'nvp-imenu--relative-positions marker)))

(defun nvp-imenu-cleaned-alist (&optional regex alist)
  "Flatten imenu alist, remove headers and things that don't look like code."
  (or regex (setq regex nvp-imenu-default-filter-regex))
  (or alist (setq alist imenu--index-alist))
  (cl-remove-if-not
   #'nvp-imenu-maybe-code-p
   (nvp-flatten-tree (if (and regex (stringp regex))
                         (nvp-imenu-filter-regex regex alist)
                       alist)
                     'alist)))

(defun nvp-imenu-maybe-code-p (elem)
  "Filter out probable non code things."
  (let (mark)
    (and (consp elem)
         (not (string-match-p "[ \t;]\\|:\\'" (car elem)))
         (or (number-or-marker-p (setq mark (cdr elem)))
             (and (overlayp mark)
                  (setq mark (overlay-start mark)))))))

(defun nvp-imenu--relative-positions (pos item1 item2)
  "Return non-nil if ITEM1 is closer to POS than ITEM2 in the buffer.
Assumes the list is flattened and only elements with markers remain."
  (< (abs (- pos (cdr item1)))
     (abs (- pos (cdr item2)))))

;; -------------------------------------------------------------------
;;; Hook

(defun nvp-imenu--create-regex (&optional headers headers-1 headers-2)
  (if (equal headers ':none)
      (list :none nil nil)
    (or headers
        (setq headers
              `((nil ,(concat "^" (regexp-quote (nvp-comment-start 3))
                              "\\s-*\\(.*\\)\\s-*$")
                     1))))
    (or headers-1 (setq headers-1 `(("Headers" ,(cadr (car headers)) 1))))
    (or headers-2
        (setq headers-2
              `(("Sub-Headers"
                 ,(concat "^" (nvp-comment-start 2) "-+\\s-*\\(.*\\)[ -]*$") 1))))
    (list headers headers-1 headers-2)))

;; make header from comment
;;;###autoload
(cl-defun nvp-imenu-setup (&key headers headers-1 headers-2 extra default)
  "Sets up imenu regexps including those to recognize HEADERS and any \
EXTRA regexps to add to `imenu-generic-expression'.
Any extra regexps should be an alist formatted as `imenu-generic-expression'."
  (when default
    (setq imenu-generic-expression default))
  (when (or headers comment-start)
    (cl-destructuring-bind (h h1 h2)
        (nvp-imenu--create-regex headers headers-1 headers-2)
      (setq nvp-imenu-comment-headers-re h
            nvp-imenu-comment-headers-re-1 h1
            nvp-imenu-comment-headers-re-2 h2)))
  (cl-callf append imenu-generic-expression extra nvp-imenu-comment-headers-re-1))

(put 'nvp-imenu-setup 'lisp-indent-function 'defun)

;; -------------------------------------------------------------------
;;; Commands

;; TODO:
;; - idomenu out-of-date => use `ido-setup-completion-map'
;; - fallback to `nvp-imenu-flattened', \C-x\C-b in `ido-buffer-completion-map'
;;   #<marker at 102574 in ido.el.gz>
;;;###autoload
(defun nvp-imenu-flattened ()
  (interactive)
  (-when-let (lst (or imenu--index-alist (imenu--make-index-alist)))
    (imenu (imenu-choose-buffer-index
            "Imenu: " (--filter (consp it) (nvp-flatten-to-alist lst))))))

;;;###autoload
(defun nvp-imenu-idomenu (arg)
  (interactive "P")
  (let ((default (eq imenu-create-index-function
                     #'imenu-default-create-index-function)))
    (when (and default (null nvp-imenu-comment-headers-re) comment-start)
      (nvp-imenu-setup))
    (with-demoted-errors "Error in nvp-imenu-idomenu: %S"
      (if (and (not (equal ':none nvp-imenu-comment-headers-re))
               (or (not default) imenu-generic-expression))
          (pcase arg
            (`(4)                       ; headers only
             (let ((imenu-generic-expression nvp-imenu-comment-headers-re)
                   (imenu-create-index-function 'imenu-default-create-index-function))
               (ido/imenu)))
            (`(16)                      ; headers + sub-headers only
             (let ((imenu-generic-expression
                    (append nvp-imenu-comment-headers-re
                            nvp-imenu-comment-headers-re-2))
                   (imenu-create-index-function 'imenu-default-create-index-function))
               (ido/imenu)))
            (_ (ido/imenu)))
        (ido/imenu)))))

(provide 'nvp-imenu)
;;; nvp-imenu.el ends here
