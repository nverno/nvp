;;; nvp-imenu.el --- imenu helpers  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; imenu extensions:
;; - add support for mode specific headers
;; - wrapper function to call imenu with additional options
;; - utilities to manipulate index-alist
;; - wrapper for ido-completion w/ fallback to flattened alist
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'imenu)
(nvp-decls :f (nvp-comment-start))
(nvp-auto "nvp-util" 'nvp-flatten-tree)

(defvar nvp-imenu-guess nil
  "If non-nil, suggest active region or `thing-at-point' if it is in the
 imenu alist.")

(defvar nvp-imenu-default-filter-regex (regexp-opt '("Headers" "Sub-Headers"))
  "Regex to match sublist headers to filter out of cleaned imenu alist.")

(defvar nvp-imenu-use-ido nil "Use ido-completion if non-nil.")

(when nvp-imenu-use-ido
  (require 'ido))

;;; Local variables

;; top-level header regexp
(defvar-local nvp-imenu-comment-headers-re nil "Imenu top-level header regexp.")

;; header regexp nested under "Headers"
(defvar-local nvp-imenu-comment-headers-re-1 nil
  "Imenu header regexp nested under 'Headers'")

;; sub-headers
(defvar-local nvp-imenu-comment-headers-re-2 nil "Imenu sub-header regexp.")

;; -------------------------------------------------------------------
;;; Util
;; #<marker at 12739 in which-func.el.gz>

(eval-when-compile
  (defmacro nvp-imenu:if-ido (then &rest else)
    (declare (indent 1) (debug t))
    (if nvp-imenu-use-ido `,@then `(progn ,@else))))

(defsubst nvp-imenu--relative-positions (pos item1 item2)
  "Return non-nil if ITEM1 is closer to POS than ITEM2 in the buffer.
Assumes the list is flattened and only elements with markers remain."
  (< (abs (- pos (cdr item1)))
     (abs (- pos (cdr item2)))))

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
   (nvp-flatten-tree
    (if (and regex (stringp regex))
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

;; substitute spaces for `imenu-space-replacement' in candidate names
(defun nvp-imenu--index-alist ()
  (--map (cons
          (subst-char-in-string ?\s (aref imenu-space-replacement 0) (car it))
          (cdr it))
         (imenu--make-index-alist)))

;; -------------------------------------------------------------------
;;; Hook

(defun nvp-imenu--create-regex (&optional headers headers-1 headers-2)
  (if (eq headers ':none)
      (list :none nil nil)
    (or headers
        (setq headers
              `((nil ,(concat
                       "^" (regexp-quote (nvp-comment-start 3)) "\\{1,2\\}"
                       "\\s-+\\(.*\\)\\s-*$")
                     1))))
    (or headers-1 (setq headers-1 `(("Headers" ,(cadr (car headers)) 1))))
    (or headers-2
        (setq headers-2
              `(("Sub-Headers"
                 ,(concat
                   "^" (nvp-comment-start 2) "-\\{1,2\\}\\s-+\\(.*\\)[ -]*$")
                 1))))
    (list headers headers-1 headers-2)))

;; make header from comment
;;;###autoload
(cl-defun nvp-imenu-setup (&key headers headers-1 headers-2 extra default)
  "Sets up imenu regexps including those to recognize HEADERS and any
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
  (setq imenu-generic-expression
        (append imenu-generic-expression extra nvp-imenu-comment-headers-re-1)))

(put 'nvp-imenu-setup 'lisp-indent-function 'defun)


;; -------------------------------------------------------------------
;;; Completion - ido or (nvp-)completing-read

(defvar nvp-imenu--flattened nil "Flag if alist is flattened.")
(defvar nvp-imenu--exit nil "Flag to monitor completing read exit.")

;; like `ido-fallback-command', exit minibuffer, set flag, push what has
;; been entered back onto the stack so it is used in next command
(defun nvp-imenu-toggle ()
  "Toggle between hierarchical and flattened imenu list."
  (interactive)
  (nvp:unread (minibuffer-contents-no-properties))
  (setq nvp-imenu--exit 'toggle)
  (exit-minibuffer))

;;--- completion map
;; XXX: and add binding to move backward up imenu-alist (DEL)
(defvar nvp-imenu-completion-map
  (let ((map (make-sparse-keymap)))
    (nvp-imenu:if-ido
        (progn
          (define-key map (kbd "C-f") #'ido-fallback-command)
          (define-key map (kbd "C-<return>") #'ido-fallback-command)
          (set-keymap-parent map ido-common-completion-map))
      (define-key map (kbd "C-f") #'nvp-imenu-toggle))
    map))

(define-minor-mode nvp-imenu-completion-mode
  "Imenu completion."
  :keymap nvp-imenu-completion-map)

(eval-when-compile
  (defsubst nvp-imenu--prompt (&optional name)
    (format "Imenu%s%s: " (if nvp-imenu--flattened "[flat]" "")
            (if name (concat " ('" name "')") ""))))

;; Completing read from imenu INDEX-ALIST with optional default NAME
(defun nvp-imenu--read-choice (index-alist &optional name)
  (when (stringp name)
    (setq name (and (imenu--in-alist
                     (or (imenu-find-default name index-alist) name)
                     index-alist)
                    name)))
  (let ((prompt (nvp-imenu--prompt name)))
    (nvp-imenu:if-ido
        (nvp-with-letf 'ido-setup-completion-map
            #'(lambda () (setq ido-completion-map nvp-imenu-completion-map))
          (ido-completing-read
           prompt (mapcar #'car index-alist)
           nil t nil 'imenu--history-list name))
      (minibuffer-with-setup-hook
          (lambda () (nvp-imenu-completion-mode))
        (nvp-completing-read
         prompt index-alist nil t nil 'imenu--history-list name)))))

;; completing read for selection in INDEX-ALIST
(defun nvp-menu--read (index-alist)
  (let* ((default (and nvp-imenu-guess (nvp-tap 'dwim)))
         (name (nvp-imenu--read-choice index-alist default))
         choice)
    (if (nvp-imenu:if-ido (eq ido-exit 'fallback)
          (prog1 (eq nvp-imenu--exit 'toggle)
            (setq nvp-imenu--exit nil)))
        ;; call in minibuf's calling buffer
        (nvp@do-switch-buffer #'nvp-imenu-wrapper nil index-alist t)
      (setq choice (assoc name index-alist))
      (if (imenu--subalist-p choice)
	  (nvp-menu--read (cdr choice))
	choice))))

;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-imenu-wrapper (&optional flat alist toggle)
  "Call imenu.  If FLAT is non-nil, flatten index alist before prompting.
Input is read with `nvp-imenu-completion-mode' active."
  (interactive (list current-prefix-arg (nvp-imenu--index-alist)))
  (nvp-imenu:if-ido (ido-common-initialization))
  (-when-let (alist (or alist (nvp-imenu--index-alist)))
    (when (or flat toggle)
      (setq nvp-imenu--flattened (or flat (not nvp-imenu--flattened)))
      (setq alist (if nvp-imenu--flattened
                      (--filter (consp it) (nvp-flatten-tree alist 'alist))
                    (nvp-imenu--index-alist))))
    (imenu (nvp-menu--read alist))))

;;;###autoload
(defun nvp-imenu (arg)
  "Calls `nvp-imenu-wrapper'. With prefix C-u restrict to headers only.
C-u C-u restricts to headers+sub-headers."
  (interactive "P")
  (setq nvp-imenu--flattened nil)
  (let ((default (eq imenu-create-index-function
                     #'imenu-default-create-index-function)))
    (when (and default (null nvp-imenu-comment-headers-re) comment-start)
      (nvp-imenu-setup))
    (with-demoted-errors "Error in nvp-imenu: %S"
      (if (and (not (equal ':none nvp-imenu-comment-headers-re))
               (or (not default) imenu-generic-expression))
          (pcase arg
            (`(4)                       ; headers only
             (let ((imenu-generic-expression nvp-imenu-comment-headers-re)
                   (imenu-create-index-function 'imenu-default-create-index-function))
               (nvp-imenu-wrapper)))
            (`(16)                      ; headers + sub-headers only
             (let ((imenu-generic-expression
                    (append nvp-imenu-comment-headers-re
                            nvp-imenu-comment-headers-re-2))
                   (imenu-create-index-function 'imenu-default-create-index-function))
               (nvp-imenu-wrapper)))
            (_ (nvp-imenu-wrapper)))
        (nvp-imenu-wrapper)))))

(provide 'nvp-imenu)
;;; nvp-imenu.el ends here
