;;; nvp-imenu.el --- imenu helpers  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; imenu extensions:
;; - add support for mode specific headers
;; - add buffer restiction widening:
;;   `nvp-imenu-toggle' toggles imenu candidates search across buffers:
;;     + current only
;;     + visible only
;;     + all (subject to `imenu-anywhere-buffer-filter-functions')
;; - adds filter to hide current (imenu calling) buffer prefix in completion
;; - utilities to manipulate index-alist
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'imenu-anywhere)
(require 'imenu)
(require 'vertico)
(nvp:decls :f (nvp-comment-start))

(defvar nvp-imenu-guess nil
  "If non-nil, suggest active region or `thing-at-point' if it is in the
 imenu alist.")

(defvar nvp-imenu-default-filter-regex (regexp-opt '("Headers" "Sub-Headers"))
  "Regex to match sublist headers to filter out of cleaned imenu alist.")

(defvar nvp-imenu-use-ido nil "Use ido-completion if non-nil.")

(defvar nvp-imenu-buffer-delimiter ": "
  "Separates buffer name from candidates in other buffers.")

(defvar nvp-imenu-ignored-modes '(dired-mode elisp-byte-code-mode)
  "Major modes to ignore when searching for imenu candidates in other buffers.")

;; imenu-anywhere functions
(setq-default imenu-anywhere-buffer-list-function #'nvp-imenu-buffer-list)
(setq-default imenu-anywhere-preprocess-entry-function
              #'nvp-imenu-preprocess-entry)

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
   (nvp:flatten-tree
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
;;; Headers

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
;;; Anywhere

;;-- visibility
(defvar-local nvp-imenu--visibility 'visible
  "Buffer restiction level: \\='current, \\='visible, \\='all.")

(eval-when-compile
  (defsubst nvp:imenu-cycle-restriction (cur)
    (pcase cur
      (`visible 'current)
      (`current 'all)
      (_ 'visible)))

  (defmacro nvp:imenu-active-buffer ()
    `(if (minibufferp) minibuffer--original-buffer (current-buffer)))

  (defsubst nvp:imenu-buffer-name (buff)
    (concat (buffer-name buff) nvp-imenu-buffer-delimiter))

  (defsubst nvp:imenu-good-buffer-p (buff)
    (let ((cur-buf (nvp:imenu-active-buffer)))
      (or (eq buff cur-buf)
          (not (memq (buffer-local-value 'major-mode buff)
                     nvp-imenu-ignored-modes))))))

;;-- buffer lists
;; candidate for `imenu-anywhere-buffer-list-function'
(defun nvp-imenu-visible-buffer-list ()
  "Buffer list restricted to visible buffers in current frame."
  (nvp:visible-buffers :test-fn #'nvp:imenu-good-buffer-p))

(defun nvp-imenu-buffer-list (&optional restrict)
  "List of potential buffers to check for imenu candidates."
  (pcase (or restrict nvp-imenu--visibility)
    (`current (list (nvp:imenu-active-buffer)))
    (`visible (nvp-imenu-visible-buffer-list))
    (`all (--filter (nvp:imenu-good-buffer-p it) (buffer-list)))
    (_ (nvp-imenu-visible-buffer-list))))

;;-- preprocess completion candidates
;; candidate for `imenu-anywhere-preprocess-entry-function'
;; dont prefix candidates in the current buffer
(defun nvp-imenu-preprocess-entry (entry entry-name)
  (when entry
    (let ((bname (if (markerp (cdr entry))
                     (nvp:imenu-buffer-name (marker-buffer (cdr entry)))
                   "")))
      (setcar entry (concat bname entry-name
                            (and entry-name imenu-anywhere-delimiter)
                            (car entry))))
    entry))

;; hide active buffer in completion candidates
(defvar-local nvp-imenu--active-buffer-re nil)
(defun nvp@imenu-hide-active (orig-fn str)
  (if (and nvp-imenu--active-buffer-re
           (string-match nvp-imenu--active-buffer-re str))
      (let ((s (copy-sequence str)))
        (add-text-properties (match-beginning 0) (match-end 0) '(invisible t) s)
        (funcall orig-fn s))
    (funcall orig-fn str)))

;; bulk preprocessing - modifies in-place 
;; (defun nvp-imenu-preprocess-candidates (candidates)
;;   (let* ((cur (buffer-name (nvp:imenu-active-buffer)))
;;          (pre (concat cur nvp-imenu-buffer-delimiter "\\s-*")))
;;     (cl-loop for (cand . marker) in candidates
;;              when (string-match pre cand)
;;              do (add-text-properties (match-beginning 0) (match-end 0) '(invisible t) cand))
;;     candidates))

;; (defun nvp-imenu-candidates ()
;;   (nvp-imenu-preprocess-candidates
;;    (imenu-anywhere-candidates)))

;; -------------------------------------------------------------------
;;; Completion - ido or (nvp-)completing-read

;; let-bind imenu candidates during fallback
(defvar nvp-imenu--completion-table nil)

;; Fallback toggles b/w visible and unrestricted buffers
(defun nvp-imenu-toggle ()
  "Toggle between unrestricted/visible only buffer search."
  (interactive)
  (let ((_input (minibuffer-contents-no-properties)))
    (setq nvp-imenu--visibility
          (nvp:imenu-cycle-restriction nvp-imenu--visibility))
    (let ((imenu-anywhere-buffer-list-function
           (apply-partially #'nvp-imenu-buffer-list nvp-imenu--visibility)))
      (setq minibuffer-completion-table
            ;; call in original buffer so
            ;; `imenu-anywhere-buffer-filter-functions' use mode/project, etc.
            (with-minibuffer-selected-window
              (imenu-anywhere-candidates))
            ;; let-bind around completing-read - minibuffer-completion-table
            ;; will be nil after completing-read calls `exit-minibuffer'
            nvp-imenu--completion-table minibuffer-completion-table))
    (let ((vertico--input t))
      (vertico--exhibit))))

;;--- completion map
;; XXX: and add binding to move backward up imenu-alist (DEL)
;; TODO: ':' should restrict to buffer, '/' restrict to sublist
(defvar nvp-imenu-completion-map
  (let ((map (make-sparse-keymap)))
    (nvp-imenu:if-ido
        (progn
          (define-key map (kbd "C-f") #'ido-fallback-command)
          (define-key map (kbd "C-<return>") #'ido-fallback-command)
          (set-keymap-parent map ido-common-completion-map))
      (define-key map (kbd "C-f") #'nvp-imenu-toggle))
    map))

;; cleanup on exit from minibuffer
(defun nvp-imenu-completion-exit ()
  (advice-remove 'vertico--display-string #'nvp@imenu-hide-active)
  (remove-hook 'minibuffer-exit-hook #'nvp-imenu-completion-exit))

(define-minor-mode nvp-imenu-completion-mode
  "Imenu completion."
  :keymap nvp-imenu-completion-map
  (if nvp-imenu-completion-mode
      (progn
        (advice-add 'vertico--display-string :around #'nvp@imenu-hide-active)
        (add-hook 'minibuffer-exit-hook #'nvp-imenu-completion-exit))
    (advice-remove 'vertico--display-string #'nvp@imenu-hide-active)))

(eval-when-compile
  ;; TODO: display restriction as overlay after prompt
  (defsubst nvp:imenu-prompt ()
    "Imenu: "
    ;; (if nvp-imenu--visibility "Imenu[v]: " "Imenu[all]: ")
    ))

(defun nvp-imenu-complete (&optional restrict)
  (nvp-imenu:if-ido (ido-common-initialization))
  (let* ((imenu-default-goto-function 'imenu-anywhere-goto)
         (nvp-imenu--visibility (or restrict nvp-imenu--visibility))
         (index-alist (imenu-anywhere-candidates)))
    (if (null index-alist) (message "No imenu tags")
      (let* ((str (and nvp-imenu-guess (nvp:tap 'dwim)))
             (default
              (and str (imenu-anywhere--guess-default index-alist str))))
        (nvp-imenu:if-ido
            (nvp:with-letf 'ido-setup-completion-map
                #'(lambda () (setq ido-completion-map nvp-imenu-completion-map))
              (ido-completing-read
               (nvp:imenu-prompt) (mapcar #'car index-alist)
               nil t nil 'imenu--history-list default))
          (minibuffer-with-setup-hook (lambda () (nvp-imenu-completion-mode))
            (let* ((nvp-imenu--completion-table)
                   (nvp-imenu--active-buffer-re
                    (concat (buffer-name (nvp:imenu-active-buffer))
                            nvp-imenu-buffer-delimiter))
                   (name (nvp-completing-read (nvp:imenu-prompt)
                           index-alist nil t nil 'imenu--history-list default))
                   (selection (or (assoc name index-alist)
                                  ;; if restriction widened during completing-read
                                  ;; `index-alist' won't have candidate
                                  (assoc name nvp-imenu--completion-table))))
              (xref-push-marker-stack)
              (imenu selection))))))))

;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-imenu (&optional arg)
  "Call `imenu-anywhere' with fallback restricting to visible buffers only.
\\[universal-argument] - restrict to only headers
\\[universal-argument] \\[universal-argument] - only headers+sub-headers."
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (let ((default-p (eq imenu-create-index-function
                       #'imenu-default-create-index-function)))
    (when (and default-p (null nvp-imenu-comment-headers-re) comment-start)
      (nvp-imenu-setup))
    (with-demoted-errors "Error in nvp-imenu: %S"
      (if (or (< arg 4)
              (equal ':none nvp-imenu-comment-headers-re)
              (not (or default-p imenu-generic-expression)))
          (nvp-imenu-complete)
        (let ((imenu-generic-expression
               (if (< arg 16) nvp-imenu-comment-headers-re
                 (append nvp-imenu-comment-headers-re
                         nvp-imenu-comment-headers-re-2)))
              (imenu-create-index-function #'imenu-default-create-index-function))
          (nvp-imenu-complete 'current))))))

(provide 'nvp-imenu)
;;; nvp-imenu.el ends here
