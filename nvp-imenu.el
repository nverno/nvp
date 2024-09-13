;;; nvp-imenu.el --- imenu helpers  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; imenu extensions:
;; - add support for mode specific headers
;; - add buffer restiction widening:
;;   `nvp-imenu-cycle-restriction' toggles imenu candidates search across buffers:
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

(defvar nvp-imenu-default-filter-regex (rx (or "Headers" "Sub-Headers"))
  "Regex to match sublist headers to filter out of cleaned imenu alist.")

(defvar nvp-imenu-buffer-delimiter ": "
  "Separates buffer name from candidates in other buffers.")

(defvar nvp-imenu-ignored-modes
  '( dired-mode elisp-byte-code-mode ag-mode comint-mode
     help-mode minibuffer-inactive-mode)
  "Major modes to ignore when searching for imenu candidates in other buffers.")

;; imenu-anywhere functions
(setq-default imenu-anywhere-buffer-list-function #'nvp-imenu-buffer-list)
(setq-default imenu-anywhere-preprocess-entry-function
              #'nvp-imenu-preprocess-entry)


;; #<marker at 12739 in which-func.el.gz>
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

(defun nvp-imenu-cleaned-alist (&optional buffer regex alist)
  "Flatten imenu alist, remove headers and things that don't look like code."
  (or buffer (setq buffer (current-buffer)))
  (or regex (setq regex nvp-imenu-default-filter-regex))
  (or alist (setq alist imenu--index-alist))
  (delq nil (--map (nvp-imenu-filter-code buffer it)
                   (nvp:flatten-to-alist
                    (if (and regex (stringp regex))
                        (nvp-imenu-filter-regex regex alist)
                      alist)))))

(defsubst nvp-imenu--remove-prefix (elem)
  "Remove filename prefix added from ELEM."
  (thread-last
    elem
    (replace-regexp-in-string (concat "^.*" nvp-imenu-buffer-delimiter) "")
    (replace-regexp-in-string "^.*/" "")))

(defun nvp-imenu-filter-code (buffer elem)
  "Filter out probable non code things."
  (when (and (consp elem) (not (nvp:dotted-pair-p elem)))
    (setq elem (car elem)))
  (let (mark)
    (--when-let
        (and (consp elem)
             (setq mark (cdr elem))
             (cond
              ((numberp mark) mark)
              ((markerp mark)
               (and (or (null buffer) (eq (marker-buffer mark) buffer))
                    mark))
              ((overlayp mark)
               (and (or (null buffer) (eq (overlay-buffer mark) buffer))
                    (overlay-start mark)))
              (t nil)))
      (cons (nvp-imenu--remove-prefix (car elem)) mark))))

(defun nvp-imenu--index-alist ()
  "Substitute spaces for `imenu-space-replacement' in candidate names."
  (--map (cons
          (subst-char-in-string ?\s (aref imenu-space-replacement 0) (car it))
          (cdr it))
         (imenu--make-index-alist)))


;;; Headers
(defvar-local nvp-imenu-comment-headers-re nil "Imenu top-level header regexp.")

(defvar-local nvp-imenu-comment-headers-re-1 nil
  "Imenu header regexp nested under \"Headers\"")

(defvar-local nvp-imenu-comment-headers-re-2 nil "Imenu sub-header regexp.")

(defun nvp-imenu--create-header-regex (&optional headers headers-1 headers-2)
  "Create header regex for imenu."
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
                   "^" (nvp-comment-start 2)
                   "[-*]\\{1,2\\}\\s-+\\(.*\\)[ *-]*$")
                 1))))
    (list headers headers-1 headers-2)))

;;;###autoload
(defun nvp-imenu-setup (&optional config default)
  "Set imenu regexps to recognize headers from CONFIG.
Extra regexps in CONFIG are added to `imenu-generic-expression' and should
should formatted as an alist like `imenu-generic-expression'."
  (and default (setq imenu-generic-expression default))
  (cl-destructuring-bind (&key headers headers-1 headers-2 extra) config
    (when (or headers comment-start)
      (cl-destructuring-bind (h h1 h2)
          (nvp-imenu--create-header-regex headers headers-1 headers-2)
        (setq nvp-imenu-comment-headers-re h
              nvp-imenu-comment-headers-re-1 h1
              nvp-imenu-comment-headers-re-2 h2)))
    (setq imenu-generic-expression
          (append imenu-generic-expression extra nvp-imenu-comment-headers-re-1))))

(put 'nvp-imenu-setup 'lisp-indent-function 'defun)


;; -------------------------------------------------------------------
;;; Anywhere

;;-- visibility
(defvar nvp-imenu-restrictions '(current visibile all)
  "Restrict matches to buffers: \\='current, \\='visible, \\='all.")

(defvar-local nvp-imenu--idx 0)

(defsubst nvp-imenu--visibility ()
  (nth nvp-imenu--idx nvp-imenu-restrictions))

(defsubst nvp-imenu--next ()
  (setq nvp-imenu--idx (mod (1+ nvp-imenu--idx) 3))
  (nvp-imenu--visibility))

(defsubst nvp-imenu--active-buffer ()
  (if (minibufferp) minibuffer--original-buffer (current-buffer)))

(defsubst nvp-imenu--buffer-name (buff)
  (concat (buffer-name buff) nvp-imenu-buffer-delimiter))

(defun nvp-imenu--good-buffer-p (buff)
  "Return non-nil if BUFF should be considered by imenu."
  (and (not (string-prefix-p " " (buffer-name buff)))
       (let ((major-mode (buffer-local-value 'major-mode buff)))
         (and (not (derived-mode-p nvp-imenu-ignored-modes))
              (buffer-local-value 'imenu-generic-expression buff)))))

;;; Buffer Lists
;; candidate for `imenu-anywhere-buffer-list-function'
(defun nvp-imenu-visible-buffer-list ()
  "Buffer list restricted to visible buffers in current frame."
  (seq-uniq
   (cons (nvp-imenu--active-buffer)
         (--map (window-buffer it)
                (nvp:visible-windows :test-fn #'nvp-imenu--good-buffer-p)))))

(defun nvp-imenu-buffer-list (&optional restrict)
  "List of potential buffers to check for imenu candidates."
  (pcase (or restrict (nvp-imenu--visibility))
    (`current (list (nvp-imenu--active-buffer)))
    (`visible (nvp-imenu-visible-buffer-list))
    (`all (--filter (nvp-imenu--good-buffer-p it) (buffer-list)))
    (_ (nvp-imenu-visible-buffer-list))))

;;; Preprocess completion candidates
;; Note: doesnt prefix candidates in the current buffer
(defun nvp-imenu-preprocess-entry (entry entry-name)
  "Function for `imenu-anywhere-preprocess-entry-function', which see."
  (when entry
    (let ((bname (if (markerp (cdr entry))
                     (nvp-imenu--buffer-name (marker-buffer (cdr entry)))
                   "")))
      (setcar entry (concat bname entry-name
                            (and entry-name imenu-anywhere-delimiter)
                            (car entry))))
    entry))

;; bulk preprocessing - modifies in-place
;; (defun nvp-imenu-preprocess-candidates (candidates)
;;   (let* ((cur (buffer-name (nvp-imenu--active-buffer)))
;;          (pre (concat cur nvp-imenu-buffer-delimiter "\\s-*")))
;;     (cl-loop for (cand . marker) in candidates
;;              when (string-match pre cand)
;;              do (add-text-properties (match-beginning 0) (match-end 0) '(invisible t) cand))
;;     candidates))

;; (defun nvp-imenu-candidates ()
;;   (nvp-imenu-preprocess-candidates
;;    (imenu-anywhere-candidates)))


;; -------------------------------------------------------------------
;;; Completion
;; Note: Let-bind imenu candidates during fallback

(defvar nvp-imenu--completion-table nil)

(defvar-local nvp-imenu--filter-re nil
  "Local regex to filter imenu candidates.")

(defun nvp-imenu--candidates (&optional regex)
  "Get imenu candidates, filtering with REGEX when non-nil."
  (if-let ((re (or regex nvp-imenu--filter-re)))
      (--filter (string-match-p re (car it))
                (imenu-anywhere-candidates))
    (imenu-anywhere-candidates)))

(defvar-local nvp-imenu--active-buffer-re nil)

(defun nvp@imenu-hide-active (orig-fn str)
  "Hide active buffer in completion candidates."
  (if (and nvp-imenu--active-buffer-re
           (string-match nvp-imenu--active-buffer-re str))
      (let ((s (copy-sequence str)))
        (add-text-properties
         (match-beginning 0) (match-end 0) '(invisible t) s)
        (funcall orig-fn s))
    (funcall orig-fn str)))

;;; Mode Line
(defvar-local nvp-imenu--active-mode nil "Non-nil during completion.")

(defvar nvp-imenu-mode-line
  `(" Imenu:"
    (:eval (format (:propertize "%s" 'face 'font-lock-warning-face)
                   (substring (symbol-name (nvp-imenu--visibility)) 0 3)))))

(or (assq 'nvp-imenu--active-mode minor-mode-alist)
    (nconc minor-mode-alist
           (list `(nvp-imenu--active-mode ,nvp-imenu-mode-line))))

(defun nvp-imenu--active-mode (&optional arg idx)
  (or idx (setq idx nvp-imenu--idx))
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (setq nvp-imenu--active-mode (or arg (not nvp-imenu--active-mode)))
    (setq nvp-imenu--idx idx)
    (force-mode-line-update)))

(defun nvp-imenu-cycle-restriction ()
  "Cycle current buffer restriction during completion."
  (interactive)
  (nvp:vertico-update-candidates nil
    ;; Call in original buffer so
    ;; `imenu-anywhere-buffer-filter-functions' use mode/project, etc.
    (with-minibuffer-selected-window
      (let ((imenu-anywhere-buffer-list-function
             (apply-partially #'nvp-imenu-buffer-list (nvp-imenu--next))))
        (nvp-imenu--active-mode t)
        (nvp-imenu--candidates))))
  ;; Let-bind around completing-read - minibuffer-completion-table
  ;; will be nil after completing-read calls `exit-minibuffer'
  (setq nvp-imenu--completion-table minibuffer-completion-table))

(defun nvp-imenu-completion-exit ()
  "Cleanup on exit from minibuffer."
  (advice-remove 'vertico--display-string #'nvp@imenu-hide-active)
  (remove-hook 'minibuffer-exit-hook #'nvp-imenu-completion-exit)
  (nvp-imenu--active-mode nil))

;; XXX: Add binding to move backward up imenu-alist (DEL)
;; TODO: ':' should restrict to buffer, '/' restrict to sublist
(defvar-keymap nvp-imenu-completion-mode-map
  :doc "Keymap active during imenu completion."
  "C-o" #'nvp-imenu-cycle-restriction)

(define-minor-mode nvp-imenu-completion-mode
  "Minor mode active in minibuffer during imenu completion."
  :keymap nvp-imenu-completion-mode-map
  :lighter nil
  :interactive nil
  (if (null nvp-imenu-completion-mode)
      (advice-remove 'vertico--display-string #'nvp@imenu-hide-active)
    (advice-add 'vertico--display-string :around #'nvp@imenu-hide-active)
    (add-hook 'minibuffer-exit-hook #'nvp-imenu-completion-exit)
    (nvp-imenu--active-mode t)))

;; TODO: display restriction as overlay after prompt
(defun nvp-imenu--prompt ()
  (format "Imenu: ")) ;; (nvp-imenu--visibility)

(defun nvp-imenu-complete (&optional restrict headers-only)
  "Completing read for imenu candidate with `nvp-imenu-completion-mode' active.
Calls `imenu' to jump to location with selection."
  (and restrict (setq nvp-imenu--idx restrict))
  (let* ((imenu-default-goto-function 'imenu-anywhere-goto)
         (nvp-imenu--filter-re (and headers-only "Headers"))
         (index-alist (nvp-imenu--candidates)))
    (if (null index-alist)
        (let ((message-log-max nil))
          (message "No imenu tags"))
      (let* ((str (and nvp-imenu-guess (nvp:tap 'dwim)))
             (default
              (and str (imenu-anywhere--guess-default index-alist str))))
        (minibuffer-with-setup-hook (lambda () (nvp-imenu-completion-mode))
          (let* ((nvp-imenu--completion-table)
                 (nvp-imenu--active-buffer-re
                  (concat (regexp-quote (buffer-name (nvp-imenu--active-buffer)))
                          nvp-imenu-buffer-delimiter))
                 (name (nvp-completing-read (nvp-imenu--prompt)
                         index-alist nil t nil 'imenu--history-list default))
                 (selection (or (assoc name index-alist)
                                ;; if restriction widened during completing-read
                                ;; `index-alist' won't have candidate
                                (assoc name nvp-imenu--completion-table))))
            (xref-push-marker-stack)
            (imenu selection)))))))


;; -------------------------------------------------------------------
;;; Commands

;;; FIXME(09/06/24): 1. index not always updating after different prefixes
;;                   2. rewrite to use consult with narrowing keys
;;                   3. add narrowing for headers/sub-headers
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
          (nvp-imenu-complete 0 'headers))))))

(provide 'nvp-imenu)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-imenu.el ends here
